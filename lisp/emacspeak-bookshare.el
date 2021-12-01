;;; emacspeak-bookshare.el --- Speech-enabled  BOOKSHARE client  -*- lexical-binding: t; -*-
;;; $Id: emacspeak-bookshare.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable BOOKSHARE An Emacs Interface to bookshare
;;; Keywords: Emacspeak,  Audio Desktop bookshare
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2021, T. V. Raman
;;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
;;; All Rights Reserved.
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNBOOKSHARE FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; BOOKSHARE == http://www.bookshare.org
;;; provides book access to print-disabled users.
;;; It provides a simple Web  API http://developer.bookshare.org
;;; This module implements an Emacspeak Bookshare client.
;;; @subsection  requirements
;;; @itemize
;;; @item You need to get your own API key
;;; @item You need Emacs built with libxml2 support
;;; @end itemize
;;;
;;; @subsection Usage
;;; The main entry point is command @code{emacspeak-bookshare} bound to @kbd{C-e C-b}.
;;; This creates a special @emph{Bookshare Interaction} buffer that is
;;; placed in @emph{emacspeak-bookshare-mode}.
;;; Se the help for that mode on detailed usage instructions and key-bindings.
;;;
;;;@subsection Sample Interaction
;;;
;;; Assuming you have correctly setup your API key:
;;; @itemize
;;;@item Customize group @code{emacspeak-bookshare} by pressing @kbd{C-h G}.
;;;@item  Press @kbd{C-e C-b} to open or switch to the Bookshare buffer.
;;; @item Perform a search @kbd{a} or @kbd{t} for author or title search.
;;; @item You will be prompted for your Bookshare password if this is
;;; the first time.
;;; @item The password will be saved to your configured
;;; @code{auth-source} --- usually @code{~/.authinfo.gpg}.
;;; You can also use @code{password-store[.]}
;;; @item The results of the search appear in the Bookshare buffer.
;;; Audio formatting and auditory icons convey if  a result is already available locally.
;;; @item If not available locally, press @kbd{D} to download the content.
;;; @item Press @kbd{U} to unpack the downloaded content.
;;;@item Press @kbd{e} to  display the entire book.
;;;@item Press @kbd{c} to display the table of contents.
;;; @item Now, use all of EWW  @xref{emacspeak-eww} extensions  and profit!
;;;@end itemize
;;; Code:
;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(eval-when-compile (require 'derived)
                   (require 'g-utils))
(require 'dom)
(require 'xml)
(declare-function auth-source-search "auth-source" (&rest rest))
(declare-function dired-get-filename "dired" (&optional localp
                                                        no-error-if-not-filep))
(declare-function emacspeak-xslt-get "emacspeak-xslt" (arg1))
(declare-function emacspeak-xslt-params-from-xpath "emacspeak-bookshare" t)
;;}}}
;;{{{ Customizations

(defgroup emacspeak-bookshare nil
  "Bookshare Access on the Complete Audio Desktop."
  :group 'emacspeak)

(defcustom emacspeak-bookshare-api-key nil
  "Web API  key for this application.
See http://developer.bookshare.org/docs for details on how to get
  an API key. "
  :type
  '(choice :tag "Key"
           (const :tag "Unspecified" nil)
           (string :tag "API Key"))
  :group 'emacspeak-bookshare)

(defvar emacspeak-bookshare-user-id nil
  "Bookshare user Id.")

(defcustom emacspeak-bookshare-directory
  (eval-when-compile (expand-file-name "~/books/book-share"))
  "Customize this to the root of where books are organized."
  :type 'directory
  :group 'emacspeak-bookshare)

(defvar emacspeak-bookshare-downloads-directory
  (expand-file-name "downloads/" emacspeak-bookshare-directory)
  "Directory where archives are saved on download.")

(defvar emacspeak-bookshare-browser-function
  'eww-browse-url
  "Function to display Bookshare Book content in a WWW browser.
This is used by the various Bookshare view commands to display
  content from Bookshare books.")

;;}}}
;;{{{ Variables:

(defvar emacspeak-bookshare-api-base
  "https://api.bookshare.org"
  "Base end-point for Bookshare API  access.")

;;}}}
;;{{{ Helpers:

(defun emacspeak-bookshare-dom-clean-text (dom tag)
  "Extract text from specified tag, and clean up entity references."
  (xml-substitute-special
   (xml-substitute-numeric-entities
    (dom-text (dom-by-tag dom tag)))))

(defun emacspeak-bookshare-assert ()
  "Error out if not in Bookshare mode."
  (unless (eq major-mode 'emacspeak-bookshare-mode)
    (error "Not in Bookshare Interaction.")))

(defvar emacspeak-bookshare-md5-cached-token nil
  "Cache MD5 token for future use.")

(defun emacspeak-bookshare-user-password ()
  "User password.
Get user and secret from auth-sources, and memoize the user and
the MD5-encoded secret."
  (cl-declare (special emacspeak-bookshare-user-id
                       emacspeak-bookshare-md5-cached-token))
  (unless emacspeak-bookshare-md5-cached-token
    (let ((auth-info (emacspeak-bookshare-get-auth-info)))
      (setq emacspeak-bookshare-user-id (car auth-info))
      (setq emacspeak-bookshare-md5-cached-token (md5 (cdr auth-info)))))
  (format "-H 'X-password: %s'" emacspeak-bookshare-md5-cached-token))

(defun emacspeak-bookshare-get-auth-info()
  "Get the email and password for BookShare if it already exists
in `auth-sources'. If not present, ask for email and password,
and create an entry in the `auth-sources'.

Returns a cons cell where the car is email, and the cdr is password."
  (let* ((auth-source-creation-prompts
          '((user . "Your BookShare.org e-mail: ")
            (secret . "Your BookShare.org password: ")))
         (found
          (nth 0
               (auth-source-search
                :max 1
                :host "api.bookshare.org"
                :port 'https
                :create t
                :require '(:user :secret)))))
    (when found
      (let ((user (plist-get found :user))
            (secret (plist-get found :secret))
            (save-function (plist-get found :save-function)))
        (while (functionp secret) (setq secret (funcall secret)))
        (when (functionp save-function) (funcall save-function))
        (cons user secret)))))

(defun emacspeak-bookshare-rest-endpoint (operation operand &optional noauth)
  "Return  URL  end point for specified operation.
Optional argument `noauth' says no user auth needed."
  (cl-assert emacspeak-bookshare-api-key nil "API key not set.")
  (unless (or  noauth  emacspeak-bookshare-user-id)
    ;;  initialize user-id
    (emacspeak-bookshare-user-password))
  (url-encode-url
   (format "%s/%s/%s/%s?api_key=%s"
           emacspeak-bookshare-api-base operation operand
           (if noauth "" (format "for/%s" emacspeak-bookshare-user-id))
           emacspeak-bookshare-api-key)))

(defun emacspeak-bookshare-page-rest-endpoint ()
  "Generate REST endpoint for the next page of results."
  (cl-declare (special emacspeak-bookshare-last-action-uri))
  (unless emacspeak-bookshare-last-action-uri
    (error "No query to  page!"))
  (let ((root
         (cl-first (split-string emacspeak-bookshare-last-action-uri "/for")))
        (page nil))
    (setq page (string-match "/page/" root))
    (cond
     (page
      (setq page (split-string root "/page/"));Already paged once
      (format "%s/page/%s/for/%s?api_key=%s"
              (cl-first page)
              (1+ (read (cl-second page)))
              emacspeak-bookshare-user-id
              emacspeak-bookshare-api-key))
     (t
      (format "%s/page/2/for/%s?api_key=%s"
              root
              emacspeak-bookshare-user-id
              emacspeak-bookshare-api-key)))))

(defun emacspeak-bookshare-destruct-rest-url (url)
  "Return operator and operand used to construct this REST end-point."
  (cl-declare (special emacspeak-bookshare-api-base))
  (let* ((start (length emacspeak-bookshare-api-base))
         (end (string-match "for/" url)))
    (nthcdr 2
            (split-string
             (substring url start end) "/" 'no-null))))

(defun emacspeak-bookshare-download-url (id fmt)
  "Return  URL  end point for content download.
Argument id specifies content. Argument fmt = 0 for Braille, 1
   for Daisy, 3 for epub-3,6 for audio."
  (cl-declare (special emacspeak-bookshare-api-base emacspeak-bookshare-user-id))
  (format "%s/%s/%s?api_key=%s"
          emacspeak-bookshare-api-base
          (format "download/content/%s/version/%s" id fmt)
          (format "for/%s" emacspeak-bookshare-user-id)
          emacspeak-bookshare-api-key))

(defun emacspeak-bookshare-get-result (command)
  "Run command and return its output."
  (cl-declare (special shell-file-name shell-command-switch))
  (g-using-scratch
   (call-process shell-file-name nil t
                 nil shell-command-switch
                 command)
   (goto-char (point-min))
   (libxml-parse-xml-region (point-min) (point-max))))

(defvar emacspeak-bookshare-last-action-uri nil
  "Cache last API call URI.")
(defvar emacspeak-bookshare-curl-common-options
  " --insecure --location "
  "Common Curl options for Bookshare. Includes --insecure as per
Bookshare docs.")

(defun emacspeak-bookshare-api-call (operation operand &optional no-auth)
  "Make a Bookshare API  call and get the result.
Optional argument 'no-auth says we dont need a user auth."
  (cl-declare (special emacspeak-bookshare-last-action-uri))
  (setq emacspeak-bookshare-last-action-uri
        (emacspeak-bookshare-rest-endpoint operation operand no-auth))
  (emacspeak-bookshare-get-result
   (format
    "%s %s %s  %s 2>/dev/null"
    emacspeak-curl-program
    emacspeak-bookshare-curl-common-options
    (if no-auth "" (emacspeak-bookshare-user-password))
    emacspeak-bookshare-last-action-uri)))

(defun emacspeak-bookshare-get-more-results ()
  "Get next page of results for last query."
  (interactive)
  (cl-declare (special emacspeak-bookshare-last-action-uri))
  (setq emacspeak-bookshare-last-action-uri
        (emacspeak-bookshare-page-rest-endpoint))
  (emacspeak-bookshare-get-result
   (format "%s %s %s  %s 2>/dev/null"
           emacspeak-curl-program
           emacspeak-bookshare-curl-common-options
           (emacspeak-bookshare-user-password)
           emacspeak-bookshare-last-action-uri)))

(defun emacspeak-bookshare-generate-target (author title &optional fmt)
  "Generate a suitable filename target."
  (cl-declare (special emacspeak-bookshare-downloads-directory))
  (expand-file-name
   (replace-regexp-in-string
    "[ _&'\":();]+" "-"
    (format "%s-%s%s.zip"
            author title
            (if  fmt
                (format "-%s" fmt) "")))
   emacspeak-bookshare-downloads-directory))

(defun emacspeak-bookshare-generate-directory (author title)
  "Generate name of unpack directory."
  (cl-declare (special emacspeak-bookshare-directory))
  (expand-file-name
   (replace-regexp-in-string
    "[ _&'\":();]+" "-"
    (format "%s/%s" author title))
   emacspeak-bookshare-directory))

(defun emacspeak-bookshare-destruct-target (target)
  "Destruct  a  filename target into components."
  (split-string
   (substring target  0 -4)
   "-" 'no-null))

;;}}}
;;{{{ Book Actions:

(defvar emacspeak-bookshare-categories nil
  "Cached list of categories.")

;;;temporary definition

(defun emacspeak-bookshare-categories ()
  "Return memoized list of categories."
  (cl-declare (special emacspeak-bookshare-categories))
  (or
   emacspeak-bookshare-categories
   (setq
    emacspeak-bookshare-categories
    (let ((result
           (dom-by-tag
            (emacspeak-bookshare-api-call "reference/category/list" "" 'no-auth)
            'result)))
      (cl-loop
       for r in result collect
       (url-encode-url (dom-text (dom-by-tag r  'name))))))))

;;;  Following actions return book metadata:

(defun emacspeak-bookshare-isbn-search (query)
  "Perform a Bookshare isbn search."
  (interactive "sISBN: ")
  (emacspeak-bookshare-api-call "book/isbn" query))

(defun emacspeak-bookshare-id-search (query)
  "Perform a Bookshare id search."
  (interactive "sId: ")
  (emacspeak-bookshare-api-call "book/id" query))

;;; preference  getter/setter:

(defun emacspeak-bookshare-list-preferences ()
  "Return preference list."
  (interactive)
  (emacspeak-bookshare-api-call
   "user" "preferences/list"))

(defun emacspeak-bookshare-set-preference (preference-id value)
  "Set preference preference-id to value."
  (interactive "sPreference Id:\nsValue: ")
  (emacspeak-bookshare-api-call
   "user"
   (format "preference/%s/set/%s"
           preference-id value)))

;;; Following Actions return book-list structures within a bookshare envelope.

(defun emacspeak-bookshare-author-search (query &optional category)
  "Perform a Bookshare author search.
Interactive prefix arg filters search by category."
  (interactive
   (list
    (url-hexify-string
     (read-from-minibuffer "author: "))
    current-prefix-arg))
  (cond
   ((null category)                     ; plain search
    (emacspeak-bookshare-api-call "book/searchFTS/author" query))
   (t                                   ; filter using category:
    (let* ((completion-ignore-case  t)
           (filter
            (completing-read "Category: "
                             (emacspeak-bookshare-categories))))
      (emacspeak-bookshare-api-call
       "book/searchFTS/author"
       (format "%s/category/%s"
               query filter))))))

(defun emacspeak-bookshare-title-search (query &optional category)
  "Perform a Bookshare title search.
Interactive prefix arg filters search by category."
  (interactive
   (list
    (url-hexify-string
     (read-from-minibuffer "Title: "))
    current-prefix-arg))
  (cond
   ((null category)                     ; plain search
    (emacspeak-bookshare-api-call "book/searchFTS/title" query))
   (t                                   ; filter using category:
    (let* ((completion-ignore-case t)
           (filter
            (completing-read "Category: "
                             (emacspeak-bookshare-categories))))
      (emacspeak-bookshare-api-call
       "book/searchFTS/title"
       (format "%s/category/%s"
               query filter))))))

(defun emacspeak-bookshare-title/author-search (query)
  "Perform a Bookshare title/author  search."
  (interactive "sTitle/Author: ")
  (emacspeak-bookshare-api-call "book/searchTA" query))

(defun emacspeak-bookshare-fulltext-search (query)
  "Perform a Bookshare fulltext search."
  (interactive "sFulltext Search: ")
  (emacspeak-bookshare-api-call "book/searchFTS" query))

(defun emacspeak-bookshare-since-search (query &optional category)
  "Perform a Bookshare date  search.
Optional interactive prefix arg filters by category."
  (interactive
   (list
    (read-from-minibuffer "Date:MmDdYyYy")
    current-prefix-arg))
  (cond
   ((null category)                     ; plain search
    (emacspeak-bookshare-api-call "book/search/since" query))
   (t                                   ; filter using category:
    (let* ((completion-ignore-case t)
           (filter
            (completing-read "Category: "
                             (emacspeak-bookshare-categories))))
      (emacspeak-bookshare-api-call
       "book/search/since"
       (format "%s/category/%s"
               query filter))))))

(defun emacspeak-bookshare-browse-latest()
  "Return latest books."
  (interactive)
  (emacspeak-bookshare-api-call "book/browse/latest" ""))

(defun emacspeak-bookshare-browse-popular(&optional category)
  "Browse popular books.
Optional interactive prefix arg prompts for a category to use as a filter."
  (interactive "P")
  (cond
   ((null category)                     ; plain search
    (emacspeak-bookshare-api-call "book/browse/popular" ""))
   (t                                   ; filter using category:
    (let* ((completion-ignore-case t)
           (filter
            (completing-read "Category: "
                             (emacspeak-bookshare-categories))))
      (emacspeak-bookshare-api-call
       "book/browse/popular"
       (format "category/%s" filter))))))

;;}}}
;;{{{ Periodical Actions:

;;; Returns periodical list

(defun emacspeak-bookshare-periodical-list ()
  "Return list of periodicals."
  (interactive)
  (emacspeak-bookshare-api-call
   "periodical" "list"))

;;}}}
;;{{{ Downloading Content:

(defun emacspeak-bookshare-download-internal(url target)
  "Download content  to target location."
  (interactive)
  (shell-command
   (format
    "%s %s %s  '%s' -o \"%s\""
    emacspeak-curl-program
    emacspeak-bookshare-curl-common-options
    (emacspeak-bookshare-user-password)
    url
    target)))

(defun emacspeak-bookshare-download-daisy(id target)
  "Download Daisy format of specified book to target location."
  (interactive)
  (emacspeak-bookshare-download-internal
   (emacspeak-bookshare-download-url id 1)
   target))

(defun emacspeak-bookshare-download-audio(id target)
  "Download audio format of specified book to target location."
  (interactive)
  (emacspeak-bookshare-download-internal
   (emacspeak-bookshare-download-url id 6)
   target))

(defun emacspeak-bookshare-download-epub-3(id target)
  "Download epub-3 format of specified book to target location."
  (interactive)
  (emacspeak-bookshare-download-internal
   (emacspeak-bookshare-download-url id 3)
   target))

(defun emacspeak-bookshare-download-brf(id target)
  "Download Daisy format of specified book to target location."
  (interactive)
  (emacspeak-bookshare-download-internal
   (emacspeak-bookshare-download-url id 0)
   target))

;;}}}
;;{{{ Actions Table:

(defvar emacspeak-bookshare-action-table (make-hash-table :test #'equal)
  "Table mapping Bookshare actions to  handlers.")

(defun emacspeak-bookshare-action-set (action handler)
  "Set up action handler."
  (cl-declare (special emacspeak-bookshare-action-table))
  (setf (gethash action emacspeak-bookshare-action-table) handler))

(defun emacspeak-bookshare-action-get (action)
  "Retrieve action handler."
  (cl-declare (special emacspeak-bookshare-action-table))
  (or (gethash action emacspeak-bookshare-action-table)
      (error "No handler defined for action %s" action)))

(define-derived-mode emacspeak-bookshare-mode special-mode
  "Bookshare Library"
  "A Bookshare front-end for the Emacspeak desktop.

The Emacspeak Bookshare front-end is launched by command
emacspeak-bookshare bound to \\[emacspeak-bookshare]

This command switches to a special buffer that has Bookshare
commands bounds to single keystrokes-- see the key-binding list at
the end of this description. Use Emacs online help facility to
look up help on these commands.

emacspeak-bookshare-mode provides the necessary functionality to
Search and download Bookshare material, Manage a local library of
downloaded Bookshare content, And commands to easily read newer
Daisy books from Bookshare.

Here is a list of all emacspeak Bookshare commands  with their key-bindings:
a Author Search
A Author/Title Search
t Title Search
s Full Text Search
d Date Search
b Browse

\\{emacspeak-bookshare-mode-map}"
  (let ((inhibit-read-only t)
        (start (point)))
    (goto-char (point-min))
    (insert "Browse And Read Bookshare Materials\n\n")
    (put-text-property start (point)
                       'face font-lock-doc-face)
    (setq header-line-format "Bookshare Library")
    (cd-absolute emacspeak-bookshare-directory)))

(cl-declaim (special emacspeak-bookshare-mode-map))

(cl-loop
 for a in
 '(
   ("+" emacspeak-bookshare-get-more-results)
   ("/" emacspeak-bookshare-title/author-search)
   ("I" emacspeak-bookshare-id-search)
   ("P" emacspeak-bookshare-list-preferences)
   ("S" emacspeak-bookshare-set-preference)
   ("a" emacspeak-bookshare-author-search)
   ("d" emacspeak-bookshare-since-search)
   ("i" emacspeak-bookshare-isbn-search)
   ("l" emacspeak-bookshare-browse-latest)
   ("m" emacspeak-bookshare-periodical-list)
   ("p" emacspeak-bookshare-browse-popular)
   ("s" emacspeak-bookshare-fulltext-search)
   ("t" emacspeak-bookshare-title-search)
   )
 do
 (progn
   (emacspeak-bookshare-action-set (cl-first a) (cl-second a))
   (define-key emacspeak-bookshare-mode-map (ems-kbd (cl-first a))
     'emacspeak-bookshare-action)))

;;}}}
;;{{{ Bookshare XML  handlers:

(defvar emacspeak-bookshare-handler-table
  (make-hash-table :test #'eq)
  "Table of handlers for processing  Bookshare response elements.")

(defun emacspeak-bookshare-handler-set (element handler)
  "Set up element handler."
  (cl-declare (special emacspeak-bookshare-handler-table))
  (setf (gethash element emacspeak-bookshare-handler-table) handler))

(defun emacspeak-bookshare-handler-get (element)
  "Retrieve action handler."
  (cl-declare (special emacspeak-bookshare-handler-table))
  (let ((handler (gethash element emacspeak-bookshare-handler-table)))
    (if (fboundp handler) handler 'emacspeak-bookshare-recurse)))

(defvar emacspeak-bookshare-response-elements
  '(bookshare debugInfo  version metadata messages string status-code
              book user string downloads-remaining
              id name value editable
              periodical list page num-pages limit result)
  "Bookshare response elements for which we have explicit handlers.")

(cl-loop
 for e in emacspeak-bookshare-response-elements
 do
 (emacspeak-bookshare-handler-set
  e
  (intern (format "emacspeak-bookshare-%s-handler" (symbol-name e)))))

(cl-loop
 for container in
 '(book list periodical user)
 do
 (eval
  `(defun
       ,(intern (format "emacspeak-bookshare-%s-handler"
                        (symbol-name container)))
       (element)
     "Process children silently."
     (mapc #'emacspeak-bookshare-apply-handler (dom-children element)))))

(defun emacspeak-bookshare-apply-handler (element)
  "Lookup and apply installed handler."
  (let* ((tag (dom-tag element))
         (handler  (emacspeak-bookshare-handler-get tag)))
    (cond
     ((and handler (fboundp handler))
      (funcall handler element))
     (t ; Can't get here:
      (insert (format "Handler for %s not implemented yet.\n" tag))))))

(defun emacspeak-bookshare-bookshare-handler (response)
  "Handle Bookshare response."
  (unless (eq (dom-tag response) 'bookshare)
    (error "Does not look like a Bookshare response."))
  (mapc #'emacspeak-bookshare-apply-handler (dom-children response)))

(cl--defalias 'emacspeak-bookshare-version-handler 'ignore)
(cl--defalias 'emacspeak-bookshare-debugInfo-handler 'ignore)

(defun emacspeak-bookshare-recurse (tree)
  "Recurse down tree."
  (insert (format "Begin %s:\n" (dom-tag tree)))
  (mapc #'emacspeak-bookshare-apply-handler (dom-children tree))
  (insert (format "\nEnd %s\n" (dom-tag tree))))

(defun emacspeak-bookshare-messages-handler (messages)
  "Handle messages element."
  (cl-declare (special emacspeak-bookshare-last-action-uri))
  (let ((start (point)))
    (mapc #'insert(dom-text   (dom-child-by-tag messages 'string)))
    (insert "\t")
    (insert
     (mapconcat
      #'identity
      (emacspeak-bookshare-destruct-rest-url emacspeak-bookshare-last-action-uri)
      " "))
    (add-text-properties  start (point)
                          (list 'uri emacspeak-bookshare-last-action-uri
                                'face 'font-lock-string-face))
    (insert "\n")))

(defun emacspeak-bookshare-status-code-handler (status-code)
  "Handlestatus-code element."
  (cl-declare (special emacspeak-bookshare-last-action-uri))
  (let ((start (point)))
    (message "Status-Code: %s" (dom-text    status-code))
    (insert "Status Code: ")
    (mapc #'insert (dom-text    status-code))
    (insert "\t")
    (insert
     (mapconcat
      #'identity
      (emacspeak-bookshare-destruct-rest-url emacspeak-bookshare-last-action-uri)
      " "))
    (add-text-properties  start (point)
                          (list 'uri emacspeak-bookshare-last-action-uri
                                'face 'font-lock-string-face))
    (insert "\n")))

(defun emacspeak-bookshare-page-handler (page)
  "Handle page element."
  (insert (format "Page: %s\t" (dom-text page))))

(defun emacspeak-bookshare-limit-handler (limit)
  "Handle limit element."
  (insert (format "Limit: %s\t" (dom-text limit))))

(defun emacspeak-bookshare-num-pages-handler (num-pages)
  "Handle num-pages element."
  (insert (format "Num-Pages: %s\n" (dom-text num-pages))))

(defun emacspeak-bookshare-display-setting (result)
  "Display user setting result."
  (mapc #'emacspeak-bookshare-apply-handler (dom-children result)))

(defun emacspeak-bookshare-result-handler (result)
  "Handle result element in Bookshare response."
  (insert "\n")
  (cond
   ((dom-child-by-tag result 'editable) ;handle settings
    (emacspeak-bookshare-display-setting result))
   (t ;Book Result
    (let ((start (point))
          (id (dom-text (dom-child-by-tag result 'id)))
          (title (emacspeak-bookshare-dom-clean-text result 'title))
          (author (emacspeak-bookshare-dom-clean-text result 'author))
          (directory nil)
          (target nil)
          (face nil)
          (icon nil))
      (unless ; We found a meaningful author or title
          (and (zerop (length title)) (zerop (length author)))
        (setq
         directory (emacspeak-bookshare-generate-directory author title)
         target (emacspeak-bookshare-generate-target author title))
                                        ;Render result with formatted properties
        (cond
         ((file-exists-p directory)
          (setq face 'highlight
                icon 'item))
         ((file-exists-p target)
          (setq face 'bold
                icon 'select-object))))
      (when title (insert (format "%s\t" title)))
      (when author
        (while (< (current-column)50)
          (insert "\t"))
        (insert (format "By %s" author)))
      (untabify start (point))
      (add-text-properties
       start (point)
       (list
        'author author 'title title 'id id
        'directory directory 'target target
        'face face 'auditory-icon icon))))))

(defvar emacspeak-bookshare-metadata-filtered-elements
  '(author bookshare-id brf content-id
           daisy images download-format title)
  "Elements in Bookshare Metadata that we filter.")

(defvar emacspeak-bookshare-leaf-elements
  '(string downloads-remaining
           id name value editable)
  "Leaf level elements, just print element name: children.")

(cl-loop
 for e in
 emacspeak-bookshare-leaf-elements
 do
 (eval
  `(defun
       ,(intern (format "emacspeak-bookshare-%s-handler" (symbol-name e)))
       (element)
     ,(format "Handle leaf-level element  %s. " e)
     (insert (format "%s:\t" ,e))
     (mapc #'insert (dom-children  element))
     (insert "\n"))))

(defun emacspeak-bookshare-metadata-handler (metadata)
  "Handle metadata element."
  (cl-declare (special emacspeak-bookshare-metadata-filtered-elements))
  (let* ((children (dom-children metadata))
         (available (dom-by-tag metadata 'download-format))
         (display
          (cl-remove-if
           #'(lambda (c)
               (member (dom-tag c)
                       emacspeak-bookshare-metadata-filtered-elements))
           children)))
    ;;; First render generic metadata items to display
    (mapc
     #'(lambda (child)
         (let ((start (point)))
           (insert
            (format "%s: "
                    (capitalize (symbol-name (dom-tag child)))))
           (put-text-property start (point)
                              'face 'highlight)
           (insert
            (format "%s\n"
                    (xml-substitute-special
                     (xml-substitute-numeric-entities
                      (dom-text child)))))
           (fill-region-as-paragraph start (point))))
     (sort
      display
      #'(lambda (a b)
          (string-lessp (symbol-name (car a)) (symbol-name (car b))))))
                                        ; Show availability:
    (insert
     (format "Available: %s"
             (mapconcat #'dom-text available " ")))))

;;}}}
;;{{{  Property Accessors:

;;{{{ Generate Declarations:
;; (cl-loop
;;  for p in
;;  '(author title id metadata target directory)
;;  do
;;  (declare-function (format "emacspeak-bookshare-get-%s"  p) "emacspeak-bookshare" nil))

(declare-function emacspeak-bookshare-get-author    "emacspeak-bookshare" nil)

(declare-function emacspeak-bookshare-get-title    "emacspeak-bookshare" nil)
(declare-function emacspeak-bookshare-get-id    "emacspeak-bookshare" nil)
(declare-function emacspeak-bookshare-get-metadata    "emacspeak-bookshare" nil)
(declare-function emacspeak-bookshare-get-target    "emacspeak-bookshare" nil)
(declare-function emacspeak-bookshare-get-directory    "emacspeak-bookshare" nil)

;; ;;}}}
(cl-loop for p in
         '(author title id metadata target directory)
         do
         (eval
          `(defun ,(intern (format "emacspeak-bookshare-get-%s" p)) ()
             ,(format "Auto-generated function: Get %s at point. " p)
             (get-text-property (point) ',p))))

;;}}}
;;{{{ Bookshare Mode:

(defun emacspeak-bookshare-define-keys ()
  "Define keys for  Bookshare Interaction."
  (cl-declare (special emacspeak-bookshare-mode-map))
  (cl-loop for k in
           '(
             ("e" emacspeak-bookshare-eww)
             ("q" bury-buffer)
             ("f" emacspeak-bookshare-flush-lines)
             ("v" emacspeak-bookshare-view)
             ("c" emacspeak-bookshare-toc-at-point)
             ("\C-m" emacspeak-bookshare-toc-at-point)
             ("M-n" emacspeak-bookshare-next-result)
             ("n" emacspeak-bookshare-next-result)
             ("p" emacspeak-bookshare-previous-result)
             ("M-p" emacspeak-bookshare-previous-result)
             ("["  backward-page)
             ("]" forward-page)
             ("b" emacspeak-bookshare-browse)
             ("SPC" emacspeak-bookshare-expand-at-point)
             ("U" emacspeak-bookshare-unpack-at-point)
             ("A" emacspeak-bookshare-download-audio-at-point)
             ("3" emacspeak-bookshare-download-epub-3-at-point)
             ("V" emacspeak-bookshare-view-at-point)
             ("C" emacspeak-bookshare-fulltext)
             ("D" emacspeak-bookshare-download-daisy-at-point)
             ("E" emacspeak-bookshare-eww)
             ("B" emacspeak-bookshare-download-brf-at-point)
             ("j" next-line)
             ("k" previous-line)
             )
           do
           (emacspeak-keymap-update  emacspeak-bookshare-mode-map k)))

(emacspeak-bookshare-define-keys)

(defvar emacspeak-bookshare-interaction-buffer "*Bookshare*"
  "Buffer for Bookshare interaction.")

;;;###autoload
(defun emacspeak-bookshare ()
  "Bookshare  Interaction."
  (interactive)
  (cl-declare (special emacspeak-bookshare-interaction-buffer))
  (let ((buffer (get-buffer emacspeak-bookshare-interaction-buffer)))
    (cond
     ((buffer-live-p buffer)
      (switch-to-buffer buffer))
     (t
      (with-current-buffer
          (get-buffer-create emacspeak-bookshare-interaction-buffer)
        (erase-buffer)
        (setq buffer-undo-list t)
        (setq buffer-read-only t)
        (emacspeak-bookshare-mode))
      (switch-to-buffer emacspeak-bookshare-interaction-buffer)))
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defun emacspeak-bookshare-action  ()
  "Call action specified by  invoking key."
  (interactive)
  (emacspeak-bookshare-assert)
  (goto-char (point-max))
  (let* ((inhibit-read-only t)
         (key (format "%c" last-input-event))
         (start nil)
         (response (call-interactively (emacspeak-bookshare-action-get key))))
    (insert "\n\f\n")
    (setq start (point))
    (emacspeak-bookshare-bookshare-handler response)
    (goto-char start)
    (emacspeak-auditory-icon 'task-done)
    (emacspeak-speak-line)))

(defun emacspeak-bookshare-browse ()
  "Browse Bookshare."
  (interactive)
  (let ((action (read-char "p Popular, l Latest")))
    (cl-case action
      (?p (call-interactively 'emacspeak-bookshare-action))
      (?l (call-interactively 'emacspeak-bookshare-action))
      (otherwise (error "Unrecognized browse action.")))))

(defun emacspeak-bookshare-expand-at-point ()
  "Expand entry at point by retrieving metadata.
Once retrieved, memoize to avoid multiple retrievals."
  (interactive)
  (emacspeak-bookshare-assert)
  (emacspeak-auditory-icon 'open-object)
  (let* ((inhibit-read-only t)
         (id (emacspeak-bookshare-get-id))
         (author (emacspeak-bookshare-get-author))
         (title (emacspeak-bookshare-get-title))
         (target (emacspeak-bookshare-generate-target author title))
         (metadata (emacspeak-bookshare-get-metadata))
         (start nil)
         (response (emacspeak-bookshare-id-search id)))
    (cond
     (metadata (message "Entry already expanded."))
     (t
      (add-text-properties (line-beginning-position)
                           (line-end-position)
                           (list  'metadata t))
      (goto-char (line-end-position))
      (insert "\n")
      (setq start (point))
      (emacspeak-bookshare-bookshare-handler response)
      (add-text-properties start (point)
                           (list 'metadata t 'id id 'target target))
      (indent-rigidly start (point) 4)
      (emacspeak-speak-region start (point))))
    (goto-char start)
    (emacspeak-auditory-icon 'large-movement)))

(defun emacspeak-bookshare-download-daisy-at-point ()
  "Download Daisy version of book under point.
Target location is generated from author and title."
  (interactive)
  (let* ((inhibit-read-only t)
         (id (emacspeak-bookshare-get-id))
         (author (emacspeak-bookshare-get-author))
         (title (emacspeak-bookshare-get-title))
         (target (emacspeak-bookshare-generate-target author title)))
    (emacspeak-auditory-icon 'select-object)
    (cond
     ((file-exists-p target)
      (message "This content is available locally at %s" target))
     (t
      (cond
       ((zerop (emacspeak-bookshare-download-daisy id target))
        (add-text-properties
         (line-beginning-position) (line-end-position)
         (list'face 'bold
                    'auditory-icon 'select-object))
        (emacspeak-auditory-icon 'task-done)
        (message "Downloaded content to %s" target))
       (t
        (let ((new (read-from-minibuffer "Retry with new target:" target)))
          (if (zerop (emacspeak-bookshare-download-daisy id new))
              (message "Downloaded to %s" new)
            (error "Error downloading to %s" new)))))))))

(defun emacspeak-bookshare-download-audio-at-point ()
  "Download audio version of book under point.
Target location is generated from author and title."
  (interactive)
  (let* ((inhibit-read-only t)
         (id (emacspeak-bookshare-get-id))
         (author (emacspeak-bookshare-get-author))
         (title (emacspeak-bookshare-get-title))
         (target (emacspeak-bookshare-generate-target author title "audio")))
    (emacspeak-auditory-icon 'select-object)
    (cond
     ((file-exists-p target)
      (message "This content is available locally at %s" target))
     (t
      (cond
       ((zerop (emacspeak-bookshare-download-audio id target))
        (add-text-properties
         (line-beginning-position) (line-end-position)
         (list'face 'bold
                    'auditory-icon 'select-object))
        (emacspeak-auditory-icon 'task-done)
        (message "Downloaded content to %s" target))
       (t
        (let ((new (read-from-minibuffer "Retry with new target:" target)))
          (if (zerop (emacspeak-bookshare-download-audio id new))
              (message "Downloaded to %s" new)
            (error "Error downloading to %s" new)))))))))

(defun emacspeak-bookshare-download-epub-3-at-point ()
  "Download epub-3 version of book under point.
Target location is generated from author and title."
  (interactive)
  (let* ((inhibit-read-only t)
         (id (emacspeak-bookshare-get-id))
         (author (emacspeak-bookshare-get-author))
         (title (emacspeak-bookshare-get-title))
         (target (emacspeak-bookshare-generate-target author title "epub-3")))
    (emacspeak-auditory-icon 'select-object)
    (cond
     ((file-exists-p target)
      (message "This content is available locally at %s" target))
     (t
      (cond
       ((zerop (emacspeak-bookshare-download-epub-3 id target))
        (add-text-properties
         (line-beginning-position) (line-end-position)
         (list'face 'bold
                    'auditory-icon 'select-object))
        (emacspeak-auditory-icon 'task-done)
        (message "Downloaded content to %s" target))
       (t
        (let ((new (read-from-minibuffer "Retry with new target:" target)))
          (if (zerop (emacspeak-bookshare-download-epub-3 id new))
              (message "Downloaded to %s" new)
            (error "Error downloading to %s" new)))))))))

(defun emacspeak-bookshare-download-brf-at-point ()
  "Download Braille version of book under point.
Target location is generated from author and title."
  (interactive)
  (let* ((inhibit-read-only t)
         (id (emacspeak-bookshare-get-id))
         (author (emacspeak-bookshare-get-author))
         (title (emacspeak-bookshare-get-title))
         (target (emacspeak-bookshare-generate-target author title)))
    (emacspeak-auditory-icon 'select-object)
    (cond
     ((file-exists-p target)
      (message "This content is available locally at %s" target))
     (t
      (cond
       ((zerop (emacspeak-bookshare-download-brf id target))
        (add-text-properties
         (line-beginning-position) (line-end-position)
         (list'face 'bold
                    'auditory-icon 'select-object))
        (emacspeak-auditory-icon 'task-done)
        (message "Downloaded content to %s" target))
       (t (error "Error downloading content.")))
      (emacspeak-auditory-icon 'task-done)
      (message "Downloading content to %s" target)))))

(defun emacspeak-bookshare-unpack-at-point ()
  "Unpack downloaded content if necessary."
  (interactive)
  (emacspeak-bookshare-assert)
  (let ((inhibit-read-only t)
        (target (emacspeak-bookshare-get-target))
        (directory nil))
    (when (null target) (error  "No downloaded content here."))
    (unless   (file-exists-p target) (error "First download this content."))
    (setq directory (emacspeak-bookshare-get-directory))
    (when (file-exists-p directory) (error "Already unpacked."))
    (make-directory directory 'parents)
    (shell-command
     (format "cd \"%s\"; unzip -P %s %s"
             directory
             (cdr (emacspeak-bookshare-get-auth-info))
             target))
    (add-text-properties
     (line-beginning-position) (line-end-position)
     (list'face 'highlight
                'auditory-icon 'item))
    (message "Unpacked content.")))

(defvar emacspeak-bookshare-xslt
  "daisyTransform.xsl"
  "Name of bookshare  XSL transform.")

(defun emacspeak-bookshare-xslt (directory)
  "Return suitable XSL  transform."
  (cl-declare (special emacspeak-bookshare-xslt
                       emacspeak-xslt-directory))
  (let ((xsl (expand-file-name emacspeak-bookshare-xslt directory)))
    (cond
     ((file-exists-p xsl) xsl)
     (t (expand-file-name emacspeak-bookshare-xslt emacspeak-xslt-directory)))))

(defvar emacspeak-bookshare-toc-xslt
  "bookshare-toc.xsl"
  "Name of bookshare supplied XSL transform.")

(defun emacspeak-bookshare-toc-xslt ()
  "Return suitable XSL  transform for TOC."
  (cl-declare (special emacspeak-bookshare-toc-xslt
                       emacspeak-xslt-directory))

  (expand-file-name emacspeak-bookshare-toc-xslt emacspeak-xslt-directory))
(declare-function emacspeak-xslt-view-file "emacspeak-xslt" (style file))

(defun emacspeak-bookshare-view-at-point ()
  "View book at point.
Make sure it's downloaded and unpacked first."
  (interactive)
  (let* ((target (emacspeak-bookshare-get-target))
         (directory (emacspeak-bookshare-get-directory))
         (xsl (emacspeak-bookshare-xslt  directory)))
    (unless (file-exists-p target)
      (error "First download this content."))
    (unless (file-exists-p directory)
      (error "First unpack this content."))
    (emacspeak-xslt-view-file
     xsl
     (cl-first
      (directory-files directory 'full "\\.xml\\'")))))

(defun emacspeak-bookshare-url-executor (url)
  "Custom URL executor for use in Bookshare TOC."
  (interactive "sURL: ")
  (cond
   ((string-match "#" url)
    (emacspeak-bookshare-extract-and-view url))
   ((char-equal ??  (aref url (1- (length url))))
    (emacspeak-bookshare-view-page-range (substring url 0 -1)))
   (t (error "Doesn't look like a bookshare-specific URL."))))

(defun emacspeak-bookshare-toc-at-point ()
  "View TOC for book at point.
Make sure it's downloaded and unpacked first."
  (interactive)
  (let ((target (emacspeak-bookshare-get-target))
        (directory (emacspeak-bookshare-get-directory))
        (xsl (emacspeak-bookshare-toc-xslt)))
    (cond
     ((null target) (call-interactively 'emacspeak-bookshare-toc))
     (t
      (unless (file-exists-p target)
        (error "First download this content."))
      (unless (file-exists-p directory)
        (error "First unpack this content."))
      (add-hook
       'emacspeak-eww-post-process-hook
       #'(lambda ()
           (cl-declare (special emacspeak-we-url-executor))
           (setq emacspeak-we-url-executor 'emacspeak-bookshare-url-executor)
           (emacspeak-speak-mode-line)
           (emacspeak-auditory-icon 'open-object)))
      (emacspeak-xslt-view-file
       xsl
       (shell-quote-argument
        (cl-first
         (directory-files directory 'full "\\.xml\\'"))))))))

(defun emacspeak-bookshare-extract-xml (url)
  "Extract content referred to by link under point, and return an XML buffer."
  (interactive "sURL: ")
  (cl-declare (special  emacspeak-we-xsl-filter))
  (let ((fields (split-string url "#"))
        (id nil)
        (url nil))
    (unless (= (length fields) 2)
      (error "No fragment identifier in this link."))
    (setq url (cl-first fields)
          id (cl-second fields))
    (emacspeak-xslt-url
     emacspeak-we-xsl-filter
     url
     (emacspeak-xslt-params-from-xpath
      (format "//*[@id=\"%s\"]" id) url))))

(defun emacspeak-bookshare-extract-and-view (url)
  "Extract content referred to by link under point, and render via the browser."
  (interactive "sURL: ")
  (cl-declare (special emacspeak-bookshare-browser-function
                       emacspeak-xslt-directory))
  (let ((result (emacspeak-bookshare-extract-xml url))
        (browse-url-browser-function emacspeak-bookshare-browser-function))
    (save-current-buffer
      (set-buffer result)
      (emacspeak-eww-autospeak)
      (browse-url-of-buffer))))

(defun emacspeak-bookshare-view-page-range (url)
  "Play pages in specified page range from URL."
  (interactive "sURL:")
  (cl-declare (special emacspeak-bookshare-browser-function))
  (let* ((start (read-from-minibuffer "Start Page: "))
         (end (read-from-minibuffer "End Page: "))
         (result
          (emacspeak-xslt-xml-url
           (emacspeak-xslt-get "dtb-page-range.xsl")
           (substring url 7)
           (list
            (cons "start" (format "'%s'" start))
            (cons "end" (format "'%s'" end)))))
         (browse-url-browser-function emacspeak-bookshare-browser-function))
    (save-current-buffer
      (set-buffer result)
      (emacspeak-eww-autospeak)
      (browse-url-of-buffer))
    (kill-buffer result)))

(defun emacspeak-bookshare-view (directory)
  "View book in specified directory."
  (interactive
   (list
    (let ((completion-ignore-case t)
          (emacspeak-speak-messages nil)
          (read-file-name-completion-ignore-case t))
      (read-directory-name "Book: "
                           (when (eq major-mode 'dired-mode)
                             (dired-get-filename))
                           emacspeak-bookshare-directory))))
  (cl-declare (special emacspeak-bookshare-directory))
  (let* ((xsl (emacspeak-bookshare-xslt directory)))
    (emacspeak-xslt-view-file
     xsl
     (cl-first
      (directory-files directory 'full "\\.xml\\'")))))

(defun emacspeak-bookshare-toc (directory)
  "View TOC for book in specified directory."
  (interactive
   (list
    (let ((completion-ignore-case t)
          (emacspeak-speak-messages nil)
          (read-file-name-completion-ignore-case t))
      (read-directory-name "Book: "
                           (when (eq major-mode 'dired-mode)
                             (dired-get-filename))
                           emacspeak-bookshare-directory))))
  (cl-declare (special emacspeak-bookshare-directory))
  (let* ((xsl (emacspeak-bookshare-toc-xslt)))
    (add-hook
     'emacspeak-eww-post-process-hook
     #'(lambda ()
         (cl-declare (special emacspeak-we-url-executor))
         (setq emacspeak-we-url-executor 'emacspeak-bookshare-url-executor)))
    (emacspeak-xslt-view-file
     xsl
     (cl-first (directory-files directory 'full "\\.xml\\'")))))

(defvar emacspeak-bookshare-html-to-text-command
  "lynx -dump -stdin"
  "Command to convert html to text on stdin.")

(defun emacspeak-bookshare-fulltext (directory)
  "Display fulltext contents of  book in specified directory.
Useful for fulltext search in a book."
  (interactive
   (list
    (or (emacspeak-bookshare-get-directory)
        (let ((completion-ignore-case t)
              (emacspeak-speak-messages nil)
              (read-file-name-completion-ignore-case t))
          (read-directory-name "Book: "
                               (when (eq major-mode 'dired-mode)
                                 (dired-get-filename))
                               emacspeak-bookshare-directory)))))
  (cl-declare (special emacspeak-xslt-program))
  (cl-declare (special emacspeak-bookshare-html-to-text-command
                       emacspeak-bookshare-directory))
  (let ((xsl (emacspeak-bookshare-xslt directory))
        (buffer (get-buffer-create "Full Text"))
        (command nil)
        (inhibit-read-only t))
    (with-current-buffer buffer
      (setq command
            (format
             "%s  --nonet --novalid %s %s | %s"
             emacspeak-xslt-program xsl
             (shell-quote-argument
              (cl-first (directory-files directory 'full "\\.xml\\'")))
             emacspeak-bookshare-html-to-text-command))
      (erase-buffer)
      (setq buffer-undo-list t)
      (shell-command command (current-buffer) nil)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (switch-to-buffer buffer)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))
(defvar-local emacspeak-bookshare-this-book nil
  "Record current book in buffer where it is rendered.")


(defun emacspeak-bookshare-eww (directory)
  "Render  book using EWW"
  (interactive
   (list
    (or (emacspeak-bookshare-get-directory)
        (let ((completion-ignore-case t)
              (emacspeak-speak-messages nil)
              (read-file-name-completion-ignore-case t))
          (read-directory-name "Book: "
                               (when (eq major-mode 'dired-mode)
                                 (dired-get-filename))
                               emacspeak-bookshare-directory)))))
  (cl-declare (special emacspeak-xslt-program emacspeak-bookshare-directory
                       emacspeak-speak-directory-settings emacspeak-bookshare-this-book))
  (unless (fboundp 'eww)
    (error "Your Emacs doesn't have EWW."))
  (let ((gc-cons-threshold (max 8000000 gc-cons-threshold))
        (xsl (emacspeak-bookshare-xslt directory))
        (buffer (get-buffer-create "Full Text"))
        (command nil)
        (inhibit-read-only t))
    (with-current-buffer buffer
      (setq command
            (format
             "%s  --nonet --novalid %s %s "
             emacspeak-xslt-program xsl
             (shell-quote-argument
              (cl-first (directory-files directory 'full "\\.xml\\'")))))
      (erase-buffer)
      (setq buffer-undo-list t)
      (shell-command command (current-buffer) nil)
      (add-hook
       'emacspeak-eww-post-process-hook
       #'(lambda nil
           (setq emacspeak-bookshare-this-book directory)
           (emacspeak-speak-load-directory-settings directory)
           (emacspeak-auditory-icon 'open-object)
           (emacspeak-speak-mode-line)))
      (browse-url-of-buffer)
      (kill-buffer buffer))))

;;}}}
;;{{{ Navigation in  Bookshare Interaction

(defun emacspeak-bookshare-next-result ()
  "Move to next result."
  (interactive)
  (goto-char (line-end-position))
  (goto-char (next-single-property-change (point) 'id))
  (emacspeak-auditory-icon 'large-movement)
  (forward-char 1)
  (emacspeak-speak-line))

(defun emacspeak-bookshare-previous-result()
  "Move to previous result."
  (interactive)
  (goto-char (previous-single-property-change (point) 'id))
  (beginning-of-line)
  (emacspeak-auditory-icon 'large-movement)
  (emacspeak-speak-line))

(defun emacspeak-bookshare-flush-lines(regexp)
  "Flush lines matching regexp in Bookshare buffer."
  (interactive "sRegexp: ")
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (next-single-property-change (point-min) 'face))
      (flush-lines regexp (point) (point-max)))))

;;}}}
(provide 'emacspeak-bookshare)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

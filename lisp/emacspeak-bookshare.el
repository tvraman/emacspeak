;;; emacspeak-bookshare.el --- Speech-enabled  BOOKSHARE client
;;; $Id: emacspeak-bookshare.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable BOOKSHARE An Emacs Interface to bookshare
;;; Keywords: Emacspeak,  Audio Desktop bookshare
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2011, T. V. Raman
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
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; BOOKSHARE == http://www.bookshare.org provides book access to print-disabled users.
;;; It provides a simple Web  API http://developer.bookshare.org
;;; This module implements an Emacspeak Bookshare client.
;;; For now, users will need to get their own API key

;;; Code:
;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'dired)
(require 'browse-url)
(require 'emacspeak-we)
(require 'emacspeak-webutils)
(require 'emacspeak-xslt)
(require 'xml-parse)
(require 'xml)
(require 'derived)
;;}}}
;;{{{ Customizations

(defgroup emacspeak-bookshare nil
  "Bookshare Access on the Complete Audio Desktop."
  :group 'emacspeak)

(defcustom emacspeak-bookshare-api-key nil
  "Web API  key for this application."
  :type
  '(choice :tag "Key: "
           (const :tag "Unspecified" nil)
           (string :tag "API Key: "))
  :group 'emacspeak-bookshare)

(defcustom emacspeak-bookshare-user-id nil
  "Bookshare user Id."
  :type '(choice :tag "Bookshare User id"
                 (const :tag "None" nil)
                 (string :tag "Email"))
  :group 'emacspeak-bookshare)
(defcustom emacspeak-bookshare-directory (expand-file-name "~/")
  "Customize this to the root of where books are organized."
  :type 'directory
  :group 'emacspeak-bookshare)

(defcustom emacspeak-bookshare-downloads-directory
  (expand-file-name "~/" emacspeak-bookshare-directory)
  "Customize this to the root of where books are organized."
  :type 'directory
  :group 'emacspeak-bookshare)
;;;###autoload
(defcustom emacspeak-bookshare-browser-function
  'browse-url-w3
  "Function to display Bookshare Book content in a WWW browser.
This is used by the various Bookshare view commands to display
  content from Daisy books."
  :type '(choice
          (function-item :tag "Emacs W3" :value  browse-url-w3)
          (function-item :tag "Emacs EWW" :value  eww-browse-url)
          (function-item :tag "Mozilla" :value  browse-url-mozilla)
          (function-item :tag "Firefox" :value browse-url-firefox)
          (function-item :tag "Chromium" :value browse-url-chromium)
          (function-item :tag "Text browser in an Emacs window"
                         :value browse-url-text-emacs)
          (function-item :tag "Default Mac OS X browser"
                         :value browse-url-default-macosx-browser)
          (function :tag "Your own function"))
  :version "37"
  :group 'emacspeak-bookshare)

;;}}}
;;{{{ XML Compatibility:
(unless (fboundp 'xml-substitute-numeric-entities)
;;; cloned from xml.el in emacs 24
  (defun xml-substitute-numeric-entities (string)
    "Substitute SGML numeric entities by their respective utf characters.
This function replaces numeric entities in the input STRING and
returns the modified string.  For example \"&#42;\" gets replaced
by \"*\"."
    (if (and string (stringp string))
        (let ((start 0))
          (while (string-match "&#\\([0-9]+\\);" string start)
            (condition-case nil
                (setq string (replace-match
                              (string (read (substring string
                                                       (match-beginning 1)
                                                       (match-end 1))))
                              nil nil string))
              (error nil))
            (setq start (1+ (match-beginning 0))))
          string)
      nil)))

;;}}}
;;{{{ Variables:

(defvar emacspeak-bookshare-curl-program (executable-find "curl")
  "Curl executable.")

(defvar emacspeak-bookshare-curl-common-options
  " --insecure "
  "Common Curl options for Bookshare. Includes --insecure  as per Bookshare docs.")

(defvar emacspeak-bookshare-api-base
  "https://api.bookshare.org"
  "Base end-point for Bookshare API  access.")

;;}}}
;;{{{ Helpers:
(defsubst emacspeak-bookshare-assert ()
  "Error out if not in Bookshare mode."
  (unless (eq major-mode 'emacspeak-bookshare-mode)
    (error "Not in Bookshare Interaction.")))
(defvar emacspeak-bookshare-md5-cached-token nil
  "Cache MD5 token for future use.")
(defvar emacspeak-bookshare-password-cache nil
  "Cache user password for this session.")

(defsubst emacspeak-bookshare-user-password ()
  "User password.
Memoize token, and return token encoded using md5, and packaged
with X-password HTTP header for use with Curl."
  (declare (special emacspeak-bookshare-md5-cached-token emacspeak-bookshare-password-cache))
  (setq emacspeak-bookshare-password-cache
        (or  emacspeak-bookshare-password-cache
             (read-passwd
              (format "Bookshare password for %s: "
                      emacspeak-bookshare-user-id))))
  (setq emacspeak-bookshare-md5-cached-token (md5 emacspeak-bookshare-password-cache))
  (format "-H 'X-password: %s'" emacspeak-bookshare-md5-cached-token))

(defsubst emacspeak-bookshare-rest-endpoint (operation operand &optional no-auth)
  "Return  URL  end point for specified operation.
Optional argument `no-auth' says no user auth needed."
  (declare (special emacspeak-bookshare-api-base emacspeak-bookshare-user-id))
  (format "%s/%s/%s/%s?api_key=%s"
          emacspeak-bookshare-api-base
          operation
          operand
          (if no-auth
              ""
            (format "for/%s" emacspeak-bookshare-user-id))
          emacspeak-bookshare-api-key))

(defsubst emacspeak-bookshare-page-rest-endpoint ()
  "Generate REST endpoint for the next page of results."
  (declare (special emacspeak-bookshare-last-action-uri))
  (unless emacspeak-bookshare-last-action-uri
    (error "No query to  page!"))
  (let ((root
         (first (split-string emacspeak-bookshare-last-action-uri "/for")))
        (page nil))
    (setq page (string-match "/page/" root))
    (cond
     (page
      (setq page (split-string root "/page/"));Already paged once
      (format "%s/page/%s/for/%s?api_key=%s"
              (first page)
              (1+ (read (second page)))
              emacspeak-bookshare-user-id
              emacspeak-bookshare-api-key))
     (t
      (format "%s/page/2/for/%s?api_key=%s"
              root
              emacspeak-bookshare-user-id
              emacspeak-bookshare-api-key)))))

(defsubst emacspeak-bookshare-destruct-rest-url (url)
  "Return operator and operand used to construct this REST end-point."
  (declare (special emacspeak-bookshare-api-base))
  (let* ((start (length emacspeak-bookshare-api-base))
         (end (string-match "for/" url)))
    (nthcdr 2
            (split-string
             (substring url start end) "/" 'no-null))))

(defsubst emacspeak-bookshare-download-url (id fmt )
  "Return  URL  end point for content download.
Argument id specifies content. Argument fmt = 0 for Braille, 1
  for Daisy."
  (declare (special emacspeak-bookshare-api-base emacspeak-bookshare-user-id))
  (format "%s/%s/%s?api_key=%s"
          emacspeak-bookshare-api-base
          (format "download/content/%s/version/%s" id fmt)
          (format "for/%s" emacspeak-bookshare-user-id)
          emacspeak-bookshare-api-key))

(defvar emacspeak-bookshare-scratch-buffer " *Bookshare Scratch* "
  "Scratch buffer for Bookshare operations.")

(defmacro emacspeak-bookshare-using-scratch(&rest body)
  "Evaluate forms in a  ready to use temporary buffer."
  `(let ((buffer (get-buffer-create emacspeak-bookshare-scratch-buffer))
         (default-process-coding-system (cons 'utf-8 'utf-8))
         (coding-system-for-read 'binary)
         (coding-system-for-write 'binary)
         (buffer-undo-list t))
     (save-current-buffer
       (set-buffer buffer)
       (kill-all-local-variables)
       (erase-buffer)
       (progn ,@body))))

(defsubst emacspeak-bookshare-get-result (command)
  "Run command and return its output."
  (declare (special shell-file-name shell-command-switch))
  (emacspeak-bookshare-using-scratch
   (call-process shell-file-name nil t
                 nil shell-command-switch
                 command)
   (goto-char (point-min))
   (read-xml)))
(defvar emacspeak-bookshare-last-action-uri nil
  "Cache last API call URI.")

(defun emacspeak-bookshare-api-call (operation operand &optional no-auth)
  "Make a Bookshare API  call and get the result.
Optional argument 'no-auth says we dont need a user auth."
  (declare (special emacspeak-bookshare-last-action-uri))
  (setq emacspeak-bookshare-last-action-uri
        (emacspeak-bookshare-rest-endpoint operation operand no-auth))
  (emacspeak-bookshare-get-result
   (format
    "%s %s %s  %s 2>/dev/null"
    emacspeak-bookshare-curl-program
    emacspeak-bookshare-curl-common-options
    (if no-auth "" (emacspeak-bookshare-user-password))
    emacspeak-bookshare-last-action-uri)))

(defun emacspeak-bookshare-get-more-results ()
  "Get next page of results for last query."
  (interactive)
  (declare (special emacspeak-bookshare-last-action-uri))
  (setq emacspeak-bookshare-last-action-uri
        (emacspeak-bookshare-page-rest-endpoint))
  (emacspeak-bookshare-get-result
   (format "%s %s %s  %s 2>/dev/null"
           emacspeak-bookshare-curl-program
           emacspeak-bookshare-curl-common-options
           (emacspeak-bookshare-user-password)
           emacspeak-bookshare-last-action-uri)))

(defsubst emacspeak-bookshare-generate-target (author title)
  "Generate a suitable filename target."
  (declare (special emacspeak-bookshare-downloads-directory))
  (expand-file-name 
   (replace-regexp-in-string
    "[ _&\'\":()\;]+" "-"
    (format "%s-%s.zip" author title))
   emacspeak-bookshare-downloads-directory))

(defsubst emacspeak-bookshare-generate-directory (author title)
  "Generate name of unpack directory."
  (declare (special emacspeak-bookshare-directory))
  (expand-file-name
   (replace-regexp-in-string
    "[ _&\'\":()\;]+" "-"
    (format "%s/%s" author title))
   emacspeak-bookshare-directory))

(defsubst emacspeak-bookshare-destruct-target (target)
  "Destruct  a  filename target into components."
  (split-string
   (substring target  0 -4)
   "-" 'no-null))

;;}}}
;;{{{ Book Actions:

(defvar emacspeak-bookshare-categories nil
  "Cached list of categories.")

(defun emacspeak-bookshare-categories ()
  "Return memoized list of categories."
  (declare (special emacspeak-bookshare-categories))
  (or
   emacspeak-bookshare-categories
   (setq
    emacspeak-bookshare-categories
    (let ((result
           (emacspeak-bookshare-api-call
            "reference/category/list" "" 'no-auth)))
      (setq result (xml-tag-child result  "category"))
      (setq result (xml-tag-child result "list"))
      (setq result (xml-tag-children result))
      (setq result
            (remove-if-not
             #'(lambda (c) (string= "result" (xml-tag-name c)))
             result))
      (loop for r in result
            collect
            (emacspeak-url-encode(cadr (second r))))))))
;;;  Following actions return book metadata:

(defsubst emacspeak-bookshare-isbn-search (query)
  "Perform a Bookshare isbn search."
  (interactive "sISBN: ")
  (emacspeak-bookshare-api-call "book/isbn" query ))

(defsubst emacspeak-bookshare-id-search (query)
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
    (emacspeak-url-encode
     (read-from-minibuffer "author: "))
    current-prefix-arg))
  (cond
   ((null category)                     ; plain search
    (emacspeak-bookshare-api-call "book/searchFTS/author" query))
   (t                                   ; filter using category:
    (let ((filter
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
    (emacspeak-url-encode
     (read-from-minibuffer "Title: "))
    current-prefix-arg))
  (cond
   ((null category)                     ; plain search
    (emacspeak-bookshare-api-call "book/searchFTS/title" query))
   (t                                   ; filter using category:
    (let ((filter
           (completing-read "Category: "
                            (emacspeak-bookshare-categories))))
      (emacspeak-bookshare-api-call
       "book/searchFTS/title"
       (format "%s/category/%s"
               query filter))))))

(defsubst emacspeak-bookshare-title/author-search (query)
  "Perform a Bookshare title/author  search."
  (interactive "sTitle/Author: ")
  (emacspeak-bookshare-api-call "book/searchTA" query))

(defsubst emacspeak-bookshare-fulltext-search (query)
  "Perform a Bookshare fulltext search."
  (interactive "sFulltext Search: ")
  (emacspeak-bookshare-api-call "book/searchFTS" query))

(defun emacspeak-bookshare-since-search (query &optional category)
  "Perform a Bookshare date  search.
Optional interactive prefix arg filters by category."
  (interactive
   (list
    (read-from-minibuffer "Date:MMDDYYYY ")
    current-prefix-arg))
  (cond
   ((null category)                     ; plain search
    (emacspeak-bookshare-api-call "book/search/since" query))
   (t                                   ; filter using category:
    (let ((filter
           (completing-read "Category: "
                            (emacspeak-bookshare-categories))))
      (emacspeak-bookshare-api-call
       "book/search/since"
       (format "%s/category/%s"
               query filter))))))

(defsubst emacspeak-bookshare-browse-latest()
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
    (let ((filter
           (completing-read "Category: "
                            (emacspeak-bookshare-categories))))
      (emacspeak-bookshare-api-call
       "book/browse/popular"
       (format "category/%s" filter))))      ))

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
(defsubst emacspeak-bookshare-download-internal(url target)
  "Download content  to target location."
  (interactive)
  (shell-command
   (format
    "%s %s %s  '%s' -o \"%s\""
    emacspeak-bookshare-curl-program
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

(defun emacspeak-bookshare-download-brf(id target)
  "Download Daisy format of specified book to target location."
  (interactive)
  (emacspeak-bookshare-download-internal
   (emacspeak-bookshare-download-url id 0)
   target))

;;}}}
;;{{{ Downloading Content:

;;}}}
;;{{{ Actions Table:

(defvar emacspeak-bookshare-action-table (make-hash-table :test #'equal)
  "Table mapping Bookshare actions to  handlers.")

(defsubst emacspeak-bookshare-action-set (action handler)
  "Set up action handler."
  (declare (special emacspeak-bookshare-action-table))
  (setf (gethash action emacspeak-bookshare-action-table) handler))

(defsubst emacspeak-bookshare-action-get (action)
  "Retrieve action handler."
  (declare (special emacspeak-bookshare-action-table))
  (or (gethash action emacspeak-bookshare-action-table)
      (error "No handler defined for action %s" action)))

(define-derived-mode emacspeak-bookshare-mode special-mode
  "Bookshare Library Of Accessible Books And Periodicals"
  "A Bookshare front-end for the Emacspeak desktop.

The Emacspeak Bookshare front-end is launched by command
emacspeak-bookshare bound to \\[emacspeak-bookshare]

This command switches to a special buffer that has Bookshare
commands bounds to single keystrokes-- see the key-binding list at
the end of this description. Use Emacs online help facility to
look up help on these commands.

emacspeak-bookshare-mode provides the necessary functionality to
Search and download Bookshare material,
Manage a local library of downloaded Bookshare content,
And commands to easily read newer Daisy books from Bookshare.

Here is a list of all emacspeak Bookshare commands along with their key-bindings:
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

(declaim (special emacspeak-bookshare-mode-map))

(loop for a in
      '(
        ("+" emacspeak-bookshare-get-more-results)
        ("P" emacspeak-bookshare-list-preferences)
        ("S" emacspeak-bookshare-set-preference)
        ("a" emacspeak-bookshare-author-search)
        ("t" emacspeak-bookshare-title-search)
        ("s" emacspeak-bookshare-fulltext-search)
        (" " emacspeak-bookshare-action)
        ("A" emacspeak-bookshare-title/author-search)
        ("d" emacspeak-bookshare-since-search)
        ("p" emacspeak-bookshare-browse-popular)
        ("l" emacspeak-bookshare-browse-latest)
        ("i" emacspeak-bookshare-isbn-search)
        ("I" emacspeak-bookshare-id-search)
        ("m" emacspeak-bookshare-periodical-list)
        )
      do
      (progn
        (emacspeak-bookshare-action-set (first a) (second a))
        (define-key emacspeak-bookshare-mode-map (first a) 'emacspeak-bookshare-action)))

;;}}}
;;{{{ Bookshare XML  handlers:

(defvar emacspeak-bookshare-handler-table
  (make-hash-table :test #'equal)
  "Table of handlers for processing  Bookshare response elements.")

(defsubst emacspeak-bookshare-handler-set (element handler)
  "Set up element handler."
  (declare (special emacspeak-bookshare-handler-table))
  (setf (gethash element emacspeak-bookshare-handler-table) handler))

(defsubst emacspeak-bookshare-handler-get (element)
  "Retrieve action handler."
  (declare (special emacspeak-bookshare-handler-table))
  (let ((handler (gethash element emacspeak-bookshare-handler-table)))
    (if (fboundp handler) handler 'emacspeak-bookshare-recurse)))

(defvar emacspeak-bookshare-response-elements
  '("bookshare"
    "version"
    "metadata"
    "messages"
    "string"
    "book"
    "user"
    "string" "downloads-remaining"
    "id" "name" "value" "editable"

    "id" "name" "value" "editable"
    "periodical"
    "list"
    "page"
    "num-pages"
    "limit"
    "result")
  "Bookshare response elements for which we have explicit
  handlers.")

(loop for e in emacspeak-bookshare-response-elements
      do
      (emacspeak-bookshare-handler-set
       e
       (intern (format "emacspeak-bookshare-%s-handler" e))))

(loop for container in
      '("book" "list" "periodical" "user")
      do
      (eval
       `(defun
            ,(intern (format "emacspeak-bookshare-%s-handler" container))
            (element)
          "Process children silently."
          (mapc #'emacspeak-bookshare-apply-handler (xml-tag-children element)))))

(defsubst emacspeak-bookshare-apply-handler (element)
  "Lookup and apply installed handler."
  (let* ((tag (xml-tag-name element))
         (handler  (emacspeak-bookshare-handler-get tag)))
    (cond
     ((and handler (fboundp handler))
      (funcall handler element))
     (t ; Cant get here:
      (insert (format "Handler for %s not implemented yet.\n" tag))))))

(defun emacspeak-bookshare-bookshare-handler (response)
  "Handle Bookshare response."
  (unless (string-equal (xml-tag-name response) "bookshare")
    (error "Does not look like a Bookshare response."))
  (mapc 'emacspeak-bookshare-apply-handler (xml-tag-children response)))

(defalias 'emacspeak-bookshare-version-handler 'ignore)

(defun emacspeak-bookshare-recurse (tree)
  "Recurse down tree."
  (insert (format "Begin %s:\n" (xml-tag-name tree)))
  (mapc #'emacspeak-bookshare-apply-handler (xml-tag-children tree))
  (insert (format "\nEnd %s\n" (xml-tag-name tree))))

(defun emacspeak-bookshare-messages-handler (messages)
  "Handle messages element."
  (declare (special emacspeak-bookshare-last-action-uri))
  (let ((start (point)))
    (mapc #'insert(rest  (xml-tag-child messages "string")))
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
  (insert (format "Page: %s\t" (second page))))

(defun emacspeak-bookshare-limit-handler (limit)
  "Handle limit element."
  (insert (format "Limit: %s\t" (second limit))))

(defun emacspeak-bookshare-num-pages-handler (num-pages)
  "Handle num-pages element."
  (insert (format "Num-Pages: %s\n" (second num-pages))))

(defun emacspeak-bookshare-display-setting (result)
  "Display user setting result."
  (mapc #'emacspeak-bookshare-apply-handler (xml-tag-children
                                             result)))

(defun emacspeak-bookshare-result-handler (result)
  "Handle result element in Bookshare response."
  (insert "\n")
  (let* ((children (xml-tag-children result))
         (start (point))
         (id (second (assoc "id" children)))
         (title (second (assoc "title" children)))
         (author (second (assoc "author" children)))
         (directory nil)
         (target nil)
         (face nil)
         (icon nil))
    (cond
     ((find-if
       #'(lambda (e) (string= (car e) "editable"))
       children) ; Handle user settings result:
      (emacspeak-bookshare-display-setting result))
     (t
      (when title
        (setq title
              (xml-substitute-special
               (xml-substitute-numeric-entities title))))
      (when author
        (setq author
              (xml-substitute-special
               (xml-substitute-numeric-entities author))))
      (when (or author title)
        (setq
         directory (emacspeak-bookshare-generate-directory author title)
         target (emacspeak-bookshare-generate-target author title))
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
       (list 'author author 'title title 'id id
             'directory directory
             'target target
             'face face
             'auditory-icon icon))))))

(defvar emacspeak-bookshare-metadata-filtered-elements
  '("author"
    "bookshare-id"
    "brf"
    "content-id"
    "daisy"
    "images"
    "download-format"
    "title")
  "Elements in Bookshare Metadata that we filter.")
(defvar emacspeak-bookshare-leaf-elements
  (list "string" "downloads-remaining"
        "id" "name" "value" "editable")
  "Leaf level elements, just print element name: children.")

(loop for e in
      emacspeak-bookshare-leaf-elements
      do
      (eval
       `(defun
            ,(intern (format "emacspeak-bookshare-%s-handler" e))
            (element)
          ,(format "Handle leaf-level element  %s. " e)
          (insert (format "%s:\t" ,e))
          (mapc #'insert (xml-tag-children  element))
          (insert "\n"))))

(defun emacspeak-bookshare-metadata-handler (metadata)
  "Handle metadata element."
  (declare (special emacspeak-bookshare-metadata-filtered-elements))
  (let*
      ((children (xml-tag-children metadata))
       (available
        (remove-if-not
         #'(lambda (c)
             (string= (car c) "download-format"))
         children))
       (display
        (remove-if
         #'(lambda (c)
             (member (car  c) emacspeak-bookshare-metadata-filtered-elements))
         children)))
    (mapc
     #'(lambda (child)
         (let ((start (point)))
           (insert (format "%s: "
                           (capitalize (first child))))
           (put-text-property start (point)
                              'face 'highlight)
           (insert
            (format "%s\n"
                    (xml-substitute-special
                     (xml-substitute-numeric-entities
                      (second child)))))
           (fill-region-as-paragraph start (point))))
     (sort
      display
      #'(lambda (a b ) (string-lessp (car a) (car b)))))
    (insert
     (format "Available: %s"
             (mapconcat
              #'(lambda (a) (second a))
              available
              " ")))))

;;}}}
;;{{{  Property Accessors:

(loop for p in
      '(author title id metadata target directory)
      do
      (eval
       `(defsubst ,(intern (format "emacspeak-bookshare-get-%s" p)) ()
          ,(format "Auto-generated function: Get %s at point. " p)
          (get-text-property (point) ',p))))

;;}}}
;;{{{ Bookshare Mode:

(defun emacspeak-bookshare-define-keys ()
  "Define keys for  Bookshare Interaction."
  (declare (special emacspeak-bookshare-mode-map))
  (loop for k in
        '(
          ("e" emacspeak-epub)
          ("q" bury-buffer)
          ("f" emacspeak-bookshare-flush-lines)
          ("v" emacspeak-bookshare-view)
          ("c" emacspeak-bookshare-toc-at-point)
          ("\C-m" emacspeak-bookshare-toc-at-point)
          ("\M-n" emacspeak-bookshare-next-result)
          ("\M-p" emacspeak-bookshare-previous-result)
          ("["  backward-page)
          ("]" forward-page)
          ("b" emacspeak-bookshare-browse)
          (" " emacspeak-bookshare-expand-at-point)
          ("U" emacspeak-bookshare-unpack-at-point)
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
  (declare (special emacspeak-bookshare-interaction-buffer))
  (let ((buffer (get-buffer emacspeak-bookshare-interaction-buffer)))
    (cond
     ((buffer-live-p buffer)
      (switch-to-buffer buffer))
     (t
      (with-current-buffer (get-buffer-create emacspeak-bookshare-interaction-buffer)
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
    (case action
      (?p (call-interactively 'emacspeak-bookshare-action))
      (?l (call-interactively 'emacspeak-bookshare-action))
      (otherwise (error "Unrecognized browse action.")))))

(defun emacspeak-bookshare-expand-at-point ()
  "Expand entry at point by retrieving metadata.
Once retrieved, memoize to avoid multiple retrievals."
  (interactive)
  (emacspeak-bookshare-assert)
  (emacspeak-auditory-icon 'select-object)
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
        (let ((new-target (read-from-minibuffer "Retry with new target:" target)))
          (if (zerop (emacspeak-bookshare-download-daisy id new-target))
              (message "Downloaded to %s" new-target)
            (error "Error downloading to %s" new-target)))))))))

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
  (declare (special emacspeak-bookshare-password-cache))
  (emacspeak-bookshare-assert)
  (let ((inhibit-read-only t)
        (target (emacspeak-bookshare-get-target))
        (author (emacspeak-bookshare-get-author))
        (title (emacspeak-bookshare-get-title))
        (directory nil))
    (when (null target) (error  "No downloaded content here."))
    (unless   (file-exists-p target) (error "First download this content."))
    (setq directory (emacspeak-bookshare-get-directory))
    (when (file-exists-p directory) (error "Already unpacked."))
    (make-directory directory 'parents)
    (shell-command
     (format "cd %s; unzip -P %s %s"
             directory
             (or emacspeak-bookshare-password-cache
                 (read-passwd
                  (format "Password for %s" emacspeak-bookshare-user-id)))
             target))
    (add-text-properties
     (line-beginning-position) (line-end-position)
     (list'face 'highlight
                'auditory-icon 'item))
    (message "Unpacked content.")))

;;;###autoload
(defcustom emacspeak-bookshare-xslt
  "daisyTransform.xsl"
  "Name of bookshare  XSL transform."
  :type '(choice :tag "Key: "
                 (const :tag "Daisy transform from Bookshare"  "daisyTransform.xsl")
                 (const :tag "Default HTML View" "default.xsl"))
  :group 'emacspeak-bookshare)

(defsubst emacspeak-bookshare-xslt (directory)
  "Return suitable XSL  transform."
  (declare (special emacspeak-bookshare-xslt
                    emacspeak-xslt-directory))
  (let ((xsl (expand-file-name emacspeak-bookshare-xslt directory)))
    (cond
     ((file-exists-p xsl) xsl)
     (t (expand-file-name emacspeak-bookshare-xslt emacspeak-xslt-directory)))))

(defvar emacspeak-bookshare-toc-xslt
  "bookshare-toc.xsl"
  "Name of bookshare supplied XSL transform.")

(defsubst emacspeak-bookshare-toc-xslt ()
  "Return suitable XSL  transform for TOC."
  (declare (special emacspeak-bookshare-toc-xslt
                    emacspeak-xslt-directory))

  (expand-file-name emacspeak-bookshare-toc-xslt emacspeak-xslt-directory))

(defun emacspeak-bookshare-view-at-point ()
  "View book at point.
Make sure it's downloaded and unpacked first."
  (interactive)
  (let* ((target (emacspeak-bookshare-get-target))
         (directory (emacspeak-bookshare-get-directory))
         (title (emacspeak-bookshare-get-title))
         (xsl (emacspeak-bookshare-xslt  directory)))
    (unless (file-exists-p target)
      (error "First download this content."))
    (unless (file-exists-p directory)
      (error "First unpack this content."))
    (emacspeak-xslt-view-file
     xsl
     (first
      (directory-files directory
                       'full
                       ".xml")))))

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
        (title (emacspeak-bookshare-get-title))
        (xsl (emacspeak-bookshare-toc-xslt)))
    (cond
     ((null target) (call-interactively 'emacspeak-bookshare-toc))
     (t
      (unless (file-exists-p target)
        (error "First download this content."))
      (unless (file-exists-p directory)
        (error "First unpack this content."))
      (add-hook
       'emacspeak-web-post-process-hook
       #'(lambda ()
           (declare (special emacspeak-we-url-executor))
           (setq emacspeak-we-url-executor 'emacspeak-bookshare-url-executor)))
      (emacspeak-xslt-view-file
       xsl
       (shell-quote-argument
        (first
         (directory-files directory 'full ".xml"))))))))

(defun emacspeak-bookshare-extract-xml (url)
  "Extract content referred to by link under point, and return an XML buffer."
  (interactive "sURL: ")
  (declare (special  emacspeak-we-xsl-filter))
  (let ((fields (split-string url "#"))
        (id nil)
        (url nil))
    (unless (= (length fields) 2)
      (error "No fragment identifier in this link."))
    (setq url (first fields)
          id (second fields))
    (emacspeak-xslt-url
     emacspeak-we-xsl-filter
     url
     (emacspeak-xslt-params-from-xpath
      (format "//*[@id=\"%s\"]" id) url))))

(defun emacspeak-bookshare-extract-and-view (url)
  "Extract content referred to by link under point, and render via the browser."
  (interactive "sURL: ")
  (declare (special emacspeak-bookshare-browser-function
                    emacspeak-xslt-directory))
  (let ((result (emacspeak-bookshare-extract-xml url))
        (browse-url-browser-function emacspeak-bookshare-browser-function))
    (save-current-buffer
      (set-buffer result)
      (emacspeak-webutils-autospeak)
      (browse-url-of-buffer ))))

(defun emacspeak-bookshare-view-page-range (url )
  "Play pages in specified page range from URL."
  (interactive "sURL:")
  (declare (special emacspeak-bookshare-browser-function))
  (let* ((start (read-from-minibuffer "Start Page: "))
         (end (read-from-minibuffer "End Page: "))
         (result
          (emacspeak-xslt-xml-url
           (expand-file-name "dtb-page-range.xsl" emacspeak-xslt-directory)
           (substring url 7)
           (list
            (cons "start" (format "'%s'" start ))
            (cons "end" (format "'%s'" end )))))
         (browse-url-browser-function emacspeak-bookshare-browser-function))
    (save-current-buffer
      (set-buffer result)
      (emacspeak-webutils-autospeak)
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
                           (when (eq major-mode 'dired-mode) (dired-get-filename))
                           emacspeak-bookshare-directory))))
  (declare (special emacspeak-bookshare-directory))
  (let* ((xsl (emacspeak-bookshare-xslt directory)))
    (emacspeak-xslt-view-file
     xsl
     (first
      (directory-files directory 'full ".xml")))))

(defun emacspeak-bookshare-toc (directory)
  "View TOC for book in specified directory."
  (interactive
   (list
    (let ((completion-ignore-case t)
          (emacspeak-speak-messages nil)
          (read-file-name-completion-ignore-case t))
      (read-directory-name "Book: "
                           (when (eq major-mode 'dired-mode) (dired-get-filename))
                           emacspeak-bookshare-directory))))
  (declare (special emacspeak-bookshare-directory))
  (let* ((xsl (emacspeak-bookshare-toc-xslt)))
    (add-hook
     'emacspeak-web-post-process-hook
     #'(lambda ()
         (declare (special emacspeak-we-url-executor))
         (setq emacspeak-we-url-executor 'emacspeak-bookshare-url-executor)))
    (emacspeak-xslt-view-file
     xsl
     (first (directory-files directory 'full ".xml")))))
;;;###autoload
(defcustom emacspeak-bookshare-html-to-text-command
  "lynx -dump -stdin"
  "Command to convert html to text on stdin."
  
  :type '(choice
          (const :tag "lynx"  "lynx -dump -stdin")
          (const "html2text" "html2text"))
  :group 'emacspeak-bookshare)
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
                               (when (eq major-mode 'dired-mode) (dired-get-filename))
                               emacspeak-bookshare-directory)))))
  (declare (special emacspeak-xslt-program))
  (declare (special emacspeak-bookshare-html-to-text-command
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
              (first (directory-files directory 'full ".xml")))
             emacspeak-bookshare-html-to-text-command))
      (erase-buffer)
      (setq buffer-undo-list t)
      (shell-command command (current-buffer) nil)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (switch-to-buffer buffer)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

;;;###autoload
(defun emacspeak-bookshare-eww (directory)
  "Render complete book using EWW if available."
  (interactive
   (list
    (or (emacspeak-bookshare-get-directory)
        (let ((completion-ignore-case t)
              (emacspeak-speak-messages nil)
              (read-file-name-completion-ignore-case t))
          (read-directory-name "Book: "
                               (when (eq major-mode 'dired-mode) (dired-get-filename))
                               emacspeak-bookshare-directory)))))
  (declare (special emacspeak-xslt-program emacspeak-bookshare-directory))
  (unless (fboundp 'eww)
    (error "Your Emacs doesn't have EWW."))
  (let ((xsl (emacspeak-bookshare-xslt directory))
        (buffer (get-buffer-create "Full Text"))
        (command nil)
        (inhibit-read-only t))
    (with-current-buffer buffer
      (setq command
            (format
             "%s  --nonet --novalid %s %s "
             emacspeak-xslt-program xsl
             (shell-quote-argument
              (first (directory-files directory 'full ".xml")))))
      (erase-buffer)
      (setq buffer-undo-list t)
      (shell-command command (current-buffer) nil)
      (eww-display-html 'utf-8 (format "file://%s" directory))
      (kill-buffer buffer))))

(defun emacspeak-bookshare-sign-out ()
  "Sign out, clearing password."
  (interactive)
  (declare (special emacspeak-bookshare-md5-cached-token
                    emacspeak-bookshare-password-cache))
  (setq emacspeak-bookshare-password-cache nil
        emacspeak-bookshare-md5-cached-token nil)
  (emacspeak-auditory-icon 'close-object)
  (message "Signed out from Bookshare."))

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
;;; byte-compile-dynamic: nil
;;; end:

;;}}}

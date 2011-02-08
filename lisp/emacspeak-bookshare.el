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
;;; This might change once I get approval from Bookshare to embed the Emacspeak API  key in the Source code.

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
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
  "Error out if not in bookshre mode."
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
     (save-excursion
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

(defsubst emacspeak-bookshare-generate-target (author title)
  "Generate a suitable filename target."
  (declare (special emacspeak-bookshare-downloads-directory))
  (expand-file-name
   (replace-regexp-in-string
    " " "-"
    (format "%s-%s.zip" author title))
   emacspeak-bookshare-downloads-directory))

(defsubst emacspeak-bookshare-generate-directory (author title)
  "Generate name of unpack directory."
  (declare (special emacspeak-bookshare-directory))
  (expand-file-name
   (replace-regexp-in-string
    " " "-"
    (format "%s/%s" author title))
   emacspeak-bookshare-directory))

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
    (read-from-minibuffer "Date: ")
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
    "%s %s %s  '%s' -o %s"
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

(define-derived-mode emacspeak-bookshare-mode text-mode
  "Bookshare Library Of Accessible Books And Periodicals"
  "A Bookshare front-end for the Emacspeak desktop.

The Emacspeak Bookshare front-end is launched by command
emacspeak-bookshare bound to \\[emacspeak-bookshare]

This command switches to a special buffer that has Bookshare
commands bounds to single keystrokes-- see the ke-binding list at
the end of this description. Use Emacs online help facility to
look up help on these commands.

emacspeak-bookshare-mode provides the necessary functionality to
Search and download Bookshare material,
Manage a local library of downloaded Bookshare content,
And commands to easily read newer Daisy books from Bookshare.
For legacy Bookshare material, see command \\[emacspeak-daisy-open-book].

Here is a list of all emacspeak Bookshare commands along with their key-bindings:
a Author Search
A Author/Title Search
t Title Search
s Full Text Search
d Date Search
b Browse

\\{emacspeak-bookshare-mode-map}"
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (insert "Browse And Read Bookshare Materials\n\n")
    (setq header-line-format "Bookshare Library")
    ))

(declaim (special emacspeak-bookshare-mode-map))

(loop for a in
      '(
        ("a" emacspeak-bookshare-author-search)
        ("t" emacspeak-bookshare-title-search)
        ("s" emacspeak-bookshare-fulltext-search)
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
      '("book" "list" "periodical")
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
           'auditory-icon icon))))

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

(loop
 for p in
 '(author title id metadata target directory)
 do
 (eval
  `(defsubst ,(intern (format "emacspeak-bookshare-get-%s" p)) ()
     ,(format "Get %s at point. " p)
     (get-text-property (point) ',p))))

;;}}}
;;{{{ Bookshare Mode:

(defun emacspeak-bookshare-define-keys ()
  "Define keys for  Bookshare Interaction."
  (declare (special emacspeak-bookshare-mode-map))
  (loop for k in
        '(
          ("q" bury-buffer)
          ("\M-n" emacspeak-bookshare-next-result)
          ("\M-p" emacspeak-bookshare-previous-result)
          ("["  backward-page)
          ("]" forward-page)
          ("b" emacspeak-bookshare-browse)
          (" " emacspeak-bookshare-expand-at-point)
          ("U" emacspeak-bookshare-unpack-at-point)
          ("V" emacspeak-bookshare-view-at-point)
          ("D" emacspeak-bookshare-download-daisy-at-point)
          ("B" emacspeak-bookshare-download-brf-at-point)
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
        (emacspeak-bookshare-mode)
        (emacspeak-toggle-audio-indentation t))
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
       (t (error "Error downloading content.")))))))

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
(defvar emacspeak-bookshare-xslt "daisyTransform.xsl"
  "Name of bookshare supplied XSL transform.")

(defun emacspeak-bookshare-view-at-point ()
  "View book at point.
Make sure it's downloaded and unpacked first."
  (interactive)
  (declare (special emacspeak-bookshare-xslt))
  (let* ((target (emacspeak-bookshare-get-target))
         (directory (emacspeak-bookshare-get-directory))
         (title (emacspeak-bookshare-get-title))
         (xsl (expand-file-name emacspeak-bookshare-xslt directory)))
    (unless (file-exists-p target)
      (error "First download this content."))
    (unless (file-exists-p directory)
      (error "First unpack this content."))
    (unless (file-exists-p xsl)
      (error "No suitable XSL  transformation found."))
    (emacspeak-xslt-view-file
     xsl
     (first
      (directory-files directory
                       'full
                       ".xml")))))

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

;;}}}
(provide 'emacspeak-bookshare)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

;;; emacspeak-webedit.el --- Transform Web Pages Using XSLT
;;; $Id: emacspeak-webmarks.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Edit/Transform Web Pages using XSLT
;;; Keywords: Emacspeak,  Audio Desktop Web, XSLT
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
;;;Copyright (C) 1995 -- 2007, T. V. Raman
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:

;;; Invoke XSLT to edit/transform Web pages before they get rendered.

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))

(require 'emacspeak-preamble)
(require 'emacspeak-webutils)

;;}}}
;;{{{ applying XSL transforms before displaying

(define-prefix-command 'emacspeak-webedit-xsl-map )

;;;###autoload
(defcustom emacspeak-webedit-xsl-p nil
  "T means we apply XSL transformation before displaying
HTML."
  :type 'boolean
  :group 'emacspeak-webedit)

;;;###autoload
(defcustom emacspeak-webedit-xsl-transform nil
  "Specifies transform to use before displaying a page.
Nil means no transform is used. "
  :type  '(choice
           (file :tag "XSL")
           (const :tag "none" nil))
  :group 'emacspeak-webedit)

(defvar emacspeak-webedit-xsl-params nil
  "XSL params if any to pass to emacspeak-xslt-region.")

;;; Note that emacspeak-webedit-xsl-transform, emacspeak-webedit-xsl-params
;;; and emacspeak-webedit-xsl-p
;;; need to be set at top-levle since the page-rendering code is
;;; called asynchronously.

;;;###autoload
(defcustom emacspeak-webedit-cleanup-bogus-quotes t
  "Clean up bogus Unicode chars for magic quotes."
  :type 'boolean
  :group 'emacspeak-webedit)

(defadvice  w3-parse-buffer (before emacspeak pre act comp)
  "Apply requested XSL transform if any before displaying the
HTML."
  (when emacspeak-webedit-cleanup-bogus-quotes
    (goto-char (point-min))
    (while (search-forward "&\#147\;" nil t)
      (replace-match "\""))
    (goto-char (point-min))
    (while (search-forward "&\#148\;" nil t)
      (replace-match "\""))
    (goto-char (point-min))
    (while (search-forward "&\#180\;" nil t)
      (replace-match "\'")))
  (when (and emacspeak-webedit-xsl-p emacspeak-webedit-xsl-transform)
    (emacspeak-xslt-region
     emacspeak-webedit-xsl-transform
     (point-min)
     (point-max)
     emacspeak-webedit-xsl-params)))

(declaim (special emacspeak-xslt-directory))
;;;###autoload
(defun emacspeak-webedit-xslt-apply (xsl)
  "Apply specified transformation to current page."
  (interactive
   (list
    (expand-file-name
     (read-file-name "XSL Transformation: "
                     emacspeak-xslt-directory))))
  (declare (special major-mode
                    emacspeak-xslt-directory))
  (unless (eq major-mode 'w3-mode)
    (error "Not in a W3 buffer."))
  (let ((url (url-view-url t)))
    (emacspeak-webutils-with-xsl
     xsl
     (browse-url url))))


;;;###autoload
(defun emacspeak-webedit-xslt-select (xsl)
  "Select XSL transformation applied to WWW pages before they are displayed ."
  (interactive
   (list
    (expand-file-name
     (read-file-name "XSL Transformation: "
                     emacspeak-xslt-directory))))
  (declare (special emacspeak-webedit-xsl-transform))
  (setq emacspeak-webedit-xsl-transform xsl)
  (message "Will apply %s before displaying HTML pages."
           (file-name-sans-extension
            (file-name-nondirectory
             xsl)))
  (emacspeak-auditory-icon 'select-object))

;;;###autoload
(defun emacspeak-webedit-xsl-toggle ()
  "Toggle  application of XSL transformations.
This uses XSLT Processor xsltproc available as part of the
libxslt package."
  (interactive)
  (declare (special emacspeak-webedit-xsl-p))
  (setq emacspeak-webedit-xsl-p
        (not emacspeak-webedit-xsl-p))
  (emacspeak-auditory-icon
   (if emacspeak-webedit-xsl-p 'on 'off))
  (message "Turned %s XSL"
           (if emacspeak-webedit-xsl-p 'on 'off)))

;;;###autoload
(defun emacspeak-webedit-count-matches (prompt-url locator)
  "Count matches for locator  in HTML."
  (interactive
   (list
    (if (eq major-mode 'w3-mode)
        (url-view-url 'no-show)
      (read-from-minibuffer "URL: "))
    (read-from-minibuffer "XPath locator: ")))
  (read
   (emacspeak-xslt-url
    (expand-file-name "count-matches.xsl"
                      emacspeak-xslt-directory)
    prompt-url
    (list
     (cons "locator"
           (format "'%s'"
                   locator ))))))

;;;###autoload
(defun emacspeak-webedit-count-nested-tables (prompt-url)
  "Count nested tables in HTML."
  (interactive
   (list
    (if (eq major-mode 'w3-mode)
        (url-view-url 'no-show)
      (read-from-minibuffer "URL: "))))
  (emacspeak-webedit-count-matches
   prompt-url
   "'//table//table'" ))

;;;###autoload
(defun emacspeak-webedit-count-tables (prompt-url)
  "Count  tables in HTML."
  (interactive
   (list
    (if (eq major-mode 'w3-mode)
        (url-view-url 'no-show)
      (read-from-minibuffer "URL: "))))
  (emacspeak-webedit-count-matches
   prompt-url
   "//table"))
;;;###autoload

(defcustom emacspeak-webedit-xsl-keep-result ""
  "Set to a non-empty string  if you want the buffer containing the transformed HTML
source to be preserved.
Value of this variable if non-empty will be used as a name for the
source buffer."
  :type 'string
  :group 'emacspeak-webedit)

;;;###autoload
(defun emacspeak-webedit-set-xsl-keep-result (value)
  "Set value of `emacspeak-webedit-xsl-keep-result'."
  (interactive  "sEnter name of result buffer: ")
  (declare (special emacspeak-webedit-xsl-keep-result))
  (setq emacspeak-webedit-xsl-keep-result value))

;;;###autoload
(defun emacspeak-webedit-xslt-filter (path    url  &optional speak complement)
  "Extract elements matching specified XPath path locator
from Web page -- default is the current page being viewed.
Optional arg COMPLEMENT inverts the filter.  "
  (interactive
   (list
    (read-from-minibuffer "XPath: ")
    (if (eq major-mode 'w3-mode)
        (url-view-url t)
      (read-from-minibuffer "URL: " "http://www."))
    current-prefix-arg
    current-prefix-arg))
  (declare (special emacspeak-webedit-xsl-filter emacspeak-webedit-xsl-junk))
  (unless (or url
              (interactive-p)
              (eq major-mode 'w3-mode))
    (error "Not in a W3 buffer."))
  (let ((style (if complement
                   emacspeak-webedit-xsl-junk
                 emacspeak-webedit-xsl-filter))
        (params (emacspeak-xslt-params-from-xpath  path url)))
    (when speak
      (add-hook 'emacspeak-w3-post-process-hook
                'emacspeak-speak-buffer))
    (emacspeak-webutils-with-xsl-environment
     style params
     (browse-url url))))



(defun emacspeak-webedit-xslt-junk (path   &optional prompt-url speak)
  "Junk elements matching specified locator."
  (interactive
   (list
    (read-from-minibuffer "XPath: ")
    current-prefix-arg))
  (emacspeak-webedit-xslt-filter path prompt-url speak 'complement))

(defcustom emacspeak-webedit-media-stream-suffixes
  (list
   ".ram"
   ".rm"
   ".ra"
   ".pls"
   ".asf"
   ".asx"
   ".mp3"
   ".m3u"
   ".m4v"
   ".wma"
   ".wmv"
   ".avi"
   ".mpg")
  "Suffixes to look for in detecting URLs that point to media
streams."
  :type  '(repeat
           (string :tag "Extension Suffix"))
  :group 'emacspeak-webedit)
;;;###autoload
(defun emacspeak-webedit-extract-media-streams ( &optional prompt-url speak)
  "Extract links to media streams.
operate on current web page when in a W3 buffer; otherwise prompt for url.
`prompt-url' is the URL to process. Prompts for URL when called
interactively. Optional arg `speak' specifies if the result should be
spoken automatically."
  (interactive
   (list current-prefix-arg))
  (unless
      (or
       (eq major-mode 'w3-mode)
       (stringp prompt-url))
    (setq prompt-url
          (read-from-minibuffer "URL:")))
  (declare (special emacspeak-webedit-media-stream-suffixes))
  (let ((filter "//a[%s]")
        (predicate
         (mapconcat
          #'(lambda (suffix)
              (format "contains(@href,\"%s\")"
                      suffix))
          emacspeak-webedit-media-stream-suffixes
          " or ")))
    (emacspeak-webedit-xslt-filter
     (format filter predicate )
     prompt-url
     (or (interactive-p)
         speak))))

;;;###autoload
(defun emacspeak-webedit-extract-print-streams ( &optional prompt-url speak)
  "Extract links to printable  streams.
operate on current web page when in a W3 buffer; otherwise prompt for url.
`prompt-url' is the URL to process. Prompts for URL when called
interactively. Optional arg `speak' specifies if the result should be
spoken automatically."
  (interactive
   (list current-prefix-arg))
  (unless (and
           (or (null prompt-url) (stringp prompt-url))
           (eq major-mode 'w3-mode))
    (setq prompt-url
          (read-from-minibuffer "URL:")))
  (let ((filter "//a[contains(@href,\"print\")]"))
    (emacspeak-webedit-xslt-filter
     filter
     prompt-url
     (or (interactive-p)
         speak))))

(defun emacspeak-webedit-extract-media-streams-under-point ()
  "In W3 mode buffers, extract media streams from url under point."
  (interactive)
  (cond
   ((and (eq major-mode 'w3-mode)
         (w3-view-this-url 'no-show))
    (emacspeak-webedit-extract-media-streams (w3-view-this-url 'no-show)
                                        'speak))
   (t (error "Not on a link in a W3 buffer."))))

(defun emacspeak-webedit-extract-matching-urls (pattern  &optional prompt-url speak)
  "Extracts links whose URL matches pattern."
  (interactive
   (list
    (read-from-minibuffer "URL Pattern: ")
    current-prefix-arg))
  (let ((filter
         (format
          "//a[contains(@href,\"%s\")]"
          pattern)))
    (emacspeak-webedit-xslt-filter
     filter
     prompt-url
     (or (interactive-p)
         speak))))

;;;###autoload
(defun emacspeak-webedit-extract-nested-table (table-index   &optional prompt-url speak)
  "Extract nested table specified by `table-index'. Default is to
operate on current web page when in a W3 buffer; otherwise
`prompt-url' is the URL to process. Prompts for URL when called
interactively. Optional arg `speak' specifies if the result should be
spoken automatically."

  (interactive
   (list
    (read-from-minibuffer "Table index: ")
    current-prefix-arg))
  (emacspeak-webedit-xslt-filter
   (format "(//table//table)[%s]" table-index)
   prompt-url
   speak))

(defsubst  emacspeak-webedit-get-table-list (&optional bound)
  "Collect a list of numbers less than bound
 by prompting repeatedly in the
minibuffer.
Empty value finishes the list."
  (let ((result nil)
        (i nil)
        (done nil))
    (while (not done)
      (setq i
            (read-from-minibuffer
             (format "Index%s"
                     (if bound
                         (format " less than  %s" bound)
                       ":"))))
      (if (> (length i) 0)
          (push i result)
        (setq done t)))
    result))

(defsubst  emacspeak-webedit-get-table-match-list ()
  "Collect a list of matches by prompting repeatedly in the
minibuffer.
Empty value finishes the list."
  (let ((result nil)
        (i nil)
        (done nil))
    (while (not done)
      (setq i
            (read-from-minibuffer "Match: "))
      (if (> (length i) 0)
          (push i result)
        (setq done t)))
    result))

;;;###autoload
(defun emacspeak-webedit-extract-nested-table-list (tables   &optional prompt-url speak)
  "Extract specified list of tables from a WWW page."
  (interactive
   (list
    (emacspeak-webedit-get-table-list)
    current-prefix-arg))
  (let ((filter nil))
    (setq filter
          (mapconcat
           #'(lambda  (i)
               (format "((//table//table)[%s])" i))
           tables
           " | "))
    (emacspeak-webedit-xslt-filter
     filter
     prompt-url
     (or (interactive-p) speak))))
;;;###autoload
(defun emacspeak-webedit-extract-table-by-position (position   &optional prompt-url speak)
  "Extract table at specified position.
 Optional arg url specifies the page to extract content from.
Interactive prefix arg causes url to be read from the minibuffer."
  (interactive
   (list
    (read-from-minibuffer
     "Table position: ")
    current-prefix-arg))
  (emacspeak-webedit-xslt-filter
   (format "/descendant::table[%s]"
           position)
   prompt-url
   (or (interactive-p)
       speak)))

;;;###autoload
(defun emacspeak-webedit-extract-tables-by-position-list (positions   &optional prompt-url speak)
  "Extract specified list of nested tables from a WWW page.
Tables are specified by their position in the list
nested of tables found in the page."
  (interactive
   (list
    (emacspeak-webedit-get-table-list)
    current-prefix-arg))
  (let ((filter nil))
    (setq filter
          (mapconcat
           #'(lambda  (i)
               (format "(/descendant::table[%s])" i))
           positions
           " | "))
    (emacspeak-webedit-xslt-filter
     filter
     prompt-url
     (or (interactive-p) speak))))

;;;###autoload
(defun emacspeak-webedit-extract-table-by-match (match   &optional prompt-url speak)
  "Extract table containing  specified match.
 Optional arg url specifies the page to extract content from.
Interactive prefix arg causes url to be read from the minibuffer."
  (interactive
   (list
    (read-from-minibuffer
     "Tables matching: ")
    current-prefix-arg))
  (emacspeak-webedit-xslt-filter
   (format "(/descendant::table[contains(., \"%s\")])[last()]"
           match)
   prompt-url
   (or (interactive-p)
       speak)))

;;;###autoload
(defun emacspeak-webedit-extract-tables-by-match-list (match-list   &optional prompt-url speak)
  "Extract specified  tables from a WWW page.
Tables are specified by containing  match pattern
 found in the match list."
  (interactive
   (list
    (emacspeak-webedit-get-table-match-list)
    current-prefix-arg))
  (let ((filter nil))
    (setq filter
          (mapconcat
           #'(lambda  (i)
               (format "((/descendant::table[contains(.,\"%s\")])[last()])" i))
           match-list
           " | "))
    (emacspeak-webedit-xslt-filter
     filter
     prompt-url
     (or (interactive-p) speak))))

(defvar emacspeak-webedit-buffer-css-class-cache nil
  "Caches class attribute values for current buffer.")

(make-variable-buffer-local 'emacspeak-webedit-buffer-css-class-cache)

(defun emacspeak-webedit-css-class-cache ()
  "Build CSS class cache for buffer if needed."
  (unless (eq major-mode 'w3-mode)
    (error "Not in W3 buffer."))
  (or emacspeak-webedit-buffer-css-class-cache
      (let ((values nil)
            (buffer
             (emacspeak-xslt-url
              (expand-file-name "class-values.xsl"
                                emacspeak-xslt-directory)
              (url-view-url 'no-show)
              nil
              'no-comment)))
        (setq values
              (save-excursion
                (set-buffer buffer)
                (shell-command-on-region (point-min) (point-max)
                                         "sort  -u"
                                         (current-buffer))
                (split-string (buffer-string))))
        (setq emacspeak-webedit-buffer-css-class-cache
              (mapcar
               #'(lambda (v)
                   (cons v v ))
               values)))))

(make-variable-buffer-local 'emacspeak-webedit-buffer-id-cache)

(defun emacspeak-webedit-id-cache ()
  "Build CSS class cache for buffer if needed."
  (declare (special emacspeak-webedit-buffer-id-cache))
  (unless (eq major-mode 'w3-mode)
    (error "Not in W3 buffer."))
  (or emacspeak-webedit-buffer-id-cache
      (let ((values nil)
            (buffer
             (emacspeak-xslt-url
              (expand-file-name "class-values.xsl"
                                emacspeak-xslt-directory)
              (url-view-url 'no-show)
              nil
              'no-comment)))
        (setq values
              (save-excursion
                (set-buffer buffer)
                (shell-command-on-region (point-min) (point-max)
                                         "sort  -u"
                                         (current-buffer))
                (split-string (buffer-string))))
        (setq emacspeak-webedit-buffer-id-cache
              (mapcar
               #'(lambda (v)
                   (cons v v ))
               values)))))

;;;###autoload
(defun emacspeak-webedit-extract-by-class (class    url &optional speak)
  "Extract elements having specified class attribute from HTML. Extracts
specified elements from current WWW page and displays it in a separate
buffer. Interactive use provides list of class values as completion."
  (interactive
   (list
    (completing-read "Class: "
                     (emacspeak-webedit-css-class-cache))
    (if (eq major-mode 'w3-mode)
        (url-view-url t)
      (read-from-minibuffer "URL: " "http://www."))
    (or (interactive-p) current-prefix-arg)))
  (message "url: %s filter: %s"
           url class)
  (let ((filter (format "//*[@class=\"%s\"]" class)))
    (message "filter:%s" filter)
  (emacspeak-webedit-xslt-filter filter
   url
   'speak)))

(defsubst  emacspeak-webedit-get-id-list ()
  "Collect a list of ids by prompting repeatedly in the
minibuffer.
Empty value finishes the list."
  (let ((ids (emacspeak-webedit-id-cache))
        (result nil)
        (c nil)
        (done nil))
    (while (not done)
      (setq c
            (completing-read "Id: "
                             ids
                             nil 'must-match))
      (if (> (length c) 0)
          (push c result)
        (setq done t)))
    result))

(defsubst  emacspeak-webedit-css-get-class-list ()
  "Collect a list of classes by prompting repeatedly in the
minibuffer.
Empty value finishes the list."
  (let ((classes (emacspeak-webedit-css-class-cache))
        (result nil)
        (c nil)
        (done nil))
    (while (not done)
      (setq c
            (completing-read "Class: "
                             classes
                             nil 'must-match))
      (if (> (length c) 0)
          (push c result)
        (setq done t)))
    result))
;;;###autoload
(defun emacspeak-webedit-extract-by-class-list(classes   &optional prompt-url speak)
  "Extract elements having class specified in list `classes' from HTML.
Extracts specified elements from current WWW page and displays it in a
separate buffer. Optional arg url specifies the page to extract
content from. Interactive use provides list of class values as
completion. "
  (interactive
   (list
    (emacspeak-webedit-css-get-class-list)
    current-prefix-arg))
  (let ((filter nil))
    (setq filter
          (mapconcat
           #'(lambda  (c)
               (format "(@class=\"%s\")" c))
           classes
           " or "))
    (emacspeak-webedit-xslt-filter
     (format "//*[%s]" filter)
     prompt-url
     (or (interactive-p) speak))))
(defvar emacspeak-webedit-buffer-id-cache nil
  "Caches id attribute values for current buffer.")

(make-variable-buffer-local 'emacspeak-webedit-buffer-id-cache)

;;;###autoload
(defun emacspeak-webedit-extract-by-id (id   &optional prompt-url speak)
  "Extract elements having specified id attribute from HTML. Extracts
specified elements from current WWW page and displays it in a separate
buffer. Optional arg url specifies the page to extract content from.
Interactive use provides list of id values as completion."
  (interactive
   (list
    (completing-read "Id: "
                     (emacspeak-webedit-id-cache))
    current-prefix-arg))
  (emacspeak-webedit-xslt-filter
   (format "//*[@id=\"%s\"]"
           id)
   prompt-url
   (or (interactive-p)
       speak)))

;;;###autoload
(defun emacspeak-webedit-extract-by-id-list(ids   &optional prompt-url speak)
  "Extract elements having id specified in list `ids' from HTML.
Extracts specified elements from current WWW page and displays it in a
separate buffer. Optional arg url specifies the page to extract
content from. Interactive use provids list of id values as
completion. "
  (interactive
   (list
    (emacspeak-webedit-css-get-id-list)
    current-prefix-arg))
  (let ((filter nil))
    (setq filter
          (mapconcat
           #'(lambda  (c)
               (format "(@id=\"%s\")" c))
           ids
           " or "))
    (emacspeak-webedit-xslt-filter
     (format "//*[%s]" filter)
     prompt-url
     (or (interactive-p) speak))))

(defun emacspeak-webedit-junk-by-class-list(classes   &optional prompt-url speak)
  "Junk elements having class specified in list `classes' from HTML.
Extracts specified elements from current WWW page and displays it in a
separate buffer. Optional arg url specifies the page to extract
content from. Interactive use provides list of class values as
completion. "
  (interactive
   (list
    (emacspeak-webedit-css-get-class-list)
    current-prefix-arg))
  (let ((filter nil))
    (setq filter
          (mapconcat
           #'(lambda  (c)
               (format "(@class=\"%s\")" c))
           classes
           " or "))
    (emacspeak-webedit-xslt-junk
     (format "//*[%s]" filter)
     prompt-url
     (or (interactive-p) speak))))

(defvar emacspeak-webedit-xsl-filter
  (expand-file-name "xpath-filter.xsl"
                    emacspeak-xslt-directory)
  "XSL transform to extract  elements matching a specified
XPath locator.")
(defvar emacspeak-webedit-xsl-junk
  (expand-file-name "xpath-junk.xsl"
                    emacspeak-xslt-directory)
  "XSL transform to junk  elements matching a specified
XPath locator.")

;;; Extracting node specified by id
(defvar emacspeak-webedit-extract-node-by-id-xsl
  (expand-file-name "extract-node-by-id.xsl"
                    emacspeak-xslt-directory)
  "XSL transform to extract a node.")
;;;###autoload
(defun emacspeak-webedit-extract-node-by-id (url node-id   )
  "Extract specified node from URI."
  (interactive
   (list
    (read-from-minibuffer "URL: ")
    (read-from-minibuffer "Node Id: ")))
  (declare (special emacspeak-xslt-program
                    emacspeak-webedit-extract-node-by-id-xsl))
  (let ((result
         (emacspeak-xslt-url
          emacspeak-webedit-extract-node-by-id-xsl
          url
          (list
           (cons "node-id"
                 (format "\"'%s'\"" node-id))
           (cons "base"
                 (format "\"'%s'\"" url))))))
    (save-excursion
      (set-buffer  result)
      (browse-url-of-buffer))
    (kill-buffer result)))

;;}}}
;;{{{  xsl keymap

(declaim (special emacspeak-webedit-xsl-map))

(loop for binding in
      '(
        ("C" emacspeak-webedit-extract-by-class-list)
        ("M" emacspeak-webedit-extract-tables-by-match-list)
        ("P" emacspeak-webedit-extract-print-streams)
        ("R" emacspeak-webedit-extract-media-streams-under-point)
        ("T" emacspeak-webedit-extract-tables-by-position-list)
        ("X" emacspeak-webedit-extract-nested-table-list)
        ("\C-c" emacspeak-webedit-junk-by-class-list)
        ("\C-f" emacspeak-webedit-count-matches)
        ("\C-p" emacspeak-webedit-xpath-junk-and-follow)
        ("\C-t" emacspeak-webedit-count-tables)
        ("\C-x" emacspeak-webedit-count-nested-tables)
        ("a" emacspeak-webedit-xslt-apply)
        ("c" emacspeak-webedit-extract-by-class)
        ("e" emacspeak-webedit-url-expand-and-execute)
        ("f" emacspeak-webedit-xslt-filter)
        ("i" emacspeak-webedit-extract-by-id)
        ("I" emacspeak-webedit-extract-by-id-list)
        ("j" emacspeak-webedit-xslt-junk)
        ("k" emacspeak-webedit-set-xsl-keep-result)
        ("m" emacspeak-webedit-extract-table-by-match)
        ("o" emacspeak-webedit-xsl-toggle)
        ("p" emacspeak-webedit-xpath-filter-and-follow)
        ("r" emacspeak-webedit-extract-media-streams)
        ("S" emacspeak-webedit-style-filter)
        ("s" emacspeak-webedit-xslt-select)
        ("t" emacspeak-webedit-extract-table-by-position)
        ("u" emacspeak-webedit-extract-matching-urls)
        ("x" emacspeak-webedit-extract-nested-table)
        ("y" emacspeak-webedit-class-filter-and-follow)
        )
      do
      (emacspeak-keymap-update emacspeak-webedit-xsl-map binding))

;;}}}
;;{{{ class filter

(defvar emacspeak-webedit-class-filter nil
  "Buffer local variable specifying a class filter for following
urls.")

(make-variable-buffer-local 'emacspeak-webedit-class-filter)
;;;###autoload
(defun emacspeak-webedit-class-filter-and-follow (&optional prompt-class)
  "Follow url and point, and filter the result by specified class.
Class can be set locally for a buffer, and overridden with an
interactive prefix arg. If there is a known rewrite url rule, that is
used as well."
  (interactive "P")
  (declare (special emacspeak-webedit-class-filter
                    emacspeak-webedit-url-rewrite-rule))
  (unless (eq major-mode 'w3-mode)
    (error "This command is only useful in W3 buffers."))
  (let ((url (w3-view-this-url t))
        (redirect nil))
    (unless url
      (error "Not on a link."))
    (when emacspeak-webedit-url-rewrite-rule
      (setq redirect
            (replace-regexp-in-string
             (first emacspeak-webedit-url-rewrite-rule)
             (second emacspeak-webedit-url-rewrite-rule)
             url)))
    (when (or prompt-class
              (null emacspeak-webedit-class-filter))
      (setq emacspeak-webedit-class-filter
            (read-from-minibuffer  "Specify class: ")))
    (emacspeak-webedit-extract-by-class
     emacspeak-webedit-class-filter
     (or redirect url)
     'speak)
    (emacspeak-auditory-icon 'open-object)))

;;}}}
;;{{{ style filter
(defun emacspeak-webedit-style-filter (style   &optional prompt-url speak )
  "Extract elements matching specified style
from HTML.  Extracts specified elements from current WWW
page and displays it in a separate buffer.  Optional arg url
specifies the page to extract contents  from."
  (interactive
   (list
    (read-from-minibuffer "Style: ")
    current-prefix-arg))
  (emacspeak-webedit-xslt-filter
   (format "//*[contains(@style,  \"%s\")]" style)
   prompt-url speak))

;;}}}
;;{{{ xpath  filter

(defvar emacspeak-webedit-xpath-filter nil
  "Buffer local variable specifying a XPath filter for following
urls.")

(make-variable-buffer-local 'emacspeak-webedit-xpath-filter)
(defcustom emacspeak-webedit-most-recent-xpath-filter
  "//p|ol|ul|dl|h1|h2|h3|h4|h5|h6|blockquote|div"
  "Caches most recently used xpath filter.
Can be customized to set up initial default."
  :type 'string
  :group 'emacspeak-webedit)
;;;###autoload
(defun emacspeak-webedit-xpath-filter-and-follow (&optional prompt)
  "Follow url and point, and filter the result by specified xpath.
XPath can be set locally for a buffer, and overridden with an
interactive prefix arg. If there is a known rewrite url rule, that is
used as well."
  (interactive "P")
  (declare (special emacspeak-webedit-xpath-filter
                    emacspeak-webedit-most-recent-xpath-filter
                    emacspeak-webedit-url-rewrite-rule))
  (unless (eq major-mode 'w3-mode)
    (error "This command is only useful in W3 buffers."))
  (let ((url (w3-view-this-url t))
        (redirect nil))
    (unless url (error "Not on a link."))
    (when emacspeak-webedit-url-rewrite-rule
      (setq redirect
            (replace-regexp-in-string
             (first emacspeak-webedit-url-rewrite-rule)
             (second emacspeak-webedit-url-rewrite-rule)
             url)))
    (when (or prompt (null emacspeak-webedit-xpath-filter))
      (setq emacspeak-webedit-xpath-filter
            (read-from-minibuffer  "Specify XPath: "
                                   emacspeak-webedit-most-recent-xpath-filter))
      (setq emacspeak-webedit-most-recent-xpath-filter
            emacspeak-webedit-xpath-filter))
    (emacspeak-webedit-xslt-filter emacspeak-webedit-xpath-filter
                              (or redirect url)
                              'speak)))

(defvar emacspeak-webedit-xpath-junk nil
  "Records XPath pattern used to junk elements.")

(make-variable-buffer-local 'emacspeak-webedit-xpath-junk)

(defvar emacspeak-webedit-most-recent-xpath-junk
  nil
  "Caches last XPath used to junk elements.")

(defun emacspeak-webedit-xpath-junk-and-follow (&optional prompt)
  "Follow url and point, and filter the result by junking
elements specified xpath.
XPath can be set locally for a buffer, and overridden with an
interactive prefix arg. If there is a known rewrite url rule, that is
used as well."
  (interactive "P")
  (declare (special emacspeak-webedit-xpath-junk
                    emacspeak-webedit-most-recent-xpath-junk
                    emacspeak-webedit-url-rewrite-rule))
  (unless (eq major-mode 'w3-mode)
    (error "This command is only useful in W3 buffers."))
  (let ((url (w3-view-this-url t))
        (redirect nil))
    (unless url
      (error "Not on a link."))
    (when emacspeak-webedit-url-rewrite-rule
      (setq redirect
            (replace-regexp-in-string
             (first emacspeak-webedit-url-rewrite-rule)
             (second emacspeak-webedit-url-rewrite-rule)
             url)))
    (when (or prompt
              (null emacspeak-webedit-xpath-junk))
      (setq emacspeak-webedit-xpath-junk
            (read-from-minibuffer  "Specify XPath: "
                                   emacspeak-webedit-most-recent-xpath-junk))
      (setq emacspeak-webedit-most-recent-xpath-junk
            emacspeak-webedit-xpath-junk))
    (emacspeak-webedit-xslt-filter emacspeak-webedit-xpath-junk
                              (or redirect url)
                              'speak
                              'complement)
    (emacspeak-auditory-icon 'open-object)))

;;}}}
;;{{{  browse url using specified style

;;;###autoload
(defun emacspeak-webedit-browse-url-with-style (style url)
  "Browse URL with specified XSL style."
  (interactive
   (list
    (expand-file-name
     (read-file-name "XSL Transformation: "
                     emacspeak-xslt-directory))
    (read-string "URL: " (browse-url-url-at-point))))
  (declare (special emacspeak-w3-post-process-hook
                    emacspeak-webedit-xsl-p
                    emacspeak-webedit-xsl-transform))
  (let ((emacspeak-webedit-xsl-p t)
        (emacspeak-webedit-xsl-transform style)
        (src-buffer
         (emacspeak-xslt-url
          style
          url
          (list
           (cons "base"
                 (format "\"'%s'\""
                         url))))))
    (add-hook 'emacspeak-w3-post-process-hook
              #'(lambda nil
                  (emacspeak-speak-mode-line)
                  (emacspeak-auditory-icon 'open-object)))
    (save-excursion
      (set-buffer src-buffer)
      (browse-url-of-buffer))
    (kill-buffer src-buffer)))

(defcustom emacspeak-webedit-charent-alist
  '(("&lt;" . "<")
    ("&gt;" . ">")
    ("&quot;" . "\"")
    ("&apos;" . "'")
    ("&amp;" . "&"))
  "Entities to unescape when treating badly escaped XML."
  :type '(repeat  :tag "Char Entity"
                  (cons :tag "Entry"
                        (string :tag "CharEnt")
                        (string :tag "Replacement")))
  :group 'emacspeak-webedit)

(defsubst emacspeak-webedit-unescape-charent ()
  "Clean up bad XML usage."
  (declare (special emacspeak-webedit-charent-alist))
  (loop for entry in emacspeak-webedit-charent-alist
        do
        (let ((entity (car  entry))
              (replacement (cdr entry )))
          (goto-char (point-min))
          (while (search-forward entity nil t)
            (replace-match replacement )))))

;;;###autoload
(defun emacspeak-webedit-browse-xml-url-with-style (style url &optional unescape-charent)
  "Browse XML URL with specified XSL style."
  (interactive
   (list
    (expand-file-name
     (read-file-name "XSL Transformation: "
                     emacspeak-xslt-directory))
    (read-string "URL: " (browse-url-url-at-point))))
  (declare (special emacspeak-w3-post-process-hook))
  (let ((src-buffer
         (emacspeak-xslt-xml-url
          style
          url
          (list
           (cons "base"
                 (format "\"'%s'\""
                         url))))))
    (add-hook 'emacspeak-w3-post-process-hook
              #'(lambda nil
                  (emacspeak-speak-mode-line)
                  (emacspeak-auditory-icon 'open-object)))
    (save-excursion
      (set-buffer src-buffer)
      (when unescape-charent
        (emacspeak-webedit-unescape-charent))
      (emacspeak-webutils-without-xsl
       (browse-url-of-buffer)))
    (kill-buffer src-buffer)))

;;}}}
(provide 'emacspeak-webedit)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

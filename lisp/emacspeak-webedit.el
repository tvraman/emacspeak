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
;;; Code:
;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'emacspeak-xslt)
(require 'emacspeak-webutils)

;;}}}
;;{{{ Helpers:

(defsubst emacspeak-webedit-read-url ()
  "Return URL of current page,
or URL read from minibuffer."
  (if (or (eq major-mode 'w3-mode)
          (eq major-mode 'w3m-mode))
      (funcall emacspeak-webutils-current-url)
    (read-from-minibuffer "URL: "
                          (or (browse-url-url-at-point)
                              "http://"))))

;;}}}
;;{{{ applying XSL transforms before displaying

(define-prefix-command 'emacspeak-webedit-xsl-map )

(defvar emacspeak-webedit-xsl-filter
  (emacspeak-xslt-get "xpath-filter.xsl")
  "XSL to extract  elements matching a specified XPath locator.")


(defvar emacspeak-webedit-xsl-junk
  (emacspeak-xslt-get "xpath-junk.xsl")
  "XSL to junk  elements matching a specified XPath locator.")

;;;###autoload
(defcustom emacspeak-webedit-xsl-p nil
  "T means we apply XSL before displaying HTML."
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

;;;###autoload
(defvar emacspeak-webedit-xsl-params nil
  "XSL params if any to pass to emacspeak-xslt-region.")

;;; Note that emacspeak-webedit-xsl-transform, emacspeak-webedit-xsl-params
;;; and emacspeak-webedit-xsl-p
;;; need to be set at top-levle since the page-rendering code is
;;; called asynchronously.

;;;###autoload
(defvar emacspeak-webedit-unescape-charent nil
  "Set to T to unescape charents.")
;;{{{ Configurators:

;;;###autoload
(defun emacspeak-webedit-xslt-apply (xsl)
  "Apply specified transformation to current page."
  (interactive
   (list
    (expand-file-name
     (read-file-name "XSL Transformation: "
                     emacspeak-xslt-directory))))
  (declare (special major-mode emacspeak-xslt-directory))
  (unless (or (eq major-mode 'w3-mode)
              (eq major-mode 'w3m-mode))
    (error "Not in a Web buffer."))
  (let ((url (funcall emacspeak-webutils-url-at-point)))
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
            (file-name-nondirectory xsl)))
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

;;}}}
;;{{{  Count Commands:

;;;###autoload
(defun emacspeak-webedit-count-matches (url locator)
  "Count matches for locator  in HTML."
  (interactive
   (list
    (if (eq major-mode 'w3-mode)
        (url-view-url 'no-show)
      (read-from-minibuffer "URL: "))
    (read-from-minibuffer "XPath locator: ")))
  (read
   (emacspeak-xslt-url
    (emacspeak-xslt-get "count-matches.xsl")
    url
    (list
     (cons "locator"
           (format "'%s'"
                   locator ))))))

;;;###autoload
(defun emacspeak-webedit-count-nested-tables (url)
  "Count nested tables in HTML."
  (interactive
   (list
    (if (eq major-mode 'w3-mode)
        (url-view-url 'no-show)
      (read-from-minibuffer "URL: "))))
  (emacspeak-webedit-count-matches url "'//table//table'" ))

;;;###autoload
(defun emacspeak-webedit-count-tables (url)
  "Count  tables in HTML."
  (interactive
   (list
    (if (eq major-mode 'w3-mode)
        (url-view-url 'no-show)
      (read-from-minibuffer "URL: "))))
  (emacspeak-webedit-count-matches url "//table"))
;;}}}

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

;;;  Helper: rename result buffer
(defsubst emacspeak-webedit-rename-buffer (key)
  "Setup emacspeak-w3-post-process-hook  to rename result buffer"
  (add-hook
   'emacspeak-w3-post-process-hook
   (eval
    `(function
      (lambda nil
        (rename-buffer
         (format "%s %s"
                 ,key (buffer-name))
         'unique))))))

;;;###autoload
(defun emacspeak-webedit-xslt-filter (path    url  &optional speak)
  "Extract elements matching specified XPath path locator
from Web page -- default is the current page being viewed."
  (interactive
   (list
    (read-from-minibuffer "XPath: ")
    (if (eq major-mode 'w3-mode)
        (funcall emacspeak-webutils-url-at-point)
      (read-from-minibuffer "URL: " "http://www."))
    current-prefix-arg))
  (declare (special emacspeak-webedit-xsl-filter ))
  (let ((webedit-reuse-buffers 'no)
        (params (emacspeak-xslt-params-from-xpath  path url)))
    (emacspeak-webedit-rename-buffer (format "Filtered %s" path))
    (when speak
      (add-hook 'emacspeak-w3-post-process-hook
                'emacspeak-speak-buffer))
    (emacspeak-webutils-with-xsl-environment
     emacspeak-webedit-xsl-filter
     params
     emacspeak-xslt-options
     emacspeak-xslt-options
     (browse-url url))))

;;;###autoload
(defun emacspeak-webedit-xslt-junk (path    url &optional speak)
  "Junk elements matching specified locator."
  (interactive
   (list
    (read-from-minibuffer "XPath: ")
    (if (eq major-mode 'w3-mode)
        (funcall emacspeak-webutils-url-at-point)
      (read-from-minibuffer "URL: " "http://www."))
    current-prefix-arg))
  (declare (special emacspeak-webedit-xsl-junk ))
  (let ((webedit-reuse-buffers 'no)
        (params (emacspeak-xslt-params-from-xpath  path url)))
    (emacspeak-webedit-rename-buffer
     (format "Filtered %s" path))
    (when speak
      (add-hook 'emacspeak-w3-post-process-hook
                'emacspeak-speak-buffer))
    (emacspeak-webutils-with-xsl-environment
     emacspeak-webedit-xsl-junk
     params
     emacspeak-xslt-options
     (browse-url url))))

;;;###autoload
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
(defun emacspeak-webedit-extract-media-streams (url &optional speak)
  "Extract links to media streams.
operate on current web page when in a WEBEDIT buffer; otherwise prompt for url.
 Optional arg `speak' specifies if the result should be
spoken automatically."
  (interactive
   (list
    (if (eq major-mode 'w3-mode)
        (funcall emacspeak-webutils-url-at-point)
      (read-from-minibuffer "URL: " "http://www."))
    (or (interactive-p) current-prefix-arg)))
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
     url speak)))

;;;###autoload
(defun emacspeak-webedit-extract-print-streams (url &optional speak)
  "Extract links to printable  streams.
operate on current web page when in a WEBEDIT buffer; otherwise prompt for url.
 Optional arg `speak' specifies if the result should be
spoken automatically."
  (interactive
   (list
    (if (eq major-mode 'w3-mode)
        (funcall emacspeak-webutils-url-at-point)
      (read-from-minibuffer "URL: " "http://www."))
    (or (interactive-p) current-prefix-arg)))
  (let ((filter "//a[contains(@href,\"print\")]"))
    (emacspeak-webedit-xslt-filter filter url speak)))

;;;###autoload
(defun emacspeak-webedit-extract-media-streams-under-point ()
  "In WEBEDIT mode buffers, extract media streams from url under point."
  (interactive)
  (cond
   ((and (eq major-mode 'w3-mode)
         (webedit-view-this-url 'no-show))
    (emacspeak-webedit-extract-media-streams (webedit-view-this-url 'no-show)
                                        'speak))
   (t (error "Not on a link in a WEBEDIT buffer."))))

;;;###autoload
(defun emacspeak-webedit-extract-matching-urls (pattern url &optional speak)
  "Extracts links whose URL matches pattern."
  (interactive
   (list
    (read-from-minibuffer "Pattern: ")
    (if (eq major-mode 'w3-mode)
        (funcall emacspeak-webutils-url-at-point)
      (read-from-minibuffer "URL: " "http://www."))
    (or (interactive-p)
        current-prefix-arg)))
  (let ((filter
         (format
          "//a[contains(@href,\"%s\")]"
          pattern)))
    (emacspeak-webedit-xslt-filter
     filter
     url
     speak)))

;;;###autoload
(defun emacspeak-webedit-extract-nested-table (index   url &optional speak)
  "Extract nested table specified by `table-index'. Default is to
operate on current web page when in a WEBEDIT buffer; otherwise
prompt for URL. Optional arg `speak' specifies if the result should be
spoken automatically."
  (interactive
   (list
    (read-from-minibuffer "Table Index: ")
    (if (eq major-mode 'w3-mode)
        (funcall emacspeak-webutils-url-at-point)
      (read-from-minibuffer "URL: " "http://www."))
    (or (interactive-p) current-prefix-arg)))
  (emacspeak-webedit-xslt-filter
   (format "(//table//table)[%s]" index)
   url
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
(defun emacspeak-webedit-extract-nested-table-list (tables url &optional speak)
  "Extract specified list of tables from a WWW page."
  (interactive
   (list
    (emacspeak-webedit-get-table-list)
    (if (eq major-mode 'w3-mode)
        (funcall emacspeak-webutils-url-at-point)
      (read-from-minibuffer "URL: " "http://www."))
    (or (interactive-p)
        current-prefix-arg)))
  (let ((filter
         (mapconcat
          #'(lambda  (i)
              (format "((//table//table)[%s])" i))
          tables
          " | ")))
    (emacspeak-webedit-xslt-filter filter url speak)))

;;;###autoload
(defun emacspeak-webedit-extract-table-by-position (position   url
                                                          &optional speak)
  "Extract table at specified position.
Default is to extract from current page."
  (interactive
   (list
    (read-from-minibuffer "Extract Table: ")
    (if (eq major-mode 'w3-mode)
        (funcall emacspeak-webutils-url-at-point)
      (read-from-minibuffer "URL: " "http://www."))
    (or (interactive-p)
        current-prefix-arg)))
  (emacspeak-webedit-xslt-filter
   (format "/descendant::table[%s]"
           position)
   url
   speak))

;;;###autoload
(defun emacspeak-webedit-extract-tables-by-position-list (positions url &optional speak)
  "Extract specified list of nested tables from a WWW page.
Tables are specified by their position in the list
 of nested tables found in the page."
  (interactive
   (list
    (emacspeak-webedit-get-table-list)
    (if (eq major-mode 'w3-mode)
        (funcall emacspeak-webutils-url-at-point)
      (read-from-minibuffer "URL: " "http://www."))
    (or (interactive-p)
        current-prefix-arg)))
  (let ((filter
         (mapconcat
          #'(lambda  (i)
              (format "(/descendant::table[%s])" i))
          positions
          " | ")))
    (emacspeak-webedit-xslt-filter
     filter
     url
     speak)))

;;;###autoload
(defun emacspeak-webedit-extract-table-by-match (match   url &optional speak)
  "Extract table containing  specified match.
 Optional arg url specifies the page to extract content from."
  (interactive
   (list
    (read-from-minibuffer "Tables Matching: ")
    (if (eq major-mode 'w3-mode)
        (funcall emacspeak-webutils-url-at-point)
      (read-from-minibuffer "URL: " "http://www."))
    (or (interactive-p)
        current-prefix-arg)))
  (emacspeak-webedit-xslt-filter
   (format "(/descendant::table[contains(., \"%s\")])[last()]"
           match)
   url
   speak))

;;;###autoload
(defun emacspeak-webedit-extract-tables-by-match-list (match-list
                                                  url &optional speak)
  "Extract specified  tables from a WWW page.
Tables are specified by containing  match pattern
 found in the match list."
  (interactive
   (list
    (emacspeak-webedit-get-table-match-list)
    (if (eq major-mode 'w3-mode)
        (funcall emacspeak-webutils-url-at-point)
      (read-from-minibuffer "URL: " "http://www."))
    (or (interactive-p)
        current-prefix-arg)))
  (let ((filter
         (mapconcat
          #'(lambda  (i)
              (format "((/descendant::table[contains(.,\"%s\")])[last()])" i))
          match-list
          " | ")))
    (emacspeak-webedit-xslt-filter
     filter
     url
     speak)))

(defvar emacspeak-webedit-buffer-css-class-cache nil
  "Caches class attribute values for current buffer.")

(make-variable-buffer-local 'emacspeak-webedit-buffer-css-class-cache)

(defun emacspeak-webedit-css-class-cache ()
  "Build CSS class cache for buffer if needed."
  (unless (eq major-mode 'w3-mode)
    (error "Not in WEBEDIT buffer."))
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

(defvar emacspeak-webedit-buffer-id-cache nil
  "Caches id attribute values for current buffer.")

(make-variable-buffer-local 'emacspeak-webedit-buffer-id-cache)

(defun emacspeak-webedit-id-cache ()
  "Build id  cache for buffer if needed."
  (declare (special emacspeak-webedit-buffer-id-cache))
  (unless (eq major-mode 'w3-mode)
    (error "Not in WEBEDIT buffer."))
  (or emacspeak-webedit-buffer-id-cache
      (let ((values nil)
            (buffer
             (emacspeak-xslt-url
              (expand-file-name "id-values.xsl"
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
        (funcall emacspeak-webutils-url-at-point)
      (read-from-minibuffer "URL: " "http://www."))
    (or (interactive-p) current-prefix-arg)))
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
(defun emacspeak-webedit-extract-by-class-list(classes   url &optional
                                                    speak)
  "Extract elements having class specified in list `classes' from HTML.
Extracts specified elements from current WWW page and displays it
in a separate buffer.  Interactive use provides list of class
values as completion. "
  (interactive
   (list
    (emacspeak-webedit-css-get-class-list)
    (if (eq major-mode 'w3-mode)
        (funcall emacspeak-webutils-url-at-point)
      (read-from-minibuffer "URL: " "http://www."))
    (or (interactive-p)
        current-prefix-arg)))
  (let ((filter
         (mapconcat
          #'(lambda  (c)
              (format "(@class=\"%s\")" c))
          classes
          " or ")))
    (emacspeak-webedit-xslt-filter
     (format "//*[%s]" filter)
     url
     (or (interactive-p) speak))))

;;;###autoload
(defun emacspeak-webedit-extract-by-id (id   url &optional speak)
  "Extract elements having specified id attribute from HTML. Extracts
specified elements from current WWW page and displays it in a separate
buffer.
Interactive use provides list of id values as completion."
  (interactive
   (list
    (completing-read "Id: "
                     (emacspeak-webedit-id-cache))
    (if (eq major-mode 'w3-mode)
        (funcall emacspeak-webutils-url-at-point)
      (read-from-minibuffer "URL: " "http://www."))
    (or (interactive-p)
        current-prefix-arg)))
  (emacspeak-webedit-xslt-filter
   (format "//*[@id=\"%s\"]"
           id)
   url
   speak))

;;;###autoload
(defun emacspeak-webedit-extract-by-id-list(ids   url &optional speak)
  "Extract elements having id specified in list `ids' from HTML.
Extracts specified elements from current WWW page and displays it in a
separate buffer. Interactive use provides list of id values as completion. "
  (interactive
   (list
    (emacspeak-webedit-get-id-list)
    (if (eq major-mode 'w3-mode)
        (funcall emacspeak-webutils-url-at-point)
      (read-from-minibuffer "URL: " "http://www."))
    (or (interactive-p)
        current-prefix-arg)))
  (let ((filter
         (mapconcat
          #'(lambda  (c)
              (format "(@id=\"%s\")" c))
          ids
          " or ")))
    (emacspeak-webedit-xslt-filter
     (format "//*[%s]" filter)
     url
     speak)))

;;;###autoload
(defun emacspeak-webedit-junk-by-class-list(classes   url &optional speak)
  "Junk elements having class specified in list `classes' from HTML.
Extracts specified elements from current WWW page and displays it in a
separate buffer.
 Interactive use provides list of class values as
completion. "
  (interactive
   (list
    (emacspeak-webedit-css-get-class-list)
    (if (eq major-mode 'w3-mode)
        (funcall emacspeak-webutils-url-at-point)
      (read-from-minibuffer "URL: " "http://www."))
    (or (interactive-p)
        current-prefix-arg)))
  (let ((filter
         (mapconcat
          #'(lambda  (c)
              (format "(@class=\"%s\")" c))
          classes
          " or ")))
    (emacspeak-webedit-xslt-junk
     (format "//*[%s]" filter)
     url
     speak)))

(defvar emacspeak-webedit-class-filter nil
  "Buffer local class filter.")

(make-variable-buffer-local 'emacspeak-webedit-class-filter)

;;;###autoload
(defun emacspeak-webedit-class-filter-and-follow (class url)
  "Follow url and point, and filter the result by specified class.
Class can be set locally for a buffer, and overridden with an
interactive prefix arg. If there is a known rewrite url rule, that is
used as well."
  (interactive
   (list
    (or emacspeak-webedit-class-filter
        (setq emacspeak-webedit-class-filter
              (read-from-minibuffer "Class: ")))
    (if (eq major-mode 'w3-mode)
        (webedit-view-this-url t)
      (read-from-minibuffer "URL: "))))
  (declare (special emacspeak-webedit-class-filter
                    emacspeak-webedit-url-rewrite-rule))
  (let ((redirect nil))
    (when emacspeak-webedit-url-rewrite-rule
      (setq redirect
            (replace-regexp-in-string
             (first emacspeak-webedit-url-rewrite-rule)
             (second emacspeak-webedit-url-rewrite-rule)
             url)))
    (emacspeak-webedit-extract-by-class
     emacspeak-webedit-class-filter
     (or redirect url)
     'speak)
    (emacspeak-auditory-icon 'open-object)))


(defvar emacspeak-webedit-id-filter nil
  "Buffer local id filter.")

(make-variable-buffer-local 'emacspeak-webedit-id-filter)


;;;###autoload
(defun emacspeak-webedit-follow-and-filter-by-id (id)
  "Follow url and point, and filter the result by specified id.
Id can be set locally for a buffer, and overridden with an
interactive prefix arg. If there is a known rewrite url rule, that is
used as well."
  (interactive
   (list
    (or emacspeak-webedit-id-filter
        (setq emacspeak-webedit-id-filter
              (read-from-minibuffer "Id: ")))))
  (declare (special emacspeak-webedit-id-filter
                    emacspeak-webedit-url-rewrite-rule))
  (unless (eq major-mode 'w3-mode)
    (error "This command is only useful in WEBEDIT buffers."))
  (let ((url (webedit-view-this-url t))
        (redirect nil))
    (unless url
      (error "Not on a link."))
    (when emacspeak-webedit-url-rewrite-rule
      (setq redirect
            (replace-regexp-in-string
             (first emacspeak-webedit-url-rewrite-rule)
             (second emacspeak-webedit-url-rewrite-rule)
             url)))
    (emacspeak-webedit-extract-by-id
     emacspeak-webedit-id-filter
     (or redirect url)
     'speak)))

;;;###autoload
(defun emacspeak-webedit-style-filter (style   url &optional speak )
  "Extract elements matching specified style
from HTML.  Extracts specified elements from current WWW
page and displays it in a separate buffer.  Optional arg url
specifies the page to extract contents  from."
  (interactive
   (list
    (read-from-minibuffer "Style: ")
    (if (eq major-mode 'w3-mode)
        (funcall emacspeak-webutils-url-at-point)
      (read-from-minibuffer "URL: " "http://www."))
    (or (interactive-p)
        current-prefix-arg)))
  (emacspeak-webedit-xslt-filter
   (format "//*[contains(@style,  \"%s\")]" style)
   url speak))

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
    (error "This command is only useful in WEBEDIT buffers."))
  (let ((url (webedit-view-this-url t))
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
;;;###autoload
(defun emacspeak-webedit-xpath-junk-and-follow (&optional prompt)
  "Follow url and point, and filter the result by junking
elements specified by xpath.
XPath can be set locally for a buffer, and overridden with an
interactive prefix arg. If there is a known rewrite url rule, that is
used as well."
  (interactive "P")
  (declare (special emacspeak-webedit-xpath-junk
                    emacspeak-webedit-xsl-junk
                    emacspeak-webedit-most-recent-xpath-junk
                    emacspeak-webedit-url-rewrite-rule))
  (unless (eq major-mode 'w3-mode)
    (error "This command is only useful in WEBEDIT buffers."))
  (let ((url (webedit-view-this-url t))
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
    (emacspeak-webedit-xslt-junk
     emacspeak-webedit-xpath-junk
     (or redirect url)
     'speak)))

;;}}}
;;{{{ Browse XML files:

(defsubst emacspeak-webedit-unescape-charent (start end)
  "Clean up bad XML usage."
  (declare (special emacspeak-webedit-charent-alist))
  (loop for entry in emacspeak-webedit-charent-alist
        do
        (let ((entity (car  entry))
              (replacement (cdr entry )))
          (goto-char start)
          (while (search-forward entity end t)
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
        (emacspeak-webedit-unescape-charent (point-min) (point-max)))
      (emacspeak-webutils-without-xsl
       (browse-url-of-buffer)))
    (kill-buffer src-buffer)))

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
  (emacspeak-webutils-with-xsl style url))


;;;###autoload
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

;;}}}
;;{{{  xsl keymap

(declaim (special emacspeak-webedit-map))

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
        ("a" emacspeak-webeditt-apply)
        ("c" emacspeak-webedit-extract-by-class)
        ("e" emacspeak-webedit-url-expand-and-execute)
        ("f" emacspeak-webeditt-filter)
        ("i" emacspeak-webedit-extract-by-id)
        ("I" emacspeak-webedit-extract-by-id-list)
        ("j" emacspeak-webeditt-junk)
        ("k" emacspeak-webedit-set-xsl-keep-result)
        ("m" emacspeak-webedit-extract-table-by-match)
        ("o" emacspeak-webedit-toggle)
        ("p" emacspeak-webedit-xpath-filter-and-follow)
        ("r" emacspeak-webedit-extract-media-streams)
        ("S" emacspeak-webedit-style-filter)
        ("s" emacspeak-webeditt-select)
        ("t" emacspeak-webedit-extract-table-by-position)
        ("u" emacspeak-webedit-extract-matching-urls)
        ("x" emacspeak-webedit-extract-nested-table)
        ("b" emacspeak-webedit-follow-and-filter-by-id)
        ("y" emacspeak-webedit-class-filter-and-follow)
        )
      do
      (emacspeak-keymap-update emacspeak-webedit-map binding))

;;}}}
(provide 'emacspeak-webedit)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

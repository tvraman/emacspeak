;;; emacspeak-w3.el --- Speech enable W3 WWW browser -- includes ACSS Support
;;; $Id$
;;; $Author$ 
;;; Description:  Emacspeak enhancements for W3
;;; Keywords: Emacspeak, W3, WWW
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision$ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2001, T. V. Raman 
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

;;{{{  Introduction:

;;; Commentary:

;;; Ensure that speech support for W3 gets installed and
;;; loaded correctly.
;;; The emacs W3 browser comes with builtin support for
;;; Emacspeak and ACSS

;;; Code:


;;}}}
;;{{{ requires

;;; Code:

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'custom)
(require 'emacspeak-keymap)
(require 'emacspeak-sounds)
(require 'emacspeak-speak)
(require 'wid-edit)

;;}}}
;;{{{  custom

(defgroup emacspeak-w3 nil
  "WWW browser for the Emacspeak Desktop."
  :group 'emacspeak
  :group 'w3
  :prefix "emacspeak-w3-")

;;}}}
;;{{{  additional advice

(defadvice url-write-global-history (around emacspeak pre act comp)
  "Silence messages while this function executes"
  (let ((emacspeak-speak-messages nil))
    ad-do-it))

;;}}}
;;{{{ setup

(declaim (special w3-echo-link
                  url-show-status
                  w3-mode-map))

(condition-case nil
    (progn (require 'w3-speak)
           (add-hook 'w3-mode-hook 'w3-speak-mode-hook)
           (add-hook 'w3-mode-hook 'emacspeak-pronounce-toggle-use-of-dictionaries)
           (setq w3-echo-link
                 (list 'text 'title 'name 'url))
           (if (locate-library "w3-speak-table")
               (load-library "w3-speak-table")
             (message
              "Please upgrade to W3 4.0.18 for spoken table support"))
           (setq url-show-status nil))
  (error (emacspeak-auditory-icon 'warn-user)
         (message
          "You appear to be using an old version of W3
that is no longer supported by Emacspeak.")))

(eval-when (load)
  (require 'emacspeak-keymap)
  (emacspeak-keymap-remove-emacspeak-edit-commands w3-mode-map))

(add-hook
 'w3-mode-hook
 (function
  (lambda ()
    (modify-syntax-entry 10 " ")
    (define-key w3-mode-map ";"
      'emacspeak-w3-speak-this-element)
    (define-key w3-mode-map "\M-s" 'emacspeak-w3-jump-to-submit)
    (define-key w3-mode-map "y" 'emacspeak-w3-url-rewrite-and-follow)
    (define-key w3-mode-map "n"
      'emacspeak-w3-next-doc-element)
    (define-key w3-mode-map "p" 'emacspeak-w3-previous-doc-element)
    (define-key w3-mode-map "L"
      'emacspeak-w3-lynx-url-under-point)
    (define-key w3-mode-map "\C-f" 'w3-table-focus-on-this-cell)
    (define-key w3-mode-map  "\C-t" 'emacspeak-w3-toggle-table-borders)
    (define-key w3-mode-map "'" 'emacspeak-speak-rest-of-buffer)
    (define-key w3-mode-map "j" 'imenu)
    (define-key w3-mode-map "\M- " 'emacspeak-imenu-speak-this-section)
    (define-key w3-mode-map "\M-p" 'emacspeak-imenu-goto-previous-index-position)
    (define-key w3-mode-map "\M-n" 'emacspeak-imenu-goto-next-index-position))))


(add-hook                                'w3-load-hook
                                         (function
                                          (lambda ()
                                            (when (locate-library
                                                   "w3-imenu")
                                              (require 'w3-imenu)))))

;;}}}
;;{{{  dump using lynx 

(defcustom emacspeak-w3-lynx-program "lynx"
  "Name of lynx executable"
  :type 'file
  :group 'emacspeak-w3)

(defun emacspeak-w3-lynx-done-alert (process state)
  "Alert user when lynx is done dumping the document"
  (declare (special view-exit-action))
  (when (y-or-n-p
         "Lynx is done --switch to the results?")
    (pop-to-buffer (process-buffer process))
    (goto-char (point-min))
    (view-mode)
    (setq view-exit-action 'kill-buffer)
    (skip-syntax-forward " ")
    (emacspeak-speak-line)))

(defun emacspeak-w3-lynx-url-under-point ()
  "Display contents of URL under point using LYNX.  The
document is displayed in a separate buffer. Note that the
hyperlinks in that display are not active-- this facility is
present only to help me iron out the remaining problems with
the table structure extraction code in W3."
  (interactive )
  (unless (eq major-mode 'w3-mode)
    (error
     "This command should be called only in W3 buffers"))
  (let ((url (or (w3-view-this-url t)
                 (url-view-url t)))
        (process nil))
    (unless url
      (error "No URL under point"))
    (setq process
          (start-process   "lynx"
                           (format "*lynx-%s*" url)
                           emacspeak-w3-lynx-program
                           "-dump"
                           url))
    (set-process-sentinel process 'emacspeak-w3-lynx-done-alert)))

;;}}}
;;{{{ fixup images
(declaim (special w3-version))


    
                                        ; simple heuristic to detect silly bullets and dots
                                        ; (by Greg Stark <gsstark@mit.edu>, enriched with regexp)

(defvar w3-min-img-size 32
  "*Image size under which the alt string is replaced by `w3-dummy-img-alt-repl'.
15 is a bit aggressive, 5 pixels would be safer")

(defvar w3-dummy-img-re
  "\\(bullet\\|\\b\\(boule\\|dot\\|pebble[0-9]*[a-z]?[0-9]*\\|pixel\\)\\b\\)"
  "Image name regexp for which the alt string is replaced by `w3-dummy-img-alt-repl'.")

(defvar w3-dummy-img-alt-repl "@"
  "*Dummy img alt replacement")

(declare (special  w3-auto-image-alt))
(setq w3-auto-image-alt
      (function
       (lambda (s)
         (declare (special width height
                           w3-auto-image-alt))
         (if (or (and (stringp height)
                      (< (string-to-int height) w3-min-img-size))
                 (and (stringp width)
                      (< (string-to-int width) w3-min-img-size))
                 (string-match w3-dummy-img-re s))
             w3-dummy-img-alt-repl
           (concat "[" (file-name-sans-extension s)
                   "]")))))

;;}}}
;;{{{ toggle table borders:
;;;I'd rather make the borders inaudible-- but that is hard
;;;at present.
;;; In the meantime, here is a toggle that allows you to
;;; turn borders on and off:

(defvar emacspeak-w3-table-draw-border
  nil
  "Reflects whether we allow W3 to draw table borders. ")



(defvar emacspeak-w3-table-silent-border (make-vector 16 32)
  "Used to draw empty W3 table borders. ")

(defun emacspeak-w3-toggle-table-borders ()
  "Toggle drawing of W3 table borders"
  (interactive)
  (declare (special w3-table-border-chars))
  (setq emacspeak-w3-table-draw-border (not emacspeak-w3-table-draw-border))
  (cond
   (emacspeak-w3-table-draw-border
    (setq w3-table-border-chars (w3-setup-terminal-chars)))
   (t (setq w3-table-border-chars
            emacspeak-w3-table-silent-border)))
  (message "W3 will %s draw table borders from now on"
           (if emacspeak-w3-table-draw-border "" "not")))


;;}}}
;;{{{ Experimental --element navigation

;;;This should eventually be done via a DOM API


(defsubst emacspeak-w3-html-stack () (get-text-property (point) 'html-stack))

(defsubst emacspeak-w3-html-stack-top-element (&optional stack)
  (or stack (setq stack (emacspeak-w3-html-stack)))
  (first (first stack )))

(defun emacspeak-w3-next-doc-element (&optional count)
  "Move forward  to the next document element.
Optional interactive prefix argument COUNT 
specifies by how many eleemnts to move."
  (interactive "P")
  (cond
   ((null count)
    (goto-char
     (next-single-property-change (point)
                                  'html-stack
                                  (current-buffer)
                                  (point-max)))
    (unless (emacspeak-w3-html-stack)
                                        ;skip over null region
      (goto-char
       (next-single-property-change (point)
                                    'html-stack
                                    (current-buffer)
                                    (point-max)))))
   (t (message "Moving by more than 1 not yet
implemented. ")))
  (let ((emacspeak-show-point t))
    (emacspeak-w3-speak-next-element)))

(defun emacspeak-w3-previous-doc-element (&optional count)
  "Move back  to the previous document element.
Optional interactive prefix argument COUNT 
specifies by how many eleemnts to move."
  (interactive "P")
  (cond
   ((null count)
    (unless (emacspeak-w3-html-stack)
                                        ;skip over null region
      (goto-char
       (previous-single-property-change (point)
                                        'html-stack
                                        (current-buffer)
                                        (point-min))))
    (goto-char
     (previous-single-property-change (point)
                                      'html-stack
                                      (current-buffer)
                                      (point-min))))
   (t (message "Moving by more than 1 not yet
implemented. ")))
  (let ((emacspeak-show-point t))
    (emacspeak-w3-speak-this-element)))


(defun emacspeak-w3-speak-this-element ()
  "Speak document element under point."
  (interactive)
  (let ((start nil)
        (end nil))
    (save-excursion
      (goto-char (previous-single-property-change (point)
                                                  'html-stack
                                                  (current-buffer)
                                                  (point-min)))
      (setq start (point))
      (goto-char (next-single-property-change (point)
                                              'html-stack
                                              (current-buffer)
                                              (point-max)))
      (setq end (point))
      (emacspeak-speak-region start end )
      (emacspeak-auditory-icon 'select-object))))


(defun emacspeak-w3-speak-next-element ()
  "Speak next document element."
  (interactive)
  (let ((start (point))
        (end nil))
    (save-excursion
      
      (goto-char (next-single-property-change (point)
                                              'html-stack
                                              (current-buffer)
                                              (point-max)))
      (setq end (point))
      (emacspeak-speak-region start end )
      (emacspeak-auditory-icon 'select-object))))
;;}}}
;;{{{ experimental --unravel javascript urls 
(defvar emacspeak-w3-javascript-cleanup-buffer " *javascript-cleanup*"
  "temporary scratch area")


(defun emacspeak-w3-do-onclick ()
  "Do  onclick action."
  (interactive)
  (unless (and (eq major-mode 'w3-mode)
               (widget-at (point)))
    (error "Not on a W3 link"))
  (let ((onclick (widget-get (widget-at (point)) :onclick))
        (url nil)
        (start nil)
        (end nil))
    (unless onclick
      (error "This link has no onclick attribute"))
    (message onclick)
    (when (setq start
                (string-match "http" onclick))
      (setq url (substring  onclick start ))
      (when (setq end (string-match "'" url))
        (setq url (substring url 0 end)))
      (w3-fetch url))))

(defun emacspeak-w3-javascript-follow-link ()
  "Follow URL hidden inside a javascript link"
  (interactive)
  (unless (eq major-mode 'w3-mode)
    (error "Not in a W3 buffer."))
  (let ((j-url (w3-view-this-url 'no-show))
        (url nil)
        (start nil)
        (end nil))
    (setq start (string-match "'" j-url))
    (setq url (substring j-url (1+ start)))
    (setq end (string-match "'" url))
    (setq url (substring url 0 end))
    (when (string-match "http" url)
      (w3-fetch url))
    (w3-relative-link url)))

(define-key w3-mode-map "\M-o" 'emacspeak-w3-do-onclick)
(define-key w3-mode-map "\M-j"
  'emacspeak-w3-javascript-follow-link)
(define-key w3-mode-map "t"  'emacspeak-w3-jump-to-title-in-content)

;;}}}
;;{{{ experimental --show class attribute from anchors 
(defun emacspeak-w3-show-anchor-class ()
  "Display any class attributes set on corresponding anchor
element. "
  (interactive)
  (when (and (eq major-mode 'w3-mode)
             (widget-at (point)))
    (message (mapconcat #'identity 
                        (widget-get (widget-at (point)) :class ) " "))))

;;}}}
;;{{{ load realaudio if available 
(when (locate-library "emacspeak-realaudio")
  (require 'emacspeak-realaudio))

;;}}}
;;{{{  freeamp for mp3 
(require 'emacspeak-freeamp)
(defun emacspeak-w3-freeamp ()
  "View the current buffer using emacspeak's freeamp interface"
  (let ((tmpname (mailcap-generate-unique-filename)))
    (write-region (point-min) (point-max) tmpname)
    (emacspeak-freeamp tmpname)))
(when (and (locate-library "mailcap")
           (or  (file-exists-p "/usr/bin/freeamp")
                (file-exists-p "/usr/local/bin/freeamp")))
  (require 'mailcap)
  (mapcar (lambda (type)
            (mailcap-add (concat "audio/" type) 'emacspeak-w3-freeamp
                         '(fboundp 'emacspeak-freeamp)))
          '("x-mpegurl" "x-mpeg" "x-mp3" "scpls" "mpegurl" "mpeg" "mp3")))

;;}}}
;;{{{ url rewrite

(defvar emacspeak-w3-url-rewrite-rule nil
  "URL rewrite rule to use in current buffer.")

(make-variable-buffer-local 'emacspeak-w3-url-rewrite-rule)

(defun emacspeak-w3-url-rewrite-and-follow (&optional prompt)
  "Apply a url rewrite rule as specified in the current buffer
before following link under point.  If no rewrite rule is
defined, first prompt for one.  Rewrite rules are of the
form `(from to)' where from and to are strings.  Typically,
the rewrite rule is automatically set up by Emacspeak tools
like websearch where a rewrite rule is known.  Rewrite rules
are useful in jumping directly to the printer friendly
version of an article for example.
Optional interactive prefix arg  prompts for a rewrite rule
even if one is already defined."
  (interactive "P")
  (declare (special emacspeak-w3-url-rewrite-rule))
  (unless (fboundp 'string-replace-match)
    (error "Install and load the elib package to use this feature."))
    
  (unless (eq major-mode 'w3-mode)
    (error "This command is only useful in W3 buffers."))
  (let ((url (w3-view-this-url t))
        (redirect nil))
    (unless url
      (error "Not on a link."))
    (when (or prompt 
              (null emacspeak-w3-url-rewrite-rule))
      (setq emacspeak-w3-url-rewrite-rule 
            (read-minibuffer  "Specify rewrite rule: " "(")))
    (setq redirect
          (string-replace-match (first emacspeak-w3-url-rewrite-rule)
                                url
                                (second
                                 emacspeak-w3-url-rewrite-rule)))
    (emacspeak-auditory-icon 'select-object)
    (browse-url
     (or redirect url))
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'open-object)))

;;}}}
;;{{{  jump to title in document

(defun emacspeak-w3-jump-to-title-in-content ()
  "Jumps to the occurrence of document title in page body."
  (interactive)
  (condition-case nil
      (progn
        (search-forward (buffer-name))
        (emacspeak-speak-line)
        (emacspeak-auditory-icon 'large-movement))
    (error "Title not found in body.")))

;;}}}
;;{{{ jump to submit button

(defun emacspeak-w3-jump-to-submit ()
  "Jump to next available submit button."
  (interactive)
  (let ((start (point))
        (found nil))
    (forward-char 1)
    (while (and (not found)
                (< start (point)))
      (condition-case nil
          (widget-forward 1)
        (error "No buttons found."))
      (when
          (eq (aref (widget-get (widget-at (point)) :w3-form-data) 0)
              'submit)
        (w3-speak-summarize-form-field)
        (emacspeak-auditory-icon 'large-movement)
        (setq found t)))
    (message "Could not find submit button.")))

;;}}}
;;{{{ applying XSL transforms before displaying

(defvar emacspeak-w3-xsl-directory
  (expand-file-name "xsl/" emacspeak-directory)
  "Directory holding XSL transformations.")

(define-prefix-command 'emacspeak-w3-xsl-map )
(define-key w3-mode-map "e" 'emacspeak-w3-xsl-map)  

(defvar emacspeak-w3-xsl-p nil
  "T means we apply XSL transformation before displaying
HTML.")

(defvar emacspeak-w3-xsl-transform nil
  "Specifies transform to use before displaying a page.
Nil means no transform is used. ")

(defcustom emacspeak-xslt-program "xsltproc"
  "Name of XSLT transformation engine."
  :type 'string
  :group 'emacspeak-w3)

(defun emacspeak-w3-xslt-region (xsl start end &optional params )
  "Apply XSLT transformation to region and replace it with
the result.
This uses XSLT processor xsltproc available as part of the
libxslt package."
  (declare (special emacspeak-w3-xsl-program))
  (let ((tempfile
         (format "/tmp/trans%s.xml"
                 (random))))
    (write-region start end tempfile)
    (erase-buffer)
    (let ((parameters (when params 
                        (mapconcat 
                         #'(lambda (pair)
                             (format "--param %s %s "
                                     (car pair)
                                     (cdr pair)))
                         params
                         " "))))
      (shell-command
       (format "%s %s  --html --nonet --novalid %s %s"
               emacspeak-xslt-program
               (or parameters "")
               xsl tempfile)
       (current-buffer)
       "*xslt errors*")
      (delete-file tempfile))))

(defadvice  w3-parse-buffer (before emacspeak pre act comp)
  "Apply requested transform if any before displaying the
HTML."
  (when (and emacspeak-w3-xsl-p emacspeak-w3-xsl-transform)
    (emacspeak-w3-xslt-region
     emacspeak-w3-xsl-transform
     (point-min)
     (point-max))))



(defun emacspeak-w3-xslt-apply (xsl)
  "Apply specified transformation to current page."
  (interactive
   (list
    (expand-file-name
     (read-file-name "XSL Transformation: "
                     emacspeak-w3-xsl-directory))))
  (declare (special major-mode))
   (let
       ((emacspeak-w3-xsl-transform xsl)
                    (emacspeak-w3-xsl-p t))
     (unless (eq major-mode 'w3-mode)
       (error "Not in a W3 buffer."))
     (w3-reload-document)
  (emacspeak-auditory-icon 'select-object)))


(defun emacspeak-w3-xslt-select (xsl)
  "Select transformation to apply."
  (interactive
   (list
    (expand-file-name
     (read-file-name "XSL Transformation: "
                     emacspeak-w3-xsl-directory))))
  (declare (special emacspeak-w3-xsl-transform))
  (setq emacspeak-w3-xsl-transform xsl)
  (message "Will apply %s before displaying HTML pages."
           (file-name-sans-extension
            (file-name-nondirectory
             xsl)))
  (emacspeak-auditory-icon 'select-object))

(defun emacspeak-w3-xsl-toggle ()
  "Toggle  XSL transformations before displaying HTML.
This uses XSLT Processor xsltproc available as part of the
libxslt package."
  (interactive)
  (declare (special emacspeak-w3-xsl-p))
  (setq emacspeak-w3-xsl-p
        (not emacspeak-w3-xsl-p))
  (emacspeak-auditory-icon
   (if emacspeak-w3-xsl-p 'on 'off))
  (message "Turned %s XSL"
           (if emacspeak-w3-xsl-p 'on 'off)))

(defvar emacspeak-w3-extract-table-xsl
  (expand-file-name "extract-table.xsl"
                    emacspeak-w3-xsl-directory)
  "XSL transform to extract a table.")

(defun emacspeak-w3-extract-table (table-index   &optional prompt)
  "Extract tables from HTML.  Extracts specified table from
current WWW page and displays it in a separate buffer.
Optional arg url specifies the page to extract table from.
Interactive prefix arg causes url to be read from the
minibuffer."
  (interactive
   (list
    (read-from-minibuffer "Table index: ")
    current-prefix-arg))
  (declare (special emacspeak-xslt-program
                    emacspeak-w3-extract-table-xsl))
  (unless (or prompt
              (eq major-mode 'w3-mode))
    (error "Not in a W3 buffer."))
  (let ((w3-url (when (eq major-mode 'w3-mode)
                  (url-view-url t)))
        (source-url
         (cond
          ((and (interactive-p)
                prompt)
           (read-from-minibuffer "URL: "
                                 "http://www."))
          (t  prompt))))
    (save-excursion
      (cond
       (source-url
        (set-buffer (cdr (url-retrieve source-url))))
       (t (w3-source-document nil)))
      (let ((src-buffer (current-buffer))
            (emacspeak-w3-xsl-p nil))
        (emacspeak-w3-xslt-region
         emacspeak-w3-extract-table-xsl
         (point-min)
         (point-max)
         (list
          (cons "table-index" table-index)
          (cons "base"
                (format "\"'%s'\""
                        (or source-url
                            w3-url)))))
        (w3-preview-this-buffer)
        (kill-buffer src-buffer)))))


(defvar emacspeak-w3-extract-by-class-xsl
  (expand-file-name "extract-by-class.xsl"
                    emacspeak-w3-xsl-directory)
  "XSL transform to extract a elements having a specified class.")

(defun emacspeak-w3-extract-by-class (class   &optional prompt)
  "Extract elements having specified class attribute  from
HTML.  
Extracts specified elements from
current WWW page and displays it in a separate buffer.
Optional arg url specifies the page to extract table from.
Interactive prefix arg causes url to be read from the
minibuffer."
  (interactive
   (list
    (read-from-minibuffer "Class: ")
    current-prefix-arg))
  (declare (special emacspeak-xslt-program
                    emacspeak-w3-extract-by-class-xsl))
  (unless (or prompt
              (eq major-mode 'w3-mode))
    (error "Not in a W3 buffer."))
  (let ((w3-url (when (eq major-mode 'w3-mode)
                  (url-view-url t)))
        (source-url
         (cond
          ((and (interactive-p)
                prompt)
           (read-from-minibuffer "URL: "
                                 "http://www."))
          (t  prompt))))
    (save-excursion
      (cond
       (source-url
        (set-buffer (cdr (url-retrieve source-url))))
       (t (w3-source-document nil)))
      (let ((src-buffer (current-buffer))
            (emacspeak-w3-xsl-p nil))
        (emacspeak-w3-xslt-region
         emacspeak-w3-extract-by-class-xsl
         (point-min)
         (point-max)
         (list
          (cons "class"
                (format "\"'%s'\""
                        class))
          (cons "base"
                (format "\"'%s'\""
                        (or source-url
                            w3-url)))))
        (w3-preview-this-buffer)
        (kill-buffer src-buffer)))))

(defvar emacspeak-w3-xsl-filter
  (expand-file-name "xpath-filter.xsl"
                    emacspeak-w3-xsl-directory)
  "XSL transform to extract  elements matching a specified
XPath locator.")

(defun emacspeak-w3-xslt-filter (path   &optional prompt)
  "Extract elements matching specified XPath path locator
from HTML.  Extracts specified elements from current WWW
page and displays it in a separate buffer.  Optional arg url
specifies the page to extract table from.  Interactive
prefix arg causes url to be read from the minibuffer."
  (interactive
   (list
    (read-from-minibuffer "XPath: ")
    current-prefix-arg))
  (declare (special emacspeak-xslt-program
                    emacspeak-w3-xsl-filter))
  (unless (or prompt
              (eq major-mode 'w3-mode))
    (error "Not in a W3 buffer."))
  (let ((w3-url (when (eq major-mode 'w3-mode)
                  (url-view-url t)))
        (source-url
         (cond
          ((and (interactive-p)
                prompt)
           (read-from-minibuffer "URL: "
                                 "http://www."))
          (t  prompt))))
    (save-excursion
      (cond
       (source-url
        (set-buffer (cdr (url-retrieve source-url))))
       (t (w3-source-document nil)))
      (let ((src-buffer (current-buffer))
            (emacspeak-w3-xsl-p nil))
        (emacspeak-w3-xslt-region
         emacspeak-w3-xsl-filter
         (point-min)
         (point-max)
         (list
          (cons "path"
                (format "\"'%s'\""
                        path))
          (cons "locator"
                (format "'%s'"
                        path))
          (cons "base"
                (format "\"'%s'\""
                        (or source-url
                            w3-url)))))
        (w3-preview-this-buffer)
        (kill-buffer src-buffer)))))



(declaim (special emacspeak-w3-xsl-map))
(define-key emacspeak-w3-xsl-map "a"
  'emacspeak-w3-xslt-apply)
(define-key emacspeak-w3-xsl-map "f" 'emacspeak-w3-xslt-filter)
(define-key emacspeak-w3-xsl-map "s" 'emacspeak-w3-xslt-select)
(define-key emacspeak-w3-xsl-map "t"
  'emacspeak-w3-xsl-toggle)
(define-key emacspeak-w3-xsl-map "c" 'emacspeak-w3-extract-by-class)
(define-key emacspeak-w3-xsl-map "x" 'emacspeak-w3-extract-table)

;;}}}
(provide 'emacspeak-w3)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

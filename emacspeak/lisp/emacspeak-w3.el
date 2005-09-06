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
;;;Copyright (C) 1995 -- 2004, T. V. Raman 
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
(require 'emacspeak-preamble)

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
;;{{{  show http headers
(defcustom emacspeak-w3-lwp-request "lwp-request"
  "LWP Request command from perl LWP."
  :type 'string
  :group 'emacspeak-w3)

(defun emacspeak-w3-show-http-headers ()
  "Show HTTP headers using lwp-request"
  (interactive)
  (declare (special emacspeak-w3-lwp-request))
  (let ((url (if (eq major-mode 'w3-mode)
                 (or (w3-view-this-url 'no-show)
                     (url-view-url 'no-show))
               (read-from-minibuffer "URL: "
                                     "http://"  nil nil nil 
                                     "http://"))))
    (shell-command
     (format "%s -de %s"
             emacspeak-w3-lwp-request url))
    (emacspeak-auditory-icon 'task-done)
    (emacspeak-speak-other-window 1)))
  
;;}}}
;;{{{ setup

(defcustom emacspeak-w3-punctuation-mode  'all
  "Pronunciation mode to use for W3 buffers."
  :type '(choice
          (const  :tag "Ignore" nil)
          (const  :tag "some" some)
          (const  :tag "all" all))
  :group 'emacspeak-w3)

(defun emacspeak-w3-speak-mode-hook ()
  "Updated emacspeak hook for W3 mode."
  (declare (special emacspeak-w3-post-process-hook
                    emacspeak-w3-punctuation-mode))
  (set (make-local-variable 'voice-lock-mode) t)
  (when emacspeak-w3-punctuation-mode
    (setq dtk-punctuation-mode emacspeak-w3-punctuation-mode))
  (emacspeak-auditory-icon 'open-object)
  (unless emacspeak-w3-post-process-hook
    (emacspeak-speak-mode-line)))

(add-hook 'w3-mode-hook 'emacspeak-w3-speak-mode-hook)
(add-hook 'w3-mode-hook
          'emacspeak-pronounce-refresh-pronunciations)
  
(add-hook
 'w3-mode-hook
 #'(lambda ()
     (declare (special w3-mode-map))
     (modify-syntax-entry 10 " ")
     (define-key w3-mode-map "hh" 'emacspeak-w3-show-http-headers)
     (define-key w3-mode-map "e" 'emacspeak-w3-xsl-map)
     (define-key w3-mode-map "\M-o" 'emacspeak-w3-do-onclick)
     (define-key w3-mode-map "\M-j"
       'emacspeak-w3-javascript-follow-link)
     (define-key w3-mode-map "t"  'emacspeak-w3-jump-to-title-in-content)

     (define-key w3-mode-map "P"
       'emacspeak-speak-previous-personality-chunk)
     (define-key w3-mode-map "i"
       'emacspeak-w3-next-parsed-item)
     (define-key w3-mode-map "N"
       'emacspeak-speak-next-personality-chunk)
     (define-key w3-mode-map "\M-r" 'emacspeak-w3-realaudio-play-url-at-point)
     (define-key w3-mode-map "R" 'emacspeak-w3-browse-rss-at-point)
     (define-key w3-mode-map "\M-\C-m" 'emacspeak-w3-browse-link-with-style)
     (define-key w3-mode-map "/" 'emacspeak-w3-google-similar-to-this-page)
     (define-key w3-mode-map "l"
       'emacspeak-w3-google-who-links-to-this-page)
     (define-key w3-mode-map "C" 'emacspeak-w3-google-extract-from-cache)
     (define-key w3-mode-map "g" 'emacspeak-w3-google-on-this-site)
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
     (define-key w3-mode-map "\M-n" 'emacspeak-imenu-goto-next-index-position)))

(add-hook
 'w3-load-hook
 #'(lambda ()
     (declare (special w3-mode-map
                       w3-echo-link url-show-status
                       emacspeak-pronounce-common-xml-namespace-uri-pronunciations
                       emacspeak-pronounce-load-pronunciations-on-startup))
     (when (locate-library "w3-speak") (require 'w3-speak))
     (when (and (locate-library "w3-speak-table")
                (not (featurep 'w3-speak-table)))
       (load-library "w3-speak-table")
       (provide 'w3-speak-table))
     (require 'emacspeak-keymap)
     (emacspeak-keymap-remove-emacspeak-edit-commands w3-mode-map)
     (and
      emacspeak-pronounce-load-pronunciations-on-startup
      (emacspeak-pronounce-augment-pronunciations 'w3-mode
                                                  emacspeak-pronounce-common-xml-namespace-uri-pronunciations))
     (setq url-show-status nil)
     (setq w3-echo-link
           (list 'text 'title 'name 'url))
     (when (locate-library
            "w3-imenu")
       (require 'w3-imenu))))

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


(defun emacspeak-w3-next-parsed-item ()
  "Move to and speak next parsed item."
  (interactive)
  (let ((current (emacspeak-w3-html-stack))
        (start (point))
        (end nil))
    (unless current ;move to parsed item if needed
      (goto-char
       (next-single-property-change (point)
                                    'html-stack))
      (setq current (emacspeak-w3-html-stack)))
      (while current
        (goto-char (next-single-property-change (point)
                                                'html-stack ))
        (setq current (emacspeak-w3-html-stack)))
      (setq end (point))
      (emacspeak-speak-region start end)
      (emacspeak-auditory-icon 'select-object)))


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
  (let ((tmpname (format "/tmp/mm-%s" (gensym))))
    (write-region (point-min) (point-max) tmpname)
    (emacspeak-freeamp tmpname)
    (delete-file tmpname)))

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
    (when (interactive-p)
      (emacspeak-speak-mode-line)
      (emacspeak-auditory-icon 'open-object))))

;;}}}
;;{{{ url expand and execute

(defvar emacspeak-w3-url-executor nil
  "URL expand/execute function  to use in current buffer.")

(make-variable-buffer-local 'emacspeak-w3-url-executor)

(defun emacspeak-w3-url-expand-and-execute ()
  "Applies buffer-specific URL expander/executor function."
  (interactive)
  (declare (special emacspeak-w3-url-executor))
  (unless (eq major-mode 'w3-mode)
    (error "This command is only useful in W3 buffers."))
  (let ((url (w3-view-this-url t)))
    (unless url
      (error "Not on a link."))
    (cond
     ((and (boundp 'emacspeak-w3-url-executor)
           (fboundp emacspeak-w3-url-executor))
      (funcall emacspeak-w3-url-executor url))
     (t
      (setq emacspeak-w3-url-executor
            (intern
             (completing-read 
	      "Executor function: "
	      obarray 'fboundp t
	      "emacspeak-" nil )))
      (if (and (boundp 'emacspeak-w3-url-executor)
               (fboundp emacspeak-w3-url-executor))
          (funcall emacspeak-w3-url-executor url)
        (error "Invalid executor %s"
               emacspeak-w3-url-executor))))))
      

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

(define-prefix-command 'emacspeak-w3-xsl-map )
  

(defcustom emacspeak-w3-xsl-p nil
  "T means we apply XSL transformation before displaying
HTML."
  :type 'boolean
  :group 'emacspeak-w3)

;;{{{ helper macros:

(defmacro emacspeak-w3-without-xsl (&rest body)
  "Execute body with XSL turned off."
  (`
   (progn
     (declare (special emacspeak-w3-xsl-p))
     (when emacspeak-w3-xsl-p
       (setq emacspeak-w3-xsl-p nil)
       (add-hook 'emacspeak-w3-post-process-hook
		 #'(lambda ()
		     (declare (special emacspeak-w3-xsl-p))
		     (setq emacspeak-w3-xsl-p t))))
     (,@ body))))

(defmacro emacspeak-w3-with-xsl (&rest body)
  "Execute body with XSL turned on."
  (`
   (progn
     (declare (special emacspeak-w3-xsl-p))
     (unless emacspeak-w3-xsl-p
       (setq emacspeak-w3-xsl-p t)
       (add-hook 'emacspeak-w3-post-process-hook
		 #'(lambda ()
		     (declare (special emacspeak-w3-xsl-p))
		     (setq emacspeak-w3-xsl-p nil))))
     (,@ body))))

;;}}}

(defcustom emacspeak-w3-xsl-transform nil
  "Specifies transform to use before displaying a page.
Nil means no transform is used. "
  :type  '(choice 
           (file :tag "XSL")
           (const :tag "none" nil))
  :group 'emacspeak-w3)
(defcustom emacspeak-w3-cleanup-bogus-quotes t
  "Clean up bogus Unicode chars for magic quotes."
  :type 'boolean
  :group 'emacspeak-w3)

(defadvice  w3-parse-buffer (before emacspeak pre act comp)
  "Apply requested XSL transform if any before displaying the
HTML."
  (when emacspeak-w3-cleanup-bogus-quotes
    (goto-char (point-min))
    (while (search-forward "&#147;" nil t)
      (replace-match "\""))
    (goto-char (point-min))
    (while (search-forward "&#148;" nil t)
      (replace-match "\""))
    (goto-char (point-min))
    (while (search-forward "&#180;" nil t)
      (replace-match "\'")))
  (when (and emacspeak-w3-xsl-p emacspeak-w3-xsl-transform)
    (emacspeak-xslt-region
     emacspeak-w3-xsl-transform
     (point-min)
     (point-max))))

(declaim (special emacspeak-xslt-directory))
;;;###autoload
(defun emacspeak-w3-xslt-apply (xsl)
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
    (emacspeak-w3-browse-url-with-style xsl url)))

;;;###autoload
(defun emacspeak-w3-xslt-select (xsl)
  "Select XSL transformation applied to WWW pages before they are displayed ."
  (interactive
   (list
    (expand-file-name
     (read-file-name "XSL Transformation: "
                     emacspeak-xslt-directory))))
  (declare (special emacspeak-w3-xsl-transform))
  (setq emacspeak-w3-xsl-transform xsl)
  (message "Will apply %s before displaying HTML pages."
           (file-name-sans-extension
            (file-name-nondirectory
             xsl)))
  (emacspeak-auditory-icon 'select-object))
;;;###autoload
(defun emacspeak-w3-xsl-toggle ()
  "Toggle  application of XSL transformations.
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
;;;###autoload
(defun emacspeak-w3-count-matches (prompt-url locator)
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
(defun emacspeak-w3-count-nested-tables (prompt-url)
  "Count nested tables in HTML."
  (interactive
   (list
    (if (eq major-mode 'w3-mode)
        (url-view-url 'no-show)
      (read-from-minibuffer "URL: "))))
  (emacspeak-w3-count-matches
   prompt-url
   "'//table//table'" ))
;;;###autoload
(defun emacspeak-w3-count-tables (prompt-url)
  "Count  tables in HTML."
  (interactive
   (list
    (if (eq major-mode 'w3-mode)
        (url-view-url 'no-show)
      (read-from-minibuffer "URL: "))))
  (emacspeak-w3-count-matches
   prompt-url
   "//table"))

(defcustom emacspeak-w3-xsl-keep-result ""
  "Set to a non-empty string  if you want the buffer containing the transformed HTML
source to be preserved.
Value of this variable if non-empty will be used as a name for the
source buffer."
  :type 'string
  :group 'emacspeak-w3)

(make-variable-buffer-local 'emacspeak-w3-xsl-keep-result)
;;;###autoload
(defun emacspeak-w3-set-xsl-keep-result (value)
  "Set value of `emacspeak-w3-xsl-keep-result'."
  (interactive  "sEnter name of result buffer: ")
  (declare (special emacspeak-w3-xsl-keep-result))
  (setq emacspeak-w3-xsl-keep-result value))
;;;###autoload
(defun emacspeak-w3-xslt-filter (path   &optional prompt-url speak-result complement)
  "Extract elements matching specified XPath path locator
from HTML.  Extracts specified elements from current WWW
page and displays it in a separate buffer.  Optional arg url
specifies the page to extract table from.
Optional arg COMPLEMENT inverts the filter.  "
  (interactive
   (list
    (read-from-minibuffer "XPath: ")
    current-prefix-arg))
  (declare (special emacspeak-w3-post-process-hook
                    emacspeak-w3-xsl-keep-result
                    emacspeak-xslt-program
                    emacspeak-w3-xsl-filter
                    emacspeak-w3-xsl-junk))
  (unless (or prompt-url
              (eq major-mode 'w3-mode))
    (error "Not in a W3 buffer."))
  (let* ((base-url (when (eq major-mode 'w3-mode)
                     (url-view-url t)))
         (source-url
          (cond
           ((and (interactive-p) prompt-url)
            (read-from-minibuffer "URL: "
                                  "http://www."))
           (t  (or prompt-url
                   base-url))))
         (src-buffer nil)
         (emacspeak-w3-xsl-p nil)
         (keep-result emacspeak-w3-xsl-keep-result))
    (setq src-buffer
          (emacspeak-xslt-url
	   (if complement
	       emacspeak-w3-xsl-junk
	     emacspeak-w3-xsl-filter)
	   source-url
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
			      prompt-url))))))
    (save-excursion
      (set-buffer src-buffer)
      (setq emacspeak-w3-xsl-keep-result keep-result)
      (when  (or (interactive-p)
                 speak-result)
        (add-hook 'emacspeak-w3-post-process-hook
		  'emacspeak-speak-buffer))
      (emacspeak-w3-preview-this-buffer)
      (cond
       ((> (length emacspeak-w3-xsl-keep-result) 0)
        (save-excursion
          (set-buffer src-buffer)
          (rename-buffer  emacspeak-w3-xsl-keep-result  'unique)))
       (t (kill-buffer src-buffer))))))

(defun emacspeak-w3-xslt-junk (path   &optional prompt-url)
  "Junk elements matching specified locator."
  (interactive
   (list
    (read-from-minibuffer "XPath: ")
    current-prefix-arg))
  (emacspeak-w3-xslt-filter path prompt-url 'speak 'complement))

(defcustom emacspeak-w3-media-stream-suffixes
  (list
   ".ram"
   ".rm"
   ".ra"
   ".pls"
   ".asf"
   ".asx"
   ".mp3"
   ".m3u"
   ".wma"
   ".wmv"
   ".avi"
   ".mpg")
  "Suffixes to look for in detecting URLs that point to media
streams."
  :type  '(repeat
           (string :tag "Extension Suffix"))
  :group 'emacspeak-w3)
;;;###autoload
(defun emacspeak-w3-extract-media-streams ( &optional prompt-url speak)
  "Extract links to media streams.
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
  (declare (special emacspeak-w3-media-stream-suffixes))
  (let ((filter "//a[%s]")
        (predicate 
	 (mapconcat
	  #'(lambda (suffix)
	      (format "contains(@href,\"%s\")"
		      suffix))
	  emacspeak-w3-media-stream-suffixes
	  " or ")))
    (emacspeak-w3-xslt-filter
     (format filter predicate )
     prompt-url
     (or (interactive-p)
	 speak))))

;;;###autoload
(defun emacspeak-w3-extract-print-streams ( &optional prompt-url speak)
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
  (declare (special emacspeak-w3-media-stream-suffixes))
  (let ((filter "//a[contains(@href,\".pdf\")]"))
    (emacspeak-w3-xslt-filter
     filter
     prompt-url
     (or (interactive-p)
	 speak))))

(defun emacspeak-w3-extract-media-streams-under-point ()
  "In W3 mode buffers, extract media streams from url under point."
  (interactive)
  (cond
   ((and (eq major-mode 'w3-mode)
	 (w3-view-this-url 'no-show))
    (emacspeak-w3-extract-media-streams (w3-view-this-url 'no-show)
                                        'speak))
   (t (error "Not on a link in a W3 buffer."))))

(defun emacspeak-w3-extract-matching-urls (pattern  &optional prompt-url speak)
  "Extracts links whose URL matches pattern."
  (interactive
   (list
    (read-from-minibuffer "URL Pattern: ")
    current-prefix-arg))
  (let ((filter
         (format 
          "//a[contains(@href,\"%s\")]"
          pattern)))
    (emacspeak-w3-xslt-filter
     filter
     prompt-url
     (or (interactive-p)
	 speak))))

;;;###autoload
(defun emacspeak-w3-extract-nested-table (table-index   &optional prompt-url speak)
  "Extract nested table specified by `table-index'. Default is to
operate on current web page when in a W3 buffer; otherwise
`prompt-url' is the URL to process. Prompts for URL when called
interactively. Optional arg `speak' specifies if the result should be
spoken automatically."

  (interactive
   (list
    (read-from-minibuffer
     "Table index: ")
    current-prefix-arg))
  (emacspeak-w3-xslt-filter
   (format "(//table//table)[%s]" table-index)
   prompt-url
   speak))

(defsubst  emacspeak-w3-get-table-list (&optional bound)
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

(defsubst  emacspeak-w3-get-table-match-list ()
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
(defun emacspeak-w3-extract-nested-table-list (tables   &optional prompt-url speak)
  "Extract specified list of tables from a WWW page."
  (interactive
   (list
    (emacspeak-w3-get-table-list)
    current-prefix-arg))
  (let ((filter nil))
    (setq filter
          (mapconcat
           #'(lambda  (i)
               (format "((//table//table)[%s])" i))
           tables
           " | "))
    (emacspeak-w3-xslt-filter
     filter
     prompt-url
     (or (interactive-p) speak))))
;;;###autoload
(defun emacspeak-w3-extract-table-by-position (position   &optional prompt-url speak)
  "Extract table at specified position.
 Optional arg url specifies the page to extract content from.
Interactive prefix arg causes url to be read from the minibuffer."
  (interactive
   (list
    (read-from-minibuffer
     "Table position: ")
    current-prefix-arg))
  (emacspeak-w3-xslt-filter
   (format "/descendant::table[%s]"
           position)
   prompt-url
   (or (interactive-p)
       speak)))

;;;###autoload
(defun emacspeak-w3-extract-tables-by-position-list (positions   &optional prompt-url speak)
  "Extract specified list of nested tables from a WWW page.
Tables are specified by their position in the list 
nested of tables found in the page."
  (interactive
   (list
    (emacspeak-w3-get-table-list)
    current-prefix-arg))
  (let ((filter nil))
    (setq filter
          (mapconcat
           #'(lambda  (i)
               (format "(/descendant::table[%s])" i))
           positions 
           " | "))
    (emacspeak-w3-xslt-filter
     filter
     prompt-url
     (or (interactive-p) speak))))

;;;###autoload
(defun emacspeak-w3-extract-table-by-match (match   &optional prompt-url speak)
  "Extract table containing  specified match.
 Optional arg url specifies the page to extract content from.
Interactive prefix arg causes url to be read from the minibuffer."
  (interactive
   (list
    (read-from-minibuffer
     "Tables matching: ")
    current-prefix-arg))
  (emacspeak-w3-xslt-filter
   (format "(/descendant::table[contains(., \"%s\")])[last()]"
           match)
   prompt-url
   (or (interactive-p)
       speak)))

;;;###autoload
(defun emacspeak-w3-extract-tables-by-match-list (match-list   &optional prompt-url speak)
  "Extract specified  tables from a WWW page.
Tables are specified by containing  match pattern 
 found in the match list."
  (interactive
   (list
    (emacspeak-w3-get-table-match-list)
    current-prefix-arg))
  (let ((filter nil))
    (setq filter
          (mapconcat
           #'(lambda  (i)
               (format "((/descendant::table[contains(.,\"%s\")])[last()])" i))
           match-list
           " | "))
    (emacspeak-w3-xslt-filter
     filter
     prompt-url
     (or (interactive-p) speak))))

(defvar emacspeak-w3-buffer-css-class-cache nil
  "Caches class attribute values for current buffer.")

(make-variable-buffer-local 'emacspeak-w3-buffer-css-class-cache)
        
(defun emacspeak-w3-css-class-cache ()
  "Build CSS class cache for buffer if needed."
  (unless (eq major-mode 'w3-mode)
    (error "Not in W3 buffer."))
  (or emacspeak-w3-buffer-css-class-cache
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
        (setq emacspeak-w3-buffer-css-class-cache
              (mapcar
               #'(lambda (v)
                   (cons v v ))
               values)))))

;;;###autoload
(defun emacspeak-w3-extract-by-class (class   &optional prompt-url speak)
  "Extract elements having specified class attribute from HTML. Extracts
specified elements from current WWW page and displays it in a separate
buffer. Optional arg url specifies the page to extract content from.
Interactive use provides list of class values as completion."
  (interactive
   (list
    (completing-read "Class: "
                     (emacspeak-w3-css-class-cache))
    current-prefix-arg))
  (emacspeak-w3-xslt-filter
   (format "//*[@class=\"%s\"]"
           class)
   prompt-url
   (or (interactive-p)
       speak)))

(defsubst  emacspeak-w3-css-get-class-list ()
  "Collect a list of classes by prompting repeatedly in the
minibuffer.
Empty value finishes the list."
  (let ((classes (emacspeak-w3-css-class-cache))
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
(defun emacspeak-w3-extract-by-class-list(classes   &optional prompt-url speak)
  "Extract elements having class specified in list `classes' from HTML.
Extracts specified elements from current WWW page and displays it in a
separate buffer. Optional arg url specifies the page to extract
content from. Interactive use provides list of class values as
completion. "
  (interactive
   (list
    (emacspeak-w3-css-get-class-list)
    current-prefix-arg))
  (let ((filter nil))
    (setq filter
          (mapconcat
           #'(lambda  (c)
               (format "(@class=\"%s\")" c))
           classes
           " or "))
    (emacspeak-w3-xslt-filter
     (format "//*[%s]" filter)
     prompt-url
     (or (interactive-p) speak))))

(defun emacspeak-w3-junk-by-class-list(classes   &optional prompt-url speak)
  "Junk elements having class specified in list `classes' from HTML.
Extracts specified elements from current WWW page and displays it in a
separate buffer. Optional arg url specifies the page to extract
content from. Interactive use provides list of class values as
completion. "
  (interactive
   (list
    (emacspeak-w3-css-get-class-list)
    current-prefix-arg))
  (let ((filter nil))
    (setq filter
          (mapconcat
           #'(lambda  (c)
               (format "(@class=\"%s\")" c))
           classes
           " or "))
    (emacspeak-w3-xslt-junk
     (format "//*[%s]" filter)
     prompt-url
     (or (interactive-p) speak))))

(defun emacspeak-w3-junk-by-class-list(classes   &optional prompt-url speak)
  "Junk elements having class specified in list `classes' from HTML.
Extracts specified elements from current WWW page and displays it in a
separate buffer. Optional arg url specifies the page to extract
content from. Interactive use provides list of class values as
completion. "
  (interactive
   (list
    (emacspeak-w3-css-get-class-list)
    current-prefix-arg))
  (let ((filter nil))
    (setq filter
          (mapconcat
           #'(lambda  (c)
               (format "(@class=\"%s\")" c))
           classes
           " or "))
    (emacspeak-w3-xslt-filter
     (format "//*[%s]" filter)
     prompt-url
     (or (interactive-p) speak)
     'complement)))

(defvar emacspeak-w3-xsl-filter
  (expand-file-name "xpath-filter.xsl"
                    emacspeak-xslt-directory)
  "XSL transform to extract  elements matching a specified
XPath locator.")
(defvar emacspeak-w3-xsl-junk
  (expand-file-name "xpath-junk.xsl"
                    emacspeak-xslt-directory)
  "XSL transform to junk  elements matching a specified
XPath locator.")

;;; Extracting node specified by id
(defvar emacspeak-w3-extract-node-by-id-xsl
  (expand-file-name "extract-node-by-id.xsl"
                    emacspeak-xslt-directory)
  "XSL transform to extract a node.")
;;;###autoload
(defun emacspeak-w3-extract-node-by-id (url node-id   )
  "Extract specified node from URI."
  (interactive
   (list
    (read-from-minibuffer "URL: ")
    (read-from-minibuffer "Node Id: ")))
  (declare (special emacspeak-xslt-program
                    emacspeak-w3-extract-node-by-id-xsl))
  (let ((result
         (emacspeak-xslt-url
          emacspeak-w3-extract-node-by-id-xsl
          url
          (list
           (cons "node-id" 
                 (format "\"'%s'\"" node-id))
           (cons "base"
                 (format "\"'%s'\"" url))))))
    (save-excursion
      (set-buffer  result)
      (emacspeak-w3-preview-this-buffer))))

;;}}}
;;{{{ Browse XML files:

(defun emacspeak-w3-browse-xml(location &optional prompt-style)
  "Browse XML+CSS using W3.
With interactive prefix arg, also prompt for an  XSL stylesheet.
XML files can be rendered by an XML browser that is CSS aware.
Emacs/W3 is not quite a complete XML+CSS browser, but it  does a
good enough job for many things, especially the XML files from
bookshare.org.
Setting W3 up at present to display any and all XML files at
present would be a bug, since W3 is an HTML browser --not a true
XML browser.
This command opens a specified XML file under the covers and has
W3 render it using CSS as available. The result on bookshare.org
XML files is quite usable:

0) You get Aural CSS support.

1) You get a navigable buffer using imenu if you have w3-imenu
loaded. "
  (interactive
   (list
    (read-file-name "XML File: ")
    current-prefix-arg))
  (declare (special emacspeak-xslt-options))
  (let ((buffer  (get-buffer-create " *xml work * "))
        (emacspeak-xslt-options ""))
    (save-excursion
      (set-buffer buffer)
      (insert-file-contents location)
      (when prompt-style
        (let ((xslt (read-file-name "XSL: " emacspeak-xslt-directory)))
          (emacspeak-xslt-region xslt (point-min)
                                 (point-max))))
      (emacspeak-w3-preview-this-buffer)
      (kill-buffer buffer)
      (emacspeak-auditory-icon 'open-object))))
          

;;}}}
;;{{{  xsl keymap

(declaim (special emacspeak-w3-xsl-map))
(define-key emacspeak-w3-xsl-map "k"
  'emacspeak-w3-set-xsl-keep-result)
(define-key emacspeak-w3-xsl-map "a"
  'emacspeak-w3-xslt-apply)
(define-key emacspeak-w3-xsl-map "f" 'emacspeak-w3-xslt-filter)
(define-key emacspeak-w3-xsl-map "j" 'emacspeak-w3-xslt-junk)
(define-key emacspeak-w3-xsl-map "p"
  'emacspeak-w3-xpath-filter-and-follow)
(define-key emacspeak-w3-xsl-map "\C-p"
  'emacspeak-w3-xpath-junk-and-follow)
(define-key emacspeak-w3-xsl-map "P"
  'emacspeak-w3-extract-print-streams)
(define-key emacspeak-w3-xsl-map "R"
  'emacspeak-w3-extract-media-streams-under-point)
(define-key emacspeak-w3-xsl-map "r"
  'emacspeak-w3-extract-media-streams)
(define-key emacspeak-w3-xsl-map "u" 'emacspeak-w3-extract-matching-urls)
(define-key emacspeak-w3-xsl-map "s" 'emacspeak-w3-xslt-select)
(define-key emacspeak-w3-xsl-map "\C-t"
  'emacspeak-w3-count-tables)
(define-key emacspeak-w3-xsl-map "t"
  'emacspeak-w3-extract-table-by-position)
(define-key emacspeak-w3-xsl-map "T"
  'emacspeak-w3-extract-tables-by-position-list)
(define-key emacspeak-w3-xsl-map "m"
  'emacspeak-w3-extract-table-by-match)
(define-key emacspeak-w3-xsl-map "M"
  'emacspeak-w3-extract-tables-by-match-list)
(define-key emacspeak-w3-xsl-map "o"
  'emacspeak-w3-xsl-toggle)
(define-key emacspeak-w3-xsl-map "c" 'emacspeak-w3-extract-by-class)
(define-key emacspeak-w3-xsl-map "C"
  'emacspeak-w3-extract-by-class-list)
(define-key emacspeak-w3-xsl-map "\C-c" 'emacspeak-w3-junk-by-class-list)
(define-key emacspeak-w3-xsl-map "y" 'emacspeak-w3-class-filter-and-follow)
(define-key emacspeak-w3-xsl-map "x"
  'emacspeak-w3-extract-nested-table)
(define-key emacspeak-w3-xsl-map "\C-f" 'emacspeak-w3-count-matches)
(define-key emacspeak-w3-xsl-map "\C-x" 'emacspeak-w3-count-nested-tables)
(define-key emacspeak-w3-xsl-map "X"
  'emacspeak-w3-extract-nested-table-list)
(define-key emacspeak-w3-xsl-map "e" 'emacspeak-w3-url-expand-and-execute)
(define-key emacspeak-w3-xsl-map "i" 'emacspeak-w3-extract-node-by-id)

;;}}}
;;{{{ class filter 

(defvar emacspeak-w3-class-filter nil
  "Buffer local variable specifying a class filter for following
urls.")

(make-variable-buffer-local 'emacspeak-w3-class-filter)
;;;###autoload
(defun emacspeak-w3-class-filter-and-follow (&optional prompt-class)
  "Follow url and point, and filter the result by specified class.
Class can be set locally for a buffer, and overridden with an
interactive prefix arg. If there is a known rewrite url rule, that is
used as well."
  (interactive "P")
  (declare (special emacspeak-w3-class-filter
		    emacspeak-w3-url-rewrite-rule))
  (unless (fboundp 'string-replace-match)
    (error "Install and load the elib package to use this feature."))
  (unless (eq major-mode 'w3-mode)
    (error "This command is only useful in W3 buffers."))
  (let ((url (w3-view-this-url t))
        (redirect nil))
    (unless url
      (error "Not on a link."))
    (when emacspeak-w3-url-rewrite-rule
      (setq redirect
	    (string-replace-match (first emacspeak-w3-url-rewrite-rule)
				  url
				  (second emacspeak-w3-url-rewrite-rule))))
    (when (or prompt-class 
              (null emacspeak-w3-class-filter))
      (setq emacspeak-w3-class-filter 
            (read-from-minibuffer  "Specify class: ")))
    (emacspeak-w3-extract-by-class
     emacspeak-w3-class-filter
     (or redirect url)
     'speak)
    (emacspeak-auditory-icon 'open-object)))

;;}}}
;;{{{ xpath  filter 

(defvar emacspeak-w3-xpath-filter nil
  "Buffer local variable specifying a XPath filter for following
urls.")

(make-variable-buffer-local 'emacspeak-w3-xpath-filter)
(defcustom emacspeak-w3-most-recent-xpath-filter
  "//p|ol|ul|dl|h1|h2|h3|h4|h5|h6|blockquote"
  "Caches most recently used xpath filter.
Can be customized to set up initial default."
  :type 'string
  :group 'emacspeak-w3)
;;;###autoload
(defun emacspeak-w3-xpath-filter-and-follow (&optional prompt)
  "Follow url and point, and filter the result by specified xpath.
XPath can be set locally for a buffer, and overridden with an
interactive prefix arg. If there is a known rewrite url rule, that is
used as well."
  (interactive "P")
  (declare (special emacspeak-w3-xpath-filter
                    emacspeak-w3-most-recent-xpath-filter
		    emacspeak-w3-url-rewrite-rule))
  (unless (eq major-mode 'w3-mode)
    (error "This command is only useful in W3 buffers."))
  (let ((url (w3-view-this-url t))
        (redirect nil))
    (unless url
      (error "Not on a link."))
    (when emacspeak-w3-url-rewrite-rule
      (unless (fboundp 'string-replace-match)
	(error "Install and load the elib package to use this feature."))
      (setq redirect
	    (string-replace-match (first emacspeak-w3-url-rewrite-rule)
				  url
				  (second
				   emacspeak-w3-url-rewrite-rule))))
    (when (or prompt 
              (null emacspeak-w3-xpath-filter))
      (setq emacspeak-w3-xpath-filter 
            (read-from-minibuffer  "Specify XPath: "
                                   emacspeak-w3-most-recent-xpath-filter))
      (setq emacspeak-w3-most-recent-xpath-filter
            emacspeak-w3-xpath-filter))
    (emacspeak-w3-xslt-filter emacspeak-w3-xpath-filter
			      (or redirect url)
			      'speak)
    (emacspeak-auditory-icon 'open-object)))

(defvar emacspeak-w3-xpath-junk nil
  "Records XPath pattern used to junk elements.")

(make-variable-buffer-local 'emacspeak-w3-xpath-junk)

(defvar emacspeak-w3-most-recent-xpath-junk
  nil 
  "Caches last XPath used to junk elements.")

(defun emacspeak-w3-xpath-junk-and-follow (&optional prompt)
  "Follow url and point, and filter the result by junking
elements specified xpath.
XPath can be set locally for a buffer, and overridden with an
interactive prefix arg. If there is a known rewrite url rule, that is
used as well."
  (interactive "P")
  (declare (special emacspeak-w3-xpath-junk
                    emacspeak-w3-most-recent-xpath-junk
		    emacspeak-w3-url-rewrite-rule))
  (unless (fboundp 'string-replace-match)
    (error "Install and load the elib package to use this feature."))
  (unless (eq major-mode 'w3-mode)
    (error "This command is only useful in W3 buffers."))
  (let ((url (w3-view-this-url t))
        (redirect nil))
    (unless url
      (error "Not on a link."))
    (when emacspeak-w3-url-rewrite-rule
      (setq redirect
	    (string-replace-match (first emacspeak-w3-url-rewrite-rule)
				  url
				  (second
				   emacspeak-w3-url-rewrite-rule))))
    (when (or prompt 
              (null emacspeak-w3-xpath-junk))
      (setq emacspeak-w3-xpath-junk 
            (read-from-minibuffer  "Specify XPath: "
                                   emacspeak-w3-most-recent-xpath-junk))
      (setq emacspeak-w3-most-recent-xpath-junk
            emacspeak-w3-xpath-junk))
    (emacspeak-w3-xslt-filter emacspeak-w3-xpath-junk
			      (or redirect url)
			      'speak
                              'complement)
    (emacspeak-auditory-icon 'open-object)))

;;}}}
;;{{{  browse url using specified style

;;;###autoload
(defun emacspeak-w3-browse-url-with-style (style url)
  "Browse URL with specified XSL style."
  (interactive
   (list
    (expand-file-name
     (read-file-name "XSL Transformation: "
                     emacspeak-xslt-directory))
    (read-string "URL: " (browse-url-url-at-point))))
  (declare (special emacspeak-w3-post-process-hook
                    emacspeak-w3-xsl-p
		    emacspeak-w3-xsl-transform))
  (let ((emacspeak-w3-xsl-p t)
        (emacspeak-w3-xsl-transform style)
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
      (emacspeak-w3-preview-this-buffer))
    (kill-buffer src-buffer)))

(defcustom emacspeak-w3-charent-alist
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
  :group 'emacspeak-w3)

(defsubst emacspeak-w3-unescape-charent ()
  "Clean up bad XML usage."
  (declare (special emacspeak-w3-charent-alist))
  (loop for entry in emacspeak-w3-charent-alist
        do
        (let ((entity (car  entry))
              (replacement (cdr entry )))
          (goto-char (point-min))
          (while (search-forward entity nil t)
            (replace-match replacement )))))

;;;###autoload
(defun emacspeak-w3-browse-xml-url-with-style (style url &optional unescape-charent)
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
        (emacspeak-w3-unescape-charent))
      (emacspeak-w3-preview-this-buffer))
    (kill-buffer src-buffer)))

;;}}}
;;{{{  google tool

;;;###autoload
(defun emacspeak-w3-google-who-links-to-this-page ()
  "Perform a google search to locate documents that link to the
current page."
  (interactive)
  (declare (special major-mode))
  (unless (eq major-mode 'w3-mode)
    (error "This command cannot be used outside W3 buffers."))
  (emacspeak-websearch-google
   (format "+link:%s"
           (url-view-url 'no-show))))
(defun emacspeak-w3-google-extract-from-cache ()
  "Extract current  page from the Google cache."
  (interactive)
  (declare (special major-mode))
  (unless (eq major-mode 'w3-mode)
    (error "This command cannot be used outside W3 buffers."))
  (emacspeak-websearch-google
   (format "+cache:%s"
           (url-view-url 'no-show))))

  
  
;;;###autoload
(defun emacspeak-w3-google-on-this-site ()
  "Perform a google search restricted to the current WWW site."
  (interactive)
  (declare (special major-mode))
  (unless (eq major-mode 'w3-mode)
    (error "This command cannot be used outside W3 buffers."))
  (emacspeak-websearch-google
   (format "+site:%s %s"
	   (aref 
	    (url-generic-parse-url (url-view-url 'no-show))
	    3)
	   (read-from-minibuffer "Search this site for: "))))

(defvar emacspeak-w3-google-related-uri
  "http://www.google.com/search?hl=en&num=10&q=related:")
;;;###autoload
(defun emacspeak-w3-google-similar-to-this-page ()
  "Ask Google to find documents similar to this one."
  (interactive)
  (declare (special emacspeak-w3-google-related-uri
                    major-mode))
  (unless (eq major-mode 'w3-mode)
    (error "This command cannot be used outside W3 buffers."))
  (let ((url (url-view-url 'no-show)))
    (browse-url
     (format 
      "%s%s"
      emacspeak-w3-google-related-uri 
      url))
    (search-forward "Similar")
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'open-object)))

;;}}}
;;{{{ advice focus on cell 
(defadvice w3-table-focus-on-this-cell (around emacspeak pre act comp)
  "Clone any url rewrite rules."
  (let ((rule emacspeak-w3-url-rewrite-rule))
    ad-do-it
    (when rule
      (setq emacspeak-w3-url-rewrite-rule rule))))

;;}}}
;;{{{ previewing buffers and regions 

;;;###autoload
(defun emacspeak-w3-preview-this-buffer ()
  "Preview this buffer."
  (interactive)
  (let ((filename
         (format "/tmp/%s.html"
                 (make-temp-name "w3"))))
    (write-region (point-min) 
                  (point-max)
                  filename)
    (cond
     ((interactive-p)
      (w3-open-local filename))
     (t
      (emacspeak-w3-without-xsl
       (w3-open-local filename))))
    (delete-file filename)))

;;;###autoload
(defun emacspeak-w3-preview-this-region (start end)
  "Preview this region."
  (interactive "r")
  (let ((filename
         (format "/tmp/%s.html"
                 (make-temp-name "w3"))))
    (write-region start 
                  end
                  filename)
    (cond
     ((interactive-p)
      (w3-open-local filename))
     (t
      (emacspeak-w3-without-xsl
       (w3-open-local filename))))
    (delete-file filename)))

;;}}}
;;{{{ fix bug in W3 under emacs 21 

(defadvice w3-nasty-disgusting-http-equiv-handling (around fix-bug pre act comp)
  (let ((emacspeak-use-auditory-icons nil))
    (condition-case nil 
        ad-do-it
      (error (message "caught an error")))))

;;}}}
;;{{{ enable post processor functionality 

(defvar emacspeak-w3-post-process-hook nil
  "Set locally to a  site specific post processor.
Note that this hook gets reset after it is used by W3 --and this is intentional.")

(defadvice w3-notify-when-ready (after emacspeak pre act comp)
  "Call w3 post-processor hook if set."
  (when    emacspeak-w3-post-process-hook
    (unwind-protect
        (run-hooks  'emacspeak-w3-post-process-hook)
      (setq emacspeak-w3-post-process-hook nil))))

;;}}}
;;{{{ silence url history save

(defadvice url-history-save-history (around emacspeak pre act comp)
  "Silence spoken messages while url history is being saved."
  (let ((emacspeak-speak-messages nil))
    ad-do-it))(provide 'emacspeak-w3)

;;}}}
;;{{{ silence  url package

(declaim (special url-http-version))
(setq url-http-version "1.0")

(defadvice w3-fetch-callback
  (around emacspeak pre act comp)
  "silence spoken messages."
  (let ((emacspeak-speak-messages nil))
    ad-do-it))

(defadvice url-http-content-length-after-change-function
  (around emacspeak pre act comp)
  "silence spoken messages."
  (let ((emacspeak-speak-messages nil))
    ad-do-it))

(defadvice url-http-chunked-encoding-after-change-function
  (around emacspeak pre act comp)
  "silence spoken messages."
  (let ((emacspeak-speak-messages nil))
    ad-do-it))

(defadvice url-http-wait-for-headers-change-function
  (around emacspeak pre act comp)
  "silence spoken messages."
  (let ((emacspeak-speak-messages nil))
    ad-do-it))

(defadvice url-cookie-handle-set-cookie
  (around emacspeak pre act comp)
  "silence spoken messages."
  (let ((emacspeak-speak-messages nil))
    ad-do-it
    ad-return-value))

(defadvice url-lazy-message
  (around emacspeak pre act comp)
  "silence spoken messages."
  (let ((emacspeak-speak-messages nil))
    ad-do-it))

;;}}}
;;{{{ pull RSS feed

;;;###autoload
(defun emacspeak-w3-browse-rss-at-point ()
  "Browses RSS url under point."
  (interactive)
  (unless (eq major-mode 'w3-mode)
    (error "Not in a W3 buffer."))
  (let ((url (w3-view-this-url  'no-show)))
    (cond
     (url
      (emacspeak-auditory-icon 'select-object)
      (emacspeak-rss-display url 'speak))
     (t (error "No URL under point.")))))

;;}}}
;;{{{  play url at point
;;;###autoload
(defun emacspeak-w3-realaudio-play-url-at-point (&optional prompt-time)
  "Play url under point as realaudio"
  (interactive "P")
  (declare (special emacspeak-realaudio-dont-insist-on-ram-url))
  (let ((url (w3-view-this-url 'no-show)))
    (cond
     ((or emacspeak-realaudio-dont-insist-on-ram-url
	  (string-match ".rm?$" url)
	  (string-match ".ram?$" url))
      (message "Playing Realaudio URL under point")
      (emacspeak-realaudio-play url prompt-time))
     (t (message "%s does not look like realaudio"
		 url)))))

;;}}}
;;{{{ backward compatibility 

;;; this will go away 
(defalias 'make-dtk-speech-style 'make-acss)
(defalias 'dtk-personality-from-speech-style 'acss-personality-from-speech-style)
(provide 'dtk-css-speech)

;;}}}
;;{{{ define pronunciation for document's base URI

(defcustom emacspeak-w3-base-uri-pronunciation
  " base "
  "Custom pronunciation for base URIs in w3 buffers."
  :type '(choice :tag "Base URI Pronunciation"
                 (const :tag "None" :value nil)
                 (string :tag "Custom pronunciation" :value " base "))
  :group 'emacspeak-w3)

(defun emacspeak-w3-customize-base-uri-pronunciation ()
  "Defines custom buffer local pronunciation for base URI."
  (interactive)
  (declare (special emacspeak-w3-base-uri-pronunciation))
  (let ((base-url (url-view-url 'no-show)))
    (when emacspeak-w3-base-uri-pronunciation
      (emacspeak-pronounce-add-buffer-local-dictionary-entry
       base-url
       emacspeak-w3-base-uri-pronunciation ))))
(defadvice url-view-url (around emacspeak pre act comp)
  (cond
   ((interactive-p)
    (let ((save-pronunciations emacspeak-pronounce-pronunciation-table))
      (setq emacspeak-pronounce-pronunciation-table nil)
      ad-do-it
      (setq emacspeak-pronounce-pronunciation-table save-pronunciations)))
   (t ad-do-it))
  ad-return-value)
;;}}}
;;{{{  make wget aware of emacspeak w3 url rewrite functionality 

(defadvice w3-wget (before emacspeak pre act comp)
  "Become aware of emacspeak w3 url rewrite rule,
and make the redirect available via the minibuffer history.
If a rewrite rule is defined in the current buffer, we change
  this command to behave as if it were called with an
  interactive prefix."
  (when (and (interactive-p)
             emacspeak-w3-url-rewrite-rule)
    (ad-set-arg 0 t)
    (let ((url (w3-view-this-url t))
          (redirect nil))
      (unless url
        (error "Not on a link."))
      (setq redirect
            (string-replace-match (first emacspeak-w3-url-rewrite-rule)
                                  url
                                  (second
                                   emacspeak-w3-url-rewrite-rule)))
      (push redirect minibuffer-history))))

;;}}}
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

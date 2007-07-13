;;; emacspeak-w3.el --- Speech enable W3 WWW browser -- includes ACSS Support
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak enhancements for W3
;;; Keywords: Emacspeak, W3, WWW
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-06-24 15:52:06 -0700 (Sun, 24 Jun 2007) $ |
;;;  $Revision: 4671 $ |
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
(require 'emacspeak-webutils)

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

(defcustom emacspeak-w3-create-imenu-index nil
  "Create IMenu index by default."
  :type 'boolean
  :group 'emacspeak-w3)

(defun emacspeak-w3-speak-mode-hook ()
  "Updated emacspeak hook for W3 mode."
  (declare (special emacspeak-w3-post-process-hook
                    imenu-create-index-function
                    emacspeak-w3-create-imenu-index
                    emacspeak-w3-punctuation-mode))
  (set (make-local-variable 'voice-lock-mode) t)
  (modify-syntax-entry 10 " ")
  (modify-syntax-entry 160 " ")
  (when emacspeak-w3-punctuation-mode
    (setq dtk-punctuation-mode emacspeak-w3-punctuation-mode))
  (emacspeak-auditory-icon 'open-object)
  (emacspeak-pronounce-refresh-pronunciations)
  (when (featurep 'w3-imenu)
    (setq imenu-create-index-function 'w3-imenu-create-index))
  (when emacspeak-w3-create-imenu-index
    (imenu--make-index-alist t))
  (unless emacspeak-w3-post-process-hook
    (emacspeak-speak-mode-line)))

(add-hook 'w3-mode-hook 'emacspeak-w3-speak-mode-hook)

(defun emacspeak-w3-load-hook ()
  "Setup Emacspeak keys in W3 mode."
  (declare (special w3-echo-link url-show-status
                    w3-mode-map
                    emacspeak-pronounce-common-xml-namespace-uri-pronunciations
                    emacspeak-pronounce-load-pronunciations-on-startup))
  (when (locate-library "w3-speak") (require 'w3-speak))
  (when (and (locate-library "w3-speak-table")
             (not (featurep 'w3-speak-table)))
    (load-library "w3-speak-table")
    (provide 'w3-speak-table))
  (emacspeak-keymap-remove-emacspeak-edit-commands w3-mode-map)
  (when emacspeak-pronounce-load-pronunciations-on-startup
    (emacspeak-pronounce-augment-pronunciations 'w3-mode
                                                emacspeak-pronounce-common-xml-namespace-uri-pronunciations)
    (emacspeak-pronounce-add-dictionary-entry 'w3-mode
                                              emacspeak-speak-rfc-3339-datetime-pattern
                                              (cons 're-search-forward
                                                    'emacspeak-speak-decode-rfc-3339-datetime)))
  (setq url-show-status nil)
  (setq w3-echo-link
        (list 'text 'title 'name 'url))
  (when (locate-library
         "w3-imenu")
    (require 'w3-imenu))
  (loop for binding in
        '(
          ( "\C-t" emacspeak-w3-toggle-table-borders)
          ("'" emacspeak-speak-rest-of-buffer)
          ("\"" emacspeak-speak-skim-buffer)
          ("/" emacspeak-webutils-google-similar-to-this-page)
          ("\;" emacspeak-w3-speak-this-element)
          ("A" emacspeak-w3-browse-atom-at-point)
          ("C" emacspeak-webutils-google-extract-from-cache)
          ("L" emacspeak-w3-lynx-url-under-point)
          ("N" emacspeak-speak-next-personality-chunk)
          ("P" emacspeak-speak-previous-personality-chunk)
          ("R" emacspeak-w3-browse-rss-at-point)
          ("\C-f" w3-table-focus-on-this-cell)
          ("\M- " emacspeak-imenu-speak-this-section)
          ("\M-n" emacspeak-imenu-goto-next-index-position)
          ("\M-p" emacspeak-imenu-goto-previous-index-position)
          ("\M-r" emacspeak-webutils-view-feed-via-google-reader)
          ("\M-;" emacspeak-webutils-play-media-at-point)
          ("\M-s" emacspeak-w3-jump-to-submit)
          ("c" emacspeak-w3-curl-url-under-point)
          ("e" emacspeak-w3-xsl-map)
          ("g" emacspeak-webutils-google-on-this-site)
          ("hh" emacspeak-w3-show-http-headers)
          ("i" emacspeak-w3-next-parsed-item)
          ("j" imenu)
          ("l" emacspeak-webutils-google-who-links-to-this-page)
          ("n" emacspeak-w3-next-doc-element)
          ("p" emacspeak-w3-previous-doc-element)
          ("t" emacspeak-webutils-transcode-via-google)
          ("T"  emacspeak-webutils-jump-to-title-in-content)
          ("y" emacspeak-w3-url-rewrite-and-follow)
          ("z" emacspeak-w3-speak-next-block)
          ([C-Return] emacspeak-webutils-open-in-other-browser))
        do
        (emacspeak-keymap-update w3-mode-map binding))
  (w3-masquerade-stub 1 "Mozilla" "5.0"))

(add-hook 'w3-load-hook 'emacspeak-w3-load-hook)

;;}}}
;;{{{ webutils variables

(defun emacspeak-w3-setup-webutils  ()
    "Setup webutils variables for using W3."
    (setq
     emacspeak-webutils-document-title 'buffer-name
     emacspeak-webutils-url-at-point #'(lambda nil (w3-view-this-url t))
     emacspeak-webutils-current-url #'(lambda nil (url-view-url t))))

(add-hook
 'w3-mode-hook
 'emacspeak-w3-setup-webutils)

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
    (set-process-sentinel process
                          'emacspeak-w3-lynx-done-alert)))
;;;###autoload
(defun emacspeak-w3-curl-url-under-point ()
  "Display contents of URL under point using Curl and W3.  The
document is displayed in a separate buffer. "
  (interactive )
  (unless (eq major-mode 'w3-mode)
    (error
     "This command should be called only in W3 buffers"))
  (let ((url (or (w3-view-this-url t)
                 (url-view-url t))))
    (unless url
      (error "No URL under point"))
    (emacspeak-curl url)))

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
    (unless current                ;move to parsed item if needed
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
          (replace-regexp-in-string
           (first emacspeak-w3-url-rewrite-rule)
           (second emacspeak-w3-url-rewrite-rule) url)
          url)
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

;;;###autoload
(defcustom emacspeak-w3-xsl-transform nil
  "Specifies transform to use before displaying a page.
Nil means no transform is used. "
  :type  '(choice
           (file :tag "XSL")
           (const :tag "none" nil))
  :group 'emacspeak-w3)

(defvar emacspeak-w3-xsl-params nil
  "XSL params if any to pass to emacspeak-xslt-region.")
(defsubst emacspeak-w3-xsl-params-from-xpath (path base)
  "Return params suitable for passing to  emacspeak-xslt-region"
  (list
   (cons "path"
         (format "\"'%s'\""
                 (shell-quote-argument path)))
   (cons "locator"
         (format "'%s'"
                 path))
   (cons "base"
         (format "\"'%s'\""
                 base))))

(defcustom emacspeak-w3-cleanup-bogus-quotes t
  "Clean up bogus Unicode chars for magic quotes."
  :type 'boolean
  :group 'emacspeak-w3)

(defadvice  w3-parse-buffer (before emacspeak pre act comp)
  "Apply requested XSL transform if any before displaying the
HTML."
  (when emacspeak-w3-cleanup-bogus-quotes
    (goto-char (point-min))
    (while (search-forward "&\#147\;" nil t)
      (replace-match "\""))
    (goto-char (point-min))
    (while (search-forward "&\#148\;" nil t)
      (replace-match "\""))
    (goto-char (point-min))
    (while (search-forward "&\#180\;" nil t)
      (replace-match "\'")))
  (when (and emacspeak-w3-xsl-p emacspeak-w3-xsl-transform)
    (emacspeak-xslt-region
     emacspeak-w3-xsl-transform
     (point-min)
     (point-max)
     emacspeak-w3-xsl-params)))

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
                          (shell-quote-argument path)))
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
      (browse-url-of-buffer)
      (cond
       ((> (length emacspeak-w3-xsl-keep-result) 0)
        (save-excursion
          (set-buffer src-buffer)
          (rename-buffer  emacspeak-w3-xsl-keep-result  'unique)))
       (t (kill-buffer src-buffer))))))

(defun emacspeak-w3-xslt-junk (path   &optional prompt-url speak)
  "Junk elements matching specified locator."
  (interactive
   (list
    (read-from-minibuffer "XPath: ")
    current-prefix-arg))
  (emacspeak-w3-xslt-filter path prompt-url speak 'complement))

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
   ".m4v"
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
  (unless
      (or
       (eq major-mode 'w3-mode)
       (stringp prompt-url))
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
  (let ((filter "//a[contains(@href,\"print\")]"))
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

(make-variable-buffer-local 'emacspeak-w3-buffer-id-cache)

(defun emacspeak-w3-id-cache ()
  "Build CSS class cache for buffer if needed."
  (declare (special emacspeak-w3-buffer-id-cache))
  (unless (eq major-mode 'w3-mode)
    (error "Not in W3 buffer."))
  (or emacspeak-w3-buffer-id-cache
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
        (setq emacspeak-w3-buffer-id-cache
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

(defsubst  emacspeak-w3-get-id-list ()
  "Collect a list of ids by prompting repeatedly in the
minibuffer.
Empty value finishes the list."
  (let ((ids (emacspeak-w3-id-cache))
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
(defvar emacspeak-w3-buffer-id-cache nil
  "Caches id attribute values for current buffer.")

(make-variable-buffer-local 'emacspeak-w3-buffer-id-cache)

;;;###autoload
(defun emacspeak-w3-extract-by-id (id   &optional prompt-url speak)
  "Extract elements having specified id attribute from HTML. Extracts
specified elements from current WWW page and displays it in a separate
buffer. Optional arg url specifies the page to extract content from.
Interactive use provides list of id values as completion."
  (interactive
   (list
    (completing-read "Id: "
                     (emacspeak-w3-id-cache))
    current-prefix-arg))
  (emacspeak-w3-xslt-filter
   (format "//*[@id=\"%s\"]"
           id)
   prompt-url
   (or (interactive-p)
       speak)))

;;;###autoload
(defun emacspeak-w3-extract-by-id-list(ids   &optional prompt-url speak)
  "Extract elements having id specified in list `ids' from HTML.
Extracts specified elements from current WWW page and displays it in a
separate buffer. Optional arg url specifies the page to extract
content from. Interactive use provids list of id values as
completion. "
  (interactive
   (list
    (emacspeak-w3-css-get-id-list)
    current-prefix-arg))
  (let ((filter nil))
    (setq filter
          (mapconcat
           #'(lambda  (c)
               (format "(@id=\"%s\")" c))
           ids
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
      (browse-url-of-buffer))
    (kill-buffer result)))

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
      (kill-all-local-variables)
      (erase-buffer)
      (insert-file-contents location)
      (when prompt-style
        (let ((xslt (read-file-name "XSL: " emacspeak-xslt-directory)))
          (emacspeak-xslt-region xslt (point-min)
                                 (point-max))))
      (browse-url-of-buffer)
      (kill-buffer buffer)
      (emacspeak-auditory-icon 'open-object))))

;;}}}
;;{{{  xsl keymap

(declaim (special emacspeak-w3-xsl-map))

(loop for binding in
      '(
        ("C" emacspeak-w3-extract-by-class-list)
        ("M" emacspeak-w3-extract-tables-by-match-list)
        ("P" emacspeak-w3-extract-print-streams)
        ("R" emacspeak-w3-extract-media-streams-under-point)
        ("T" emacspeak-w3-extract-tables-by-position-list)
        ("X" emacspeak-w3-extract-nested-table-list)
        ("\C-c" emacspeak-w3-junk-by-class-list)
        ("\C-f" emacspeak-w3-count-matches)
        ("\C-p" emacspeak-w3-xpath-junk-and-follow)
        ("\C-t" emacspeak-w3-count-tables)
        ("\C-x" emacspeak-w3-count-nested-tables)
        ("a" emacspeak-w3-xslt-apply)
        ("c" emacspeak-w3-extract-by-class)
        ("e" emacspeak-w3-url-expand-and-execute)
        ("f" emacspeak-w3-xslt-filter)
        ("i" emacspeak-w3-extract-by-id)
        ("I" emacspeak-w3-extract-by-id-list)
        ("j" emacspeak-w3-xslt-junk)
        ("k" emacspeak-w3-set-xsl-keep-result)
        ("m" emacspeak-w3-extract-table-by-match)
        ("o" emacspeak-w3-xsl-toggle)
        ("p" emacspeak-w3-xpath-filter-and-follow)
        ("r" emacspeak-w3-extract-media-streams)
        ("S" emacspeak-w3-style-filter)
        ("s" emacspeak-w3-xslt-select)
        ("t" emacspeak-w3-extract-table-by-position)
        ("u" emacspeak-w3-extract-matching-urls)
        ("x" emacspeak-w3-extract-nested-table)
        ("y" emacspeak-w3-class-filter-and-follow)
        )
      do
      (emacspeak-keymap-update emacspeak-w3-xsl-map binding))

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
  (unless (eq major-mode 'w3-mode)
    (error "This command is only useful in W3 buffers."))
  (let ((url (w3-view-this-url t))
        (redirect nil))
    (unless url
      (error "Not on a link."))
    (when emacspeak-w3-url-rewrite-rule
      (setq redirect
            (replace-regexp-in-string
             (first emacspeak-w3-url-rewrite-rule)
             (second emacspeak-w3-url-rewrite-rule)
             url)))
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
;;{{{ style filter
(defun emacspeak-w3-style-filter (style   &optional prompt-url speak )
  "Extract elements matching specified style
from HTML.  Extracts specified elements from current WWW
page and displays it in a separate buffer.  Optional arg url
specifies the page to extract contents  from."
  (interactive
   (list
    (read-from-minibuffer "Style: ")
    current-prefix-arg))
  (emacspeak-w3-xslt-filter
   (format "//*[contains(@style,  \"%s\")]" style)
   prompt-url speak))

;;}}}
;;{{{ xpath  filter

(defvar emacspeak-w3-xpath-filter nil
  "Buffer local variable specifying a XPath filter for following
urls.")

(make-variable-buffer-local 'emacspeak-w3-xpath-filter)
(defcustom emacspeak-w3-most-recent-xpath-filter
  "//p|ol|ul|dl|h1|h2|h3|h4|h5|h6|blockquote|div"
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
      (setq redirect
            (replace-regexp-in-string
             (first emacspeak-w3-url-rewrite-rule)
             (second emacspeak-w3-url-rewrite-rule)
             url)))
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
  (unless (eq major-mode 'w3-mode)
    (error "This command is only useful in W3 buffers."))
  (let ((url (w3-view-this-url t))
        (redirect nil))
    (unless url
      (error "Not on a link."))
    (when emacspeak-w3-url-rewrite-rule
      (setq redirect
            (replace-regexp-in-string
             (first emacspeak-w3-url-rewrite-rule)
             (second emacspeak-w3-url-rewrite-rule)
             url)))
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
      (browse-url-of-buffer))
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
      (emacspeak-webutils-without-xsl
       (browse-url-of-buffer)))
    (kill-buffer src-buffer)))

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

;;;###autoload

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
  (when     emacspeak-w3-post-process-hook
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

;; (defadvice url-http-wait-for-headers-change-function
;;   (around emacspeak pre act comp)
;;   "silence spoken messages."
;;   (let ((emacspeak-speak-messages nil))
;;     ad-do-it))

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
;;;###autoload
(defun emacspeak-w3-browse-atom-at-point ()
  "Browses Atom url under point."
  (interactive)
  (unless (eq major-mode 'w3-mode)
    (error "Not in a W3 buffer."))
  (let ((url (w3-view-this-url  'no-show)))
    (cond
     (url
      (emacspeak-auditory-icon 'select-object)
      (emacspeak-atom-display url 'speak))
     (t (error "No URL under point.")))))

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
;;{{{ jump by block level elements (experimental:

(defun emacspeak-w3-next-block ()
  "Move by block level displays."
  (interactive)
  (cond
   ((w3-table-info 0 'no-error) (w3-table-move-to-table-end))
   (t
    (while (and (not (eobp))
                (emacspeak-w3-html-stack))
      (goto-char
       (next-single-property-change (point) 'html-stack)))))
  (when (null (emacspeak-w3-html-stack))
    (goto-char (next-single-property-change (point) 'html-stack)))
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))

(defun emacspeak-w3-speak-next-block ()
  "Move to next block and speak it."
  (interactive)
  (let ((start nil))
    (emacspeak-w3-next-block)
    (save-excursion
      (setq start (point))
      (emacspeak-w3-next-block)
      (emacspeak-auditory-icon 'select-object)
      (emacspeak-speak-region start (point)))))

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
            (replace-regexp-in-string
             (first emacspeak-w3-url-rewrite-rule)
             (second emacspeak-w3-url-rewrite-rule)
             url))
      (push redirect minibuffer-history))))

;;}}}
;;{{{ cleanup with tidy:

(defcustom emacspeak-w3-tidy-program "tidy"
  "Name of tidy executable"
  :type 'file
  :group 'emacspeak-w3)

(defcustom emacspeak-w3-tidy-options
  (list "--show-warnings" "no" "--show-errors" "0" "--force-output" "yes"
        "-asxml" "-quiet"  "-bare" "-omit"
        "--drop-proprietary-attributes" "yes" "--hide-comments"
        "yes")
  "Options to pass to tidy program"
  :type '(repeat string)
  :group 'emacspeak-w3)

(defcustom emacspeak-w3-tidy-html t
  "Tidy HTML before rendering."
  :type 'boolean
  :group 'emacspeak-w3)

(defun emacspeak-w3-tidy (&optional buff)
  "Use html tidy to clean up the HTML in the current buffer."
  (declare (special emacspeak-w3-tidy-html
                    emacspeak-w3-tidy-program emacspeak-w3-tidy-options))
  (when emacspeak-w3-tidy-html
    (save-excursion
      (if buff
          (set-buffer buff)
        (setq buff (current-buffer)))
      (setq buffer-undo-list t)
      (widen)
      (apply 'call-process-region
             (point-min) (point-max)
             emacspeak-w3-tidy-program
             t
             (list buff nil)
             nil
             emacspeak-w3-tidy-options))))

(add-hook 'w3-parse-hooks 'emacspeak-w3-tidy)

;;}}}
;;{{{ fix css bug:

(defadvice css-expand-value (around fix-bug pre act comp )
  "Fix problem where bad CSS breaks W3."
  (condition-case nil
      ad-do-it
    (error nil)))

;;}}}
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

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
(require 'emacspeak-we)
(require 'emacspeak-xslt)

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
                    w3-reuse-buffers
                    w3-mode-map
                    emacspeak-pronounce-common-xml-namespace-uri-pronunciations
                    emacspeak-pronounce-load-pronunciations-on-startup))
  (setq w3-reuse-buffers 'no)
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
          ("A" emacspeak-webutils-atom-display)
          ("F" emacspeak-webutils-fv)
          ("X" emacspeak-xslt-view-xml)
          ("C" emacspeak-webutils-google-extract-from-cache)
          ("L" emacspeak-w3-lynx-url-under-point)
          ("N" emacspeak-speak-next-personality-chunk)
          ("P" emacspeak-speak-previous-personality-chunk)
          ("R" emacspeak-webutils-rss-display)
          ("\C-f" w3-table-focus-on-this-cell)
          ("\M- " emacspeak-imenu-speak-this-section)
          ("\M-n" emacspeak-imenu-goto-next-index-position)
          ("\M-p" emacspeak-imenu-goto-previous-index-position)
          ("\M-r" emacspeak-webutils-view-feed-via-google-reader)
          ("\M-;" emacspeak-webutils-play-media-at-point)
          ("\M-s" emacspeak-w3-jump-to-submit)
          ("c" emacspeak-w3-curl-url-under-point)
          ("e" emacspeak-we-xsl-map)
          ("g" emacspeak-webutils-google-on-this-site)
          ("hh" emacspeak-w3-show-http-headers)
          ("i" emacspeak-w3-next-parsed-item)
          ("j" imenu)
          ("l" emacspeak-webutils-google-who-links-to-this-page)
          ("n" emacspeak-w3-next-doc-element)
          ("p" emacspeak-w3-previous-doc-element)
          ("t" emacspeak-webutils-transcode-via-google)
          ("T"  emacspeak-webutils-jump-to-title-in-content)
          ("y" emacspeak-we-url-rewrite-and-follow)
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
;;{{{ element navigation

;;;This should eventually be done via a DOM API

(defsubst emacspeak-w3-html-stack () (get-text-property (point)
                                                        'html-stack))

(defsubst emacspeak-w3-get-onclick ()
  "Return onclick handler if any at point."
  (cdr (assoc 'onclick (cdar (emacspeak-w3-html-stack)))))

(defsubst emacspeak-w3-get-class ()
  "Return class if any at point."
  (cdr (assoc 'class (cdar (emacspeak-w3-html-stack)))))


(defsubst emacspeak-w3-get-onchange ()
  "Return onchange handler if any at point."
  (cdr (assoc 'onchange (cdar (emacspeak-w3-html-stack)))))

(defsubst emacspeak-w3-get-style ()
  "Return style if any at point."
  (cdr (assoc 'style (cdar (emacspeak-w3-html-stack)))))

(defsubst emacspeak-w3-html-stack-top-element (&optional stack)
  (or stack (setq stack (emacspeak-w3-html-stack)))
  (first (first stack )))

(defun emacspeak-w3-next-parsed-item ()
  "Move to and speak next parsed item."
  (interactive)
  (let ((current (emacspeak-w3-html-stack))
        (start (point))
        (end nil))
    (unless current                     ;move to parsed item if needed
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
;;{{{ advice focus on cell
(defadvice w3-table-focus-on-this-cell (around emacspeak pre act comp)
  "Clone any url rewrite rules."
  (let ((rule emacspeak-w3-url-rewrite-rule))
    ad-do-it
    (when rule
      (setq emacspeak-w3-url-rewrite-rule rule))))

;;}}}
;;{{{ fix bug in W3 under emacs 21

(defadvice w3-nasty-disgusting-http-equiv-handling (around fix-bug pre act comp)
  (let ((emacspeak-use-auditory-icons nil))
    (condition-case nil
        ad-do-it
      (error (message "caught an error")))))

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
;;{{{ advice to call xslt
(defadvice  w3-parse-buffer (before emacspeak pre act comp)
  "Apply requested XSL transform if any before displaying the
HTML."
  (when emacspeak-we-cleanup-bogus-quotes
    (goto-char (point-min))
    (while (search-forward "&\#147\;" nil t)
      (replace-match "\""))
    (goto-char (point-min))
    (while (search-forward "&\#148\;" nil t)
      (replace-match "\""))
    (goto-char (point-min))
    (while (search-forward "&\#180\;" nil t)
      (replace-match "\'")))
  (unless
      (or emacspeak-we-xsl-p
          (string-match "temp"
                        (buffer-name)))
    (emacspeak-we-build-id-cache)
    (emacspeak-we-build-class-cache))
  (when (and emacspeak-we-xsl-p
             emacspeak-we-xsl-transform
             (not  (string-match "temp" (buffer-name))))
    (emacspeak-xslt-region
     emacspeak-we-xsl-transform
     (point-min)
     (point-max)
     emacspeak-we-xsl-params)
    (emacspeak-we-build-id-cache)
    (emacspeak-we-build-class-cache)
    (when emacspeak-we-xsl-keep-result
      (clone-buffer
       (format "xslt-%s"
               (buffer-name))))))

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

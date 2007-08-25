;;; emacspeak-w3m.el --- speech-enables w3m-el
;;;$Id$
;;{{{ Copyright

;;; This file is not part of Emacs, but the same terms and
;;; conditions apply.
;; Copyright (C) 2001,2002  Dimitri V. Paduchih

;; Initial version: Author: Dimitri Paduchih <paduch@imm.uran.ru>
;;;author: T. V. Raman (integration with Emacspeak, and sections marked TVR)
;; Keywords: emacspeak, w3m

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;}}}

;;; Code:
;;{{{  required modules

(require 'emacspeak-preamble)
(require 'emacspeak-webutils)
(require 'emacspeak-we)
(require 'easymenu)
(require 'emacspeak-m-player)
(require 'custom)
(eval-when-compile
  (condition-case nil
      (require 'w3m)
    (error nil)))
(eval-when (load)
  (require 'w3m-util)
  (require 'w3m-form))

;;}}}
;;{{{  custom

(defgroup emacspeak-w3m nil
  "WWW browser for the Emacspeak Desktop."
  :group 'emacspeak
  :group 'w3m
  :prefix "emacspeak-w3m-")

(defcustom emacspeak-w3m-speak-titles-on-switch nil
  "Speak the document title when switching between w3m buffers.
If non-nil, switching between w3m buffers will speak the title
instead of the modeline."
  :type 'boolean
  :group 'emacspeak-w3m)

;;}}}
;;{{{ keybindings

(declaim (special w3m-mode-map
                  emacspeak-prefix))
(define-key w3m-mode-map emacspeak-prefix 'emacspeak-prefix-command)

(add-hook 'w3m-display-hook 'emacspeak-webutils-run-post-process-hook)

(define-key w3m-mode-map "x" 'emacspeak-we-xsl-map)
(define-key w3m-mode-map [M-tab] 'w3m-previous-anchor)
(define-key w3m-mode-map [(shift tab)] 'w3m-previous-anchor)
(define-key w3m-mode-map [tab] 'w3m-next-anchor)
(define-key w3m-mode-map [down] 'next-line)
(define-key w3m-mode-map [up] 'previous-line)
(define-key w3m-mode-map [right] 'emacspeak-forward-char)
(define-key w3m-mode-map [left] 'emacspeak-backward-char)
(define-key w3m-mode-map "j" 'emacspeak-webutils-jump-to-title-in-content)
(define-key w3m-mode-map "l" 'emacspeak-webutils-play-media-at-point)
(define-key w3m-mode-map "\C-t" 'emacspeak-webutils-transcode-current-url-via-google)
(define-key w3m-mode-map "\M-t" 'emacspeak-webutils-transcode-via-google)
(define-key w3m-mode-map "\C-c\C-g" 'emacspeak-webutils-google-on-this-site)
(define-key w3m-mode-map "\C-c\C-x" 'emacspeak-webutils-google-extract-from-cache)
(define-key w3m-mode-map "\C-c\C-l" 'emacspeak-webutils-google-similar-to-this-page)
(define-key w3m-mode-map (kbd "<C-return>") 'emacspeak-webutils-open-in-other-browser)

;;}}}
;;{{{ helpers

;;; The following definitions through fset are needed because at the
;;; time of compilation w3m-el may be unavailable and corresponding
;;; macros not working.

(defun emacspeak-w3m-anchor ())
(fset 'emacspeak-w3m-anchor
      (byte-compile '(lambda () (w3m-anchor))))

(defun emacspeak-w3m-get-action ())
(fset 'emacspeak-w3m-get-action
      (byte-compile '(lambda () (w3m-action))))

(defun emacspeak-w3m-action ()
  (let ((act (emacspeak-w3m-get-action)))
    (if (numberp (nth 2 act))
        (append (list (car act) (cadr act)) (nthcdr 3 act))
      act)))

(defun emacspeak-w3m-form-get (form name))
(fset 'emacspeak-w3m-form-get
      (if (functionp 'w3m-form-get-by-name)
          'w3m-form-get-by-name
        (byte-compile '(lambda (form name)
                         (w3m-form-get form name)))))

(defsubst emacspeak-w3m-personalize-string (string personality)
  (let ((newstring (copy-sequence string)))
    (put-text-property 0 (length newstring)
                       'personality personality
                       newstring)
    newstring))

(defsubst emacspeak-w3m-url-at-point ()
  "Return the url at point in w3m."
  (or (w3m-anchor (point)) (w3m-image (point))))

(defun emacspeak-w3m-current-url ()
  "Returns the value of w3m-current-url."
  (eval 'w3m-current-url))

;;}}}
;;{{{ anchors

(defvar emacspeak-w3m-speak-action-alist
  '((w3m-form-input . emacspeak-w3m-speak-form-input)
    (w3m-form-input-checkbox . emacspeak-w3m-speak-form-input-checkbox)
    (w3m-form-input-radio . emacspeak-w3m-speak-form-input-radio)
    (w3m-form-input-select . emacspeak-w3m-speak-form-input-select)
    (w3m-form-input-textarea . emacspeak-w3m-speak-form-input-textarea)
    (w3m-form-submit . emacspeak-w3m-speak-form-submit)
    (w3m-form-input-password . emacspeak-w3m-speak-form-input-password)
    (w3m-form-reset . emacspeak-w3m-speak-form-reset))
  )

(defun emacspeak-w3m-anchor-text (&optional default)
  "Return string containing text of anchor under point."
  (if (get-text-property (point) 'w3m-anchor-sequence)
      (buffer-substring
       (previous-single-property-change
        (1+ (point)) 'w3m-anchor-sequence nil (point-min))
       (next-single-property-change
        (point) 'w3m-anchor-sequence nil (point-max)))
    (or default "")))

(defun emacspeak-w3m-speak-cursor-anchor ()
  (dtk-speak (emacspeak-w3m-anchor-text "Not found")))

(defun emacspeak-w3m-speak-this-anchor ()
  (let ((url (emacspeak-w3m-anchor))
        (act (emacspeak-w3m-action)))
    (cond
     (url (emacspeak-w3m-speak-cursor-anchor))
     ((consp act)
      (let ((speak-action (cdr (assq
                                (car act)
                                emacspeak-w3m-speak-action-alist))))
        (if (functionp speak-action)
            (apply speak-action (cdr act))
          (emacspeak-w3m-speak-cursor-anchor))))
     (t (emacspeak-w3m-speak-cursor-anchor)))))

;;}}}
;;{{{  forms

(defun emacspeak-w3m-speak-form-input (form name type width maxlength
                                            value)
  "Speak form input"
  (declare (special emacspeak-w3m-form-voice))
  (dtk-speak
   (format "%s input %s  %s"
           type
           name
           (emacspeak-w3m-personalize-string
            (or (emacspeak-w3m-form-get form name) value)
            emacspeak-w3m-form-voice))))

(defun emacspeak-w3m-speak-form-input-checkbox (form name value)
  "Speak checkbox"
  (declare (special emacspeak-w3m-form-voice))
  (dtk-speak
   (format "checkbox %s is %s"
           name
           (emacspeak-w3m-personalize-string
            (if (emacspeak-w3m-form-get form name)
                "on"
              "off")
            emacspeak-w3m-form-voice))))

(defun emacspeak-w3m-speak-form-input-password (form name)
  "Speech-enable password form element."
  (declare (special emacspeak-w3m-form-voice))
  (dtk-speak
   (format "password input %s  %s"
           name
           (emacspeak-w3m-personalize-string
            (emacspeak-w3m-anchor-text)
            emacspeak-w3m-form-voice))))

(defun emacspeak-w3m-speak-form-submit (form &optional name value new-session download)
  "Speak submit button."
  (declare (special emacspeak-w3m-form-button-voice))
  (dtk-speak
   (if (equal value "")
       "submit button"
     (format "button %s"
             (emacspeak-w3m-personalize-string
              value
              emacspeak-w3m-form-button-voice)))))

(defun emacspeak-w3m-speak-form-input-radio (form name value)
  "speech enable radio buttons."
  (declare (special emacspeak-w3m-form-voice))
  (and dtk-stop-immediately (dtk-stop))
  (let* ((active (equal value (emacspeak-w3m-form-get form name)))
         (personality (if active
                          emacspeak-w3m-form-voice))
         (dtk-stop-immediately nil))
    (emacspeak-auditory-icon (if active 'on 'off))
    (dtk-speak
     (if (equal value "")
         (emacspeak-w3m-personalize-string
          (format "unset radio %s" name)
          personality)
       (format "%s of the radio %s"
               (emacspeak-w3m-personalize-string
                (concat "option " value)
                personality)
               name)))))

(defun emacspeak-w3m-speak-form-input-select (form name)
  "speech enable select control."
  (declare (special emacspeak-w3m-form-voice))
  (dtk-speak
   (format "select %s  %s"
           name
           (emacspeak-w3m-personalize-string
            (emacspeak-w3m-anchor-text)
            emacspeak-w3m-form-voice))))

(defun emacspeak-w3m-speak-form-input-textarea (form &optional hseq)
  "speech enable text area."
  (declare (special emacspeak-w3m-form-voice))
  (dtk-speak
   (format "text area %s  %s"
           (or (get-text-property (point) 'w3m-form-name) "")
           (emacspeak-w3m-personalize-string
            (emacspeak-w3m-anchor-text)
            emacspeak-w3m-form-voice))))

(defun emacspeak-w3m-speak-form-reset (form)
  "Reset button."
  (declare (special emacspeak-w3m-form-button-voice))
  (dtk-speak
   (format "button %s"
           (emacspeak-w3m-personalize-string
            "reset"
            emacspeak-w3m-form-button-voice))))

;;}}}
;;{{{  advice interactive commands:
;;{{{  commenting out for now:

;; (defadvice w3m-goto-url (around emacspeak pre act)
;;   "Speech-enable W3M."
;;   (cond
;;    ((interactive-p)
;;     (emacspeak-auditory-icon 'select-object)
;;     (let ((emacspeak-speak-messages nil))
;;       ad-do-it))
;;    (t ad-do-it))ad-return-value)

;; (defadvice w3m-redisplay-this-page (around emacspeak pre act)
;;   "Speech-enable W3M."
;;   (cond
;;    ((interactive-p)
;;     (emacspeak-auditory-icon 'select-object)
;;     (let ((emacspeak-speak-messages nil))
;;       ad-do-it))
;;    (t ad-do-it))ad-return-value)

;; (defadvice w3m-reload-this-page (around emacspeak pre act)
;;   "Speech-enable W3M."
;;   (cond
;;    ((interactive-p)
;;     (emacspeak-auditory-icon 'select-object)
;;     (let ((emacspeak-speak-messages nil))
;;       ad-do-it))
;;    (t ad-do-it))ad-return-value)

;;}}}
(loop for f in
      '(w3m-print-current-url  w3m-print-this-url
                               w3m-search
                               w3m-edit-current-url w3m-edit-this-url)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Produce auditory icon."
          (when (interactive-p)
            (emacspeak-auditory-icon 'select-object)))))

(defadvice w3m-submit-form (after emacspeak pre act comp)
  "Produce auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'button)))

(loop for f in
      '(w3m-previous-buffer w3m-next-buffer
                            w3m-view-next-page w3m-view-previous-page
                            w3m-view-parent-page w3m-gohome)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (interactive-p)
            (declare (special w3m-current-title))
            (emacspeak-auditory-icon 'select-object)
            (if emacspeak-w3m-speak-titles-on-switch
                (dtk-speak w3m-current-title)
              (emacspeak-speak-mode-line))))))

(defadvice w3m-delete-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (declare (special w3m-current-title))
    (emacspeak-auditory-icon 'close-object)
    (if emacspeak-w3m-speak-titles-on-switch
        (dtk-speak w3m-current-title)
      (emacspeak-speak-mode-line))))

(defadvice w3m-delete-other-buffers (after emacspeak pre act comp)
  "Produce auditory icon."
  (when (interactive-p)
    (declare (special w3m-current-title))
    (emacspeak-auditory-icon 'close-object)
    (if emacspeak-w3m-speak-titles-on-switch
        (dtk-speak w3m-current-title)
      (emacspeak-speak-mode-line))))

(defadvice w3m-bookmark-kill-entry (around emacspeak pre act comp)
  "Resets the punctuation mode to the one before the delete"
  (when (interactive-p)
    (emacspeak-auditory-icon 'ask-question)
    (let ((current-punct-mode dtk-punctuation-mode))
      ad-do-it
      (dtk-set-punctuations current-punct-mode))
    (emacspeak-auditory-icon 'delete-object)))

(defadvice w3m-bookmark-add-current-url (after emacspeak pre act comp)
  "Produce auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'save-object)))

(defadvice w3m-bookmark-add-this-url (after emacspeak pre act comp)
  "Produce auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'save-object)))

(loop for f in
      '(w3m-next-anchor w3m-previous-anchor
                        w3m-next-image w3m-previous-image
                        w3m-next-form w3m-previous-form)
      do
      (eval
       `(defadvice ,f (around emacspeak pre act)
          "Speech-enable W3M."
          (cond
           ((interactive-p)
            (let ((emacspeak-speak-messages nil))
              ad-do-it
              (emacspeak-auditory-icon 'large-movement)
              (emacspeak-w3m-speak-this-anchor)))
           (t ad-do-it))
          ad-return-value)))

(defadvice w3m-view-this-url (around emacspeak pre act comp)
  "Speech-enable W3M."
  (cond
   ((interactive-p)
    (let ((url (emacspeak-w3m-anchor))
          (act (emacspeak-w3m-action)))
      (when url
        (emacspeak-auditory-icon 'select-object))
      ad-do-it
      (when (and (not url)
                 (consp act)
                 (memq (car act)
                       '(w3m-form-input
                         w3m-form-input-radio
                         w3m-form-input-checkbox
                         w3m-form-input-password)))
        (emacspeak-auditory-icon 'select-object)
        (emacspeak-w3m-speak-this-anchor))))
   (t ad-do-it))
  ad-return-value)

(defadvice w3m-history (around emacspeak pre act)
  "Speech-enable W3M."
  (cond
   ((interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (let ((emacspeak-speak-messages nil))
      ad-do-it))
   (t ad-do-it))ad-return-value)

(defadvice w3m-antenna (around emacspeak pre act)
  "Speech-enable W3M."
  (cond
   ((interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (let ((emacspeak-speak-messages nil))
      ad-do-it))
   (t ad-do-it))ad-return-value)

(defadvice w3m-bookmark-view (around emacspeak pre act)
  "Speech-enable W3M."
  (cond
   ((interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (let ((emacspeak-speak-messages nil))
      ad-do-it))
   (t ad-do-it))ad-return-value)

(loop for f in
      '(w3m-scroll-up-or-next-url
        w3m-scroll-down-or-previous-url w3m-scroll-left
        w3m-shift-left w3m-shift-right
        w3m-horizontal-recenter w3m-horizontal-scroll
        w3m-scroll-right
        )
      do
      (eval
       `(defadvice ,f (around emacspeak pre act comp)
          "Speech-enable scrolling."
          (cond
           ((interactive-p)
            (let ((opoint (save-excursion
                            (beginning-of-line)
                            (point))))
              ;; hide opoint from advised function
              (let (opoint) ad-do-it)
              (emacspeak-auditory-icon 'scroll)
              (emacspeak-speak-region opoint
                                      (save-excursion (end-of-line)
                                                      (point)))))
           (t ad-do-it))
          ad-return-value)))

(defadvice w3m (around emacspeak pre act)
  "Speech-enable W3M."
  (cond
   ((interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (let ((emacspeak-speak-messages nil))
      ad-do-it)
    (when (eq (ad-get-arg 0) 'popup)
      (emacspeak-speak-mode-line)))
   (t ad-do-it))ad-return-value)

(defadvice w3m-process-stop (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)))

(defadvice w3m-close-window (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (with-current-buffer (window-buffer)
      (emacspeak-speak-mode-line))))

(defadvice w3m-quit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (with-current-buffer (window-buffer)
      (emacspeak-speak-mode-line))))

(defadvice w3m-wget (after emacspeak pre act comp)
  "provide auditory confirmation"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))

(defadvice w3m-view-header (after emacspeak pre act comp)
  "Speech enable w3m"
  (when (interactive-p)
    (declare (special w3m-current-title
                      w3m-current-url))
    (cond
     ((string-match "\\`about://header/" w3m-current-url)
      (message"viewing header information for %s "w3m-current-title  )))))

(defadvice w3m-view-source (after emacspeak pre act comp)
  "Speech enable w3m"
  (when (interactive-p)
    (declare (special w3m-current-title
                      w3m-current-url))
    (cond
     ((string-match "\\`about://source/" w3m-current-url)
      (message"viewing source for %s "w3m-current-title  )))))

(defadvice w3m-history-store-position (after emacspeak pre act comp)
  "Speech enable w3m."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (dtk-speak "Marking page position")))

(defadvice w3m-history-restore-position (after emacspeak pre act comp)
  "Speech enable w3m."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (dtk-speak "Restoring previously marked position")))

(defadvice w3m-history (after emacspeak pre act comp)
  "Speech enable w3m"
  (when (interactive-p)
    (dtk-speak "Viewing history")))

;;}}}
;;{{{ webutils variables

(add-hook 'w3m-fontify-after-hook
          #'(lambda ()
              (setq emacspeak-webutils-document-title 'w3m-current-title)
              (setq emacspeak-webutils-url-at-point 'emacspeak-w3m-url-at-point)
              (setq emacspeak-webutils-current-url 'emacspeak-w3m-current-url)))

;;}}}
;;{{{ buffer select mode

(defadvice w3m-select-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice w3m-select-buffer-show-this-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-speak-other-window 1)))

(defadvice w3m-select-buffer-show-this-line-and-down (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-speak-other-window 1)))

(defadvice w3m-select-buffer-show-this-line-and-switch (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defadvice w3m-select-buffer-show-this-line-and-quit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice w3m-select-buffer-next-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice w3m-select-buffer-previous-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice w3m-select-buffer-delete-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-line)))

(defadvice w3m-select-buffer-delete-other-buffers (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)))

(defadvice w3m-select-buffer-quit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{ input select mode

(add-hook 'w3m-form-input-select-mode-hook
          (lambda ()
            (emacspeak-auditory-icon 'select-object)
            (emacspeak-speak-line)))

(defadvice w3m-form-input-select-set (after emacspeak pre act comp)
  (when (and (interactive-p) (w3m-anchor-sequence))
    (emacspeak-w3m-speak-this-anchor)))

(defadvice w3m-form-input-select-exit (after emacspeak pre act comp)
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)))

;;}}}
;;{{{ input textarea mode

(add-hook 'w3m-form-input-textarea-mode-hook
          (lambda ()
            (emacspeak-auditory-icon 'open-object)
            (dtk-speak "edit text area")))

(defadvice w3m-form-input-textarea-set (after emacspeak pre act comp)
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-w3m-speak-this-anchor)))

(defadvice w3m-form-input-textarea-exit (after emacspeak pre act comp)
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)))

;;}}}
;;{{{ TVR: applying XSL

(defadvice  w3m-create-text-page (before emacspeak pre act comp)
  "Apply requested transform if any before displaying the HTML. "
  (when (and emacspeak-we-xsl-p emacspeak-we-xsl-transform)
    (emacspeak-xslt-region
     emacspeak-we-xsl-transform
     (point-min)
     (point-max)
     emacspeak-we-xsl-params)))

;; Helper function for xslt functionality
;;;###autoload
(defun emacspeak-w3m-preview-this-buffer ()
  "Preview this buffer in w3m."
  (interactive)
  (let ((filename
         (format "/tmp/%s.html"
                 (make-temp-name "w3m"))))
    (write-region (point-min)
                  (point-max)
                  filename)
    (w3m-find-file filename)
    (delete-file filename)))

;;}}}
;;{{{  xsl keymap

(add-hook 'w3m-mode-setup-functions
          '(lambda ()
             (easy-menu-define xslt-menu w3m-mode-map
               "XSLT menu"
               '("XSLT transforming"
                 ["Enable default transforming on the fly"
                  emacspeak-we-xsl-toggle
                  :included (not emacspeak-we-xsl-p)]
                 ["Disable default transforming on the fly"
                  emacspeak-we-xsl-toggle
                  :included emacspeak-we-xsl-p]
                 ["Add regular submit button"
                  emacspeak-w3m-xsl-add-submit-button t]
                 ["Show only search hits"
                  emacspeak-wizards-google-hits t]
                 ["Linearize tables"
                  emacspeak-we-xsl-linearize-tables t]
                 ["Sort tables"
                  emacspeak-we-xsl-sort-tables t]
                 ["Select default transformation"
                  emacspeak-we-xslt-select t]
                 ["Apply specified transformation"
                  emacspeak-we-xslt-apply t]
                 )))
          t)

;;}}}
;;{{{ tvr: mapping font faces to personalities
(voice-setup-add-map
 '(
   (w3m-italic voice-animate)
   (w3m-insert voice-bolden)
   (w3m-strike-through voice-smoothen-extra)
   (w3m-history-current-url voice-bolden)
   (w3m-current-anchor voice-bolden-extra)
   (w3m-arrived-anchor voice-lighten-extra)
   (w3m-anchor voice-lighten)
   (w3m-bold voice-bolden-medium)
   (w3m-underline voice-brighten-extra)
   (w3m-header-line-location-title voice-bolden)
   (w3m-header-line-location-content voice-animate)
   (w3m-form-button voice-smoothen)
   (w3m-form-button-pressed voice-animate)
   (w3m-tab-unselected voice-monotone)
   (w3m-tab-selected voice-animate-extra)
   (w3m-form voice-brighten)
   (w3m-image voice-brighten)
   ))

(defadvice w3m-mode (after emacspeak pre act comp)
  "Set punctuation mode and refresh punctuations."
  (declare (special dtk-punctuation-mode))
  (setq dtk-punctuation-mode 'some)
  (emacspeak-pronounce-refresh-pronunciations)
  (define-key w3m-mode-map emacspeak-prefix 'emacspeak-prefix-command))

;;}}}
(provide 'emacspeak-w3m)
;;{{{ end of file

;;; emacspeak-w3m.el ends here

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

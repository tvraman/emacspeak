;;;$Id$;;; emacspeak-w3m.el --- speech-enables w3m-el
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
(require 'emacspeak-w3)
(require 'easymenu)
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
(define-key w3m-mode-map "x" 'emacspeak-w3m-xsl-map)
(define-key w3m-mode-map [M-tab] 'w3m-previous-anchor)
(define-key w3m-mode-map [backtab] 'w3m-previous-anchor)
(define-key w3m-mode-map [tab] 'w3m-next-anchor)
(define-key w3m-mode-map [down] 'next-line)
(define-key w3m-mode-map [up] 'previous-line)
(define-key w3m-mode-map [right] 'emacspeak-forward-char)
(define-key w3m-mode-map [left] 'emacspeak-backward-char)
(define-key w3m-mode-map "j" 'emacspeak-w3m-jump-to-title-in-content)
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

;;}}}
;;{{{ jump to title in document

(defun emacspeak-w3m-jump-to-title-in-content ()
  "Jumps to the occurrence of document title in page body."
  (interactive)
  (declare (special w3m-current-title))
  (let ((title (w3m-current-title)))
  (condition-case nil
      (progn
        (goto-char (point-min))
        (goto-char
         (search-forward
          (substring title 0 (min 10 (length title)))))
        (emacspeak-speak-line)
        (emacspeak-auditory-icon 'large-movement))
    (error "Title not found in body."))))
 
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
  (declare (special emacspeak-w3m-form-personality))
  (dtk-speak
   (format "%s input %s  %s"
           type
           name
           (emacspeak-w3m-personalize-string
            (or (emacspeak-w3m-form-get form name) value)
            emacspeak-w3m-form-personality))))

(defun emacspeak-w3m-speak-form-input-checkbox (form name value)
  "Speak checkbox"
  (declare (special emacspeak-w3m-form-personality))
  (dtk-speak
   (format "checkbox %s is %s"
           name
           (emacspeak-w3m-personalize-string
            (if (emacspeak-w3m-form-get form name)
                "on"
              "off")
            emacspeak-w3m-form-personality))))

(defun emacspeak-w3m-speak-form-input-password (form name)
  "Speech-enable password form element."
  (declare (special emacspeak-w3m-form-personality))
  (dtk-speak
   (format "password input %s  %s"
           name
           (emacspeak-w3m-personalize-string
            (emacspeak-w3m-anchor-text)
            emacspeak-w3m-form-personality))))

(defun emacspeak-w3m-speak-form-submit (form &optional name value)
  "Speak submit button."
  (declare (special emacspeak-w3m-button-personality))
  (dtk-speak
   (if (equal value "")
       "submit button"
     (format "button %s"
             (emacspeak-w3m-personalize-string
              value
              emacspeak-w3m-button-personality)))))

(defun emacspeak-w3m-speak-form-input-radio (form name value)
  "speech enable radio buttons."
  (declare (special emacspeak-w3m-form-personality))
  (and dtk-stop-immediately (dtk-stop))
  (let* ((active (equal value (emacspeak-w3m-form-get form name)))
         (personality (if active
                          emacspeak-w3m-form-personality))
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
  (declare (special emacspeak-w3m-form-personality))
  (dtk-speak
   (format "select %s  %s"
           name
           (emacspeak-w3m-personalize-string
            (emacspeak-w3m-anchor-text)
            emacspeak-w3m-form-personality))))

(defun emacspeak-w3m-speak-form-input-textarea (form &optional hseq)
  "speech enable text area."
  (declare (special emacspeak-w3m-form-personality))
  (dtk-speak
   (format "text area %s  %s"
           (or (get-text-property (point) 'w3m-form-name) "")
           (emacspeak-w3m-personalize-string
            (emacspeak-w3m-anchor-text)
            emacspeak-w3m-form-personality))))

(defun emacspeak-w3m-speak-form-reset (form)
  "Reset button."
  (declare (special emacspeak-w3m-button-personality))
  (dtk-speak
   (format "button %s"
           (emacspeak-w3m-personalize-string
            "reset"
            emacspeak-w3m-button-personality))))

;;}}}
;;{{{  advice interactive commands.

(defadvice w3m-goto-url (around emacspeak pre act)
  "Speech-enable W3M."
  (cond
   ((interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (let ((emacspeak-speak-messages nil))
      ad-do-it))
   (t ad-do-it))ad-return-value)

(defadvice w3m-redisplay-this-page (around emacspeak pre act)
  "Speech-enable W3M."
  (cond
   ((interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (let ((emacspeak-speak-messages nil))
      ad-do-it))
   (t ad-do-it))ad-return-value)

(defadvice w3m-reload-this-page (around emacspeak pre act)
  "Speech-enable W3M."
  (cond
   ((interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (let ((emacspeak-speak-messages nil))
      ad-do-it))
   (t ad-do-it))ad-return-value)

(defadvice w3m-print-current-url (after emacspeak pre act comp)
  "Produce auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))

(defadvice w3m-print-this-url (after emacspeak pre act comp)
  "Produce auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))

(defadvice w3m-edit-current-url (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice w3m-edit-this-url (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice w3m-submit-form (after emacspeak pre act comp)
  "Produce auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'button)))

(defadvice w3m-search (after emacspeak pre act comp)
  "Produce auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))

(defadvice w3m-next-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (declare (special w3m-current-title))    
    (emacspeak-auditory-icon 'select-object)
    (if emacspeak-w3m-speak-titles-on-switch 
	(dtk-speak w3m-current-title)
      (emacspeak-speak-mode-line))))

(defadvice w3m-previous-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (declare (special w3m-current-title))
    (emacspeak-auditory-icon 'select-object)
    (if emacspeak-w3m-speak-titles-on-switch 
	(dtk-speak w3m-current-title)
      (emacspeak-speak-mode-line))))

(defadvice w3m-delete-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice w3m-delete-other-buffers (after emacspeak pre act comp)
  "Produce auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)))

(defadvice w3m-bookmark-add-current-url (after emacspeak pre act comp)
  "Produce auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'save-object)))

(defadvice w3m-bookmark-add-this-url (after emacspeak pre act comp)
  "Produce auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'save-object)))

(defadvice w3m-next-anchor (around emacspeak pre act)
  "Speech-enable W3M."
  (cond
   ((interactive-p)
    (let ((emacspeak-speak-messages nil))
      ad-do-it
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-w3m-speak-this-anchor)))
   (t ad-do-it))
  ad-return-value)

(defadvice w3m-previous-anchor (around emacspeak pre act)
  "Speech-enable link navigation."
  (cond
   ((interactive-p)
    (let ((emacspeak-speak-messages nil))
      ad-do-it
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-w3m-speak-this-anchor)))
   (t ad-do-it))
  ad-return-value)

(defadvice w3m-next-image (around emacspeak pre act)
  "Speech-enable W3M."
  (cond
   ((interactive-p)
    (let ((emacspeak-speak-messages nil))
      ad-do-it
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-w3m-speak-this-anchor)))
   (t ad-do-it))
  ad-return-value)

(defadvice w3m-previous-image (around emacspeak pre act)
  "Speech-enable link navigation."
  (cond
   ((interactive-p)
    (let ((emacspeak-speak-messages nil))
      ad-do-it
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-w3m-speak-this-anchor)))
   (t ad-do-it))
  ad-return-value)

(defadvice w3m-next-form (around emacspeak pre act comp)
  "Speech-enable form navigation."
  (cond
   ((interactive-p)
    (let ((emacspeak-speak-messages nil))
      ad-do-it
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-w3m-speak-this-anchor)))
   (t ad-do-it))
  ad-return-value)

(defadvice w3m-previous-form (around emacspeak pre act comp)
  "Speech enable form navigation."
  (cond
   ((interactive-p)
    (let ((emacspeak-speak-messages nil))
      ad-do-it
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-w3m-speak-this-anchor)))
   (t ad-do-it)))

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

(defadvice w3m-view-next-page (around emacspeak pre act)
  "Speech-enable W3M."
  (cond
   ((interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (let ((emacspeak-speak-messages nil))
      ad-do-it))
   (t ad-do-it))ad-return-value)

(defadvice w3m-view-previous-page (around emacspeak pre act)
  "Speech-enable W3M."
  (cond
   ((interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (let ((emacspeak-speak-messages nil))
      ad-do-it))
   (t ad-do-it))ad-return-value)

(defadvice w3m-view-parent-page (around emacspeak pre act)
  "Speech-enable W3M."
  (cond
   ((interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (let ((emacspeak-speak-messages nil))
      ad-do-it))
   (t ad-do-it))ad-return-value)

(defadvice w3m-gohome (around emacspeak pre act)
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

(defadvice w3m-weather (around emacspeak pre act)
  "Speech-enable W3M."
  (cond
   ((interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (let ((emacspeak-speak-messages nil))
      ad-do-it))
   (t ad-do-it))ad-return-value)

(defadvice w3m-scroll-up-or-next-url (around emacspeak pre act comp)
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
  ad-return-value)

(defadvice w3m-scroll-down-or-previous-url (around emacspeak pre act
                                                   comp)
  "Speech-enable scrolling."
  (cond
   ((interactive-p)
    (let ((opoint (save-excursion
                    (end-of-line)
                    (point))))
      ;; hide opoint from advised function
      (let (opoint) ad-do-it)
      (emacspeak-auditory-icon 'scroll)
      (emacspeak-speak-region opoint
                              (save-excursion (beginning-of-line)
                                              (point)))))
   (t ad-do-it))
  ad-return-value)

(defadvice w3m-scroll-left (after emacspeak pre act comp)
  "Produce auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'left)))

(defadvice w3m-scroll-right (after emacspeak pre act comp)
  "Produce auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'right)))

(defadvice w3m-shift-left (after emacspeak pre act comp)
  "Produce auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'left)))

(defadvice w3m-shift-right (after emacspeak pre act comp)
  "Produce auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'right)))

(defadvice w3m-horizontal-recenter (after emacspeak pre act comp)
  "Produce auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)))

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
    (declare (special w3m-current-title))
    (cond
     ((string-match "\\`about://header/" w3m-current-url)
      (message"viewing header information for %s "w3m-current-title  )))))

(defadvice w3m-view-source (after emacspeak pre act comp)
  "Speech enable w3m"
  (when (interactive-p)
    (declare (special w3m-current-title))
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
;;{{{ displaying pages

(add-hook 'w3m-display-hook
          (lambda (url)
	    (declare (special w3m-current-title))
            (emacspeak-auditory-icon 'open-object)
            (when (stringp w3m-current-title)
	      (dtk-speak w3m-current-title)))
          t)

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

(defvar emacspeak-w3m-xsl-once nil
  "XSL transformation for once applying.")

(defun emacspeak-w3m-xslt-apply (xsl)
  "Apply specified transformation to current page."
  (interactive
   (list
    (expand-file-name
     (read-file-name "XSL Transformation: "
                     emacspeak-xslt-directory))))
  (declare (special major-mode
                    emacspeak-w3-xsl-p
                    emacspeak-w3-xsl-transform
                    emacspeak-xslt-directory))
  (unless (eq major-mode 'w3m-mode)
    (error "Not in a W3m buffer."))
  (setq emacspeak-w3m-xsl-once xsl)
  (w3m-redisplay-this-page))

(defun emacspeak-w3m-xslt-perform (xsl-name &optional persistent)
  "Perform XSL transformation by name on the current page
or make it persistent if the second argument is not nil."
  (let ((xsl (expand-file-name (concat xsl-name ".xsl")
                               emacspeak-xslt-directory)))
    (when persistent
      (emacspeak-w3-xslt-select xsl))
    (when (or emacspeak-w3-xsl-p
              (not persistent))
      (emacspeak-w3m-xslt-apply xsl))))

(defun emacspeak-w3m-xsl-add-submit-button (&optional persistent)
  "Add regular submit button if needed.
With prefix arg makes this transformation persistent."
  (interactive "P")
  (emacspeak-w3m-xslt-perform "add-submit-button" persistent))

(defun emacspeak-w3m-xsl-google-hits (&optional persistent)
  "Extracts Google hits from the current page.
With prefix argument makes this transformation persistent."
  (interactive "P")
  (emacspeak-w3m-xslt-perform "google-hits" persistent))

(defun emacspeak-w3m-xsl-linearize-tables (&optional persistent)
  "Linearizes tables on the current page.
With prefix argument makes this transformation persistent."
  (interactive "P")
  (emacspeak-w3m-xslt-perform "linearize-tables" persistent))

(defun emacspeak-w3m-xsl-sort-tables (&optional persistent)
  "Sorts tables on the current page.
With prefix argument makes this transformation persistent."
  (interactive "P")
  (emacspeak-w3m-xslt-perform "sort-tables" persistent))

(defadvice  w3m-create-text-page (before emacspeak pre act comp)
  "Apply requested transform if any before displaying the HTML. "
  (if emacspeak-w3m-xsl-once
      (let ((emacspeak-w3-xsl-p t)
            (emacspeak-w3-xsl-transform emacspeak-w3m-xsl-once))
        (emacspeak-xslt-region
         emacspeak-w3-xsl-transform
         (point-min)
         (point-max)))
    (when (and emacspeak-w3-xsl-p emacspeak-w3-xsl-transform)
      (emacspeak-xslt-region
       emacspeak-w3-xsl-transform
       (point-min)
       (point-max))))
  (setq emacspeak-w3m-xsl-once nil))

;;}}}
;;{{{  xsl keymap

(define-prefix-command 'emacspeak-w3m-xsl-map)

(declaim (special emacspeak-w3m-xsl-map))
(define-key emacspeak-w3m-xsl-map "a" 'emacspeak-w3m-xslt-apply)
(define-key emacspeak-w3m-xsl-map "s" 'emacspeak-w3-xslt-select)
(define-key emacspeak-w3m-xsl-map "o" 'emacspeak-w3-xsl-toggle)
(define-key emacspeak-w3m-xsl-map "b" 'emacspeak-w3m-xsl-add-submit-button)
(define-key emacspeak-w3m-xsl-map "h" 'emacspeak-w3m-xsl-google-hits)
(define-key emacspeak-w3m-xsl-map "l" 'emacspeak-w3m-xsl-linearize-tables)
(define-key emacspeak-w3m-xsl-map "t" 'emacspeak-w3m-xsl-sort-tables)

(add-hook 'w3m-mode-setup-functions
          '(lambda ()
             (easy-menu-define xslt-menu w3m-mode-map
               "XSLT menu"
               '("XSLT transforming"
                 ["Enable default transforming on the fly"
                  emacspeak-w3-xsl-toggle
                  :included (not emacspeak-w3-xsl-p)]
                 ["Disable default transforming on the fly"
                  emacspeak-w3-xsl-toggle
                  :included emacspeak-w3-xsl-p]
                 ["Add regular submit button"
                  emacspeak-w3m-xsl-add-submit-button t]
                 ["Show only search hits"
                  emacspeak-w3m-xsl-google-hits t]
                 ["Linearize tables"
                  emacspeak-w3m-xsl-linearize-tables t]
                 ["Sort tables"
                  emacspeak-w3m-xsl-sort-tables t]
                 ["Select default transformation"
                  emacspeak-w3-xslt-select t]
                 ["Apply specified transformation"
                  emacspeak-w3m-xslt-apply t]
                 )))
          t)

;;}}}
;;{{{ tvr: mapping font faces to personalities 

(def-voice-font  emacspeak-w3m-arrived-anchor-personality
  voice-lighten
  'w3m-arrived-anchor-face
  "w3m-arrived-anchor-face")

(def-voice-font  emacspeak-w3m-anchor-personality
  voice-bolden
  'w3m-anchor-face
  "w3m-anchor-face")

(def-voice-font emacspeak-w3m-bold-personality
  voice-bolden
  'w3m-bold-face
  "w3m-bold-face")

(def-voice-font  emacspeak-w3m-underline-personality
  voice-brighten
  'w3m-underline-face
  "w3m-underline-face")

(def-voice-font  emacspeak-w3m-header-line-location-title-personality
  voice-bolden
  'w3m-header-line-location-title-face
  "w3m-header-line-location-title-face")

(def-voice-font  emacspeak-w3m-header-line-location-content-personality
  voice-animate
  'w3m-header-line-location-content-face
  "w3m-header-line-location-content-face")

(def-voice-font  emacspeak-w3m-button-personality
  voice-smoothen
  'w3m-form-button-face
  "w3m-form-button-face")

(def-voice-font  emacspeak-w3m-form-button-pressed-personality
  voice-animate
  'w3m-form-button-pressed-face
  "w3m-form-button-pressed-face")

(def-voice-font  emacspeak-w3m-tab-unselected-personality
  voice-monotone
  'w3m-tab-unselected-face
  "w3m-tab-unselected-face")

(def-voice-font  emacspeak-w3m-tab-selected-personality
  voice-animate-extra
  'w3m-tab-selected-face
  "w3m-tab-selected-face")
(def-voice-font emacspeak-w3m-form-personality voice-brighten
  'w3m-form-face
  "Personality for forms.")
(def-voice-font emacspeak-w3m-image-personality
  voice-brighten
  'w3m-image-face
  "Image personality.")

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

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
					;(require 'w3m nil t)
;;}}}
;;{{{ keybindings 
(declaim (special w3m-mode-map
                  emacspeak-prefix))(define-key w3m-mode-map emacspeak-prefix 'emacspeak-prefix-command)
(define-key w3m-mode-map [M-tab] 'w3m-previous-anchor)
(define-key w3m-mode-map [backtab] 'w3m-previous-anchor)
(define-key w3m-mode-map [tab] 'w3m-next-anchor)
(define-key w3m-mode-map [down] 'next-line)
(define-key w3m-mode-map [up] 'previous-line)
(define-key w3m-mode-map [right] 'emacspeak-forward-char)
(define-key w3m-mode-map [left] 'emacspeak-backward-char)

;;}}}
;;{{{ helpers

;;; The following definitions through fset are needed because at the
;;; time of compilation w3m-el may be unavailable and corresponding
;;; macros not working.

(defun emacspeak-w3m-anchor ())
(fset 'emacspeak-w3m-anchor
      (byte-compile '(lambda () (w3m-anchor))))

(defun emacspeak-w3m-action ())
(fset 'emacspeak-w3m-action
      (byte-compile '(lambda () (w3m-action))))

(defun emacspeak-w3m-form-get (form name))
(fset 'emacspeak-w3m-form-get
      (byte-compile '(lambda (form name)
		       (w3m-form-get form name))))

(defsubst emacspeak-w3m-personalize-string (string personality)
  (let ((newstring (copy-sequence string)))
    (put-text-property 0 (length newstring)
		       'personality personality
		       newstring)
    newstring))

;;}}}
;;{{{ anchors

(defvar emacspeak-w3m-speak-action-alist
  '((w3m-form-input . emacspeak-w3m-speak-form-input)
    (w3m-form-input-radio . emacspeak-w3m-speak-form-input-radio)
    (w3m-form-input-select . emacspeak-w3m-speak-form-input-select)
    (w3m-form-input-textarea . emacspeak-w3m-speak-form-input-textarea)
    (w3m-form-submit . emacspeak-w3m-speak-form-submit)
    (w3m-form-input-password . emacspeak-w3m-speak-form-input-password)
    (w3m-form-reset . emacspeak-w3m-speak-form-reset))
  )

(defun emacspeak-w3m-anchor-text (&optional default)
  "Return string containing text of anchor under point."
  (save-excursion
    (forward-char 1)
    (if (get-text-property (point) 'w3m-anchor-sequence)
	(message(emacspeak-speak-get-text-range 'w3m-anchor-sequence))
      (message (or default "")))))

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

(defun emacspeak-w3m-speak-form-input-textarea (form hseq)
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
    (let ((emacspeak-speak-messages nil))
      ad-do-it)
    (emacspeak-auditory-icon 'open-object)
    (when (stringp w3m-current-title)
      (message "%s" w3m-current-title)))
   (t ad-do-it))ad-return-value)

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

(defadvice w3m-next-form (around emacspeak pre act comp)
  "Speech-enable form navigation."
  (cond
   ((interactive-p)
    (let ((emacspeak-speak-messages nil))
      ad-do-it)
    (when (interactive-p)
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-w3m-speak-this-anchor)))
   (t ad-do-it))
  ad-return-value)

(defadvice w3m-previous-form (around emacspeak pre act comp)
  "Speech enable form navigation."
  (cond
   ((interactive-p)
    (let ((emacspeak-speak-messages nil))
      ad-do-it)
    (when (interactive-p)
      (emacspeak-w3m-speak-this-anchor)
      (emacspeak-auditory-icon 'large-movement)))
   (t ad-do-it)))

(defadvice w3m-view-this-url (around emacspeak pre act comp)
  "Speech-enable W3M."
  (cond
   ((interactive-p)
    (let ((url (emacspeak-w3m-anchor))
          (act (emacspeak-w3m-action)))
      ad-do-it
      (when (and (interactive-p)
                 (not url)
                 (consp act)
                 (memq (car act)
                       '(w3m-form-input
                         w3m-form-input-radio
                         w3m-form-input-password)))
        (emacspeak-w3m-speak-this-anchor))
      (emacspeak-auditory-icon 'select-object)))
   (t ad-do-it))
  ad-return-value)

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

(defadvice w3m (after emacspeak pre act comp)
  (when (and (interactive-p)
	     (eq (ad-get-arg 0) 'popup))
    (emacspeak-speak-mode-line)))

(defadvice w3m-close-window (after emacspeak pre act comp)
  "Produce auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice w3m-quit (after emacspeak pre act comp)
  "Produce auditory feedback."
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
  (when (and (interactive-p) (w3m-cursor-anchor))
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

(defadvice  w3m-w3m-dump-head-source (after emacspeak pre act comp)
  "Apply requested transform if any after grabbing the HTML. "
  (require 'emacspeak-w3)
  (when (and emacspeak-w3-xsl-p emacspeak-w3-xsl-transform)
    (emacspeak-xslt-region
     emacspeak-w3-xsl-transform
     (point-min)
     (point-max))))

;;}}}
;;{{{ tvr: mapping font faces to personalities 

(def-voice-font  w3m-arrived-anchor-personality
  voice-lighten
  'w3m-arrived-anchor-face
  "w3m-arrived-anchor-face")

(def-voice-font  w3m-anchor-personality
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

(def-voice-font  w3m-header-line-location-content-personality
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
  "Set punctuation mode."
  (declare (special dtk-punctuation-mode))
  (setq dtk-punctuation-mode 'some)
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

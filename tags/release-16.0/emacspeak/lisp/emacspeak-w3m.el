;;;$Id$;;; emacspeak-w3m.el --- speech-enables w3m-el
;;; This file is not part of Emacspeak, but the same terms and
;;; conditions apply.
;; Copyright (C) 2001,2002  Dimitri V. Paduchih

;; Author: Dimitri Paduchih <paduch@imm.uran.ru>
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


;;; Code:
;;{{{  required modules

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'advice)
(require 'w3m nil t)
(require 'w3m-form nil t)
(require 'dtk-speak)
(require 'emacspeak-speak)
(require 'voice-lock)
(require 'emacspeak-sounds)
(require 'emacspeak-wizards)

;;}}}
;;{{{ keybindings 
(declaim (special w3m-mode-map))
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
;;{{{ personalities

(defvar emacspeak-w3m-form-personality 'paul-animated)
(defvar emacspeak-w3m-button-personality 'harry)
(defvar emacspeak-w3m-disabled-personality 'harry)

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

(defun emacspeak-w3m-speak-form-input (form name type width maxlength value)
  (dtk-speak
   (format "%s input %s  %s"
	   type
	   name
	   (emacspeak-w3m-personalize-string
	    (or (emacspeak-w3m-form-get form name) value)
	    emacspeak-w3m-form-personality))))

(defun emacspeak-w3m-speak-form-input-password (form name)
  "Speech-enable password form element."
  (dtk-speak
   (format "password input %s  %s"
	   name
	   (emacspeak-w3m-personalize-string
	    (emacspeak-w3m-anchor-text)
	    emacspeak-w3m-form-personality))))

(defun emacspeak-w3m-speak-form-submit (form &optional name value)
  (dtk-speak
   (if (equal value "")
       "submit button"
     (format "button %s"
	     (emacspeak-w3m-personalize-string
	      value
	      emacspeak-w3m-button-personality)))))

(defun emacspeak-w3m-speak-form-input-radio (form name value)
  (and dtk-stop-immediately (dtk-stop))
  (let* ((active (equal value (emacspeak-w3m-form-get form name)))
	 (personality (if active
			  emacspeak-w3m-form-personality
			emacspeak-w3m-disabled-personality))
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
  (dtk-speak
   (format "select %s  %s"
	   name
	   (emacspeak-w3m-personalize-string
	    (emacspeak-w3m-anchor-text)
	    emacspeak-w3m-form-personality))))

(defun emacspeak-w3m-speak-form-input-textarea (form hseq)
  (dtk-speak
   (format "text area %s  %s"
	   (or (get-text-property (point) 'w3m-form-name) "")
	   (emacspeak-w3m-personalize-string
	    (emacspeak-w3m-anchor-text)
	    emacspeak-w3m-form-personality))))

(defun emacspeak-w3m-speak-form-reset (form)
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
      ad-do-it)
    (when (interactive-p)
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-w3m-speak-this-anchor)))
   (t ad-do-it))
  ad-return-value)

(defadvice w3m-previous-anchor (around emacspeak pre act)
  "Speech-enable link navigation."
  (cond
   ((interactive-p)
    (let ((emacspeak-speak-messages nil))
      ad-do-it)
    (when (interactive-p)
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

(defvar emacspeak-w3m-xsl-p nil
  "*T means we apply XSL transformation before displaying HTML.")

(defvar emacspeak-w3m-xsl-transform nil
  "Specifies transform to use before displaying a page.
Nil means no transform is used.")

(defadvice  w3m-w3m-dump-source (after emacspeak pre act comp)
  "Apply requested transform if any after grabbing the HTML. "
  (when (and emacspeak-w3m-xsl-p emacspeak-w3m-xsl-transform)
    (emacspeak-xslt-region
     emacspeak-w3m-xsl-transform
     (point-min)
     (point-max))))

(defadvice  w3m-w3m-dump-head-source (after emacspeak pre act comp)
  "Apply requested transform if any after grabbing the HTML. "
  (when (and emacspeak-w3m-xsl-p emacspeak-w3m-xsl-transform)
    (emacspeak-xslt-region
     emacspeak-w3m-xsl-transform
     (point-min)
     (point-max))))
(defun emacspeak-w3m-xslt-select (xsl)
  "Select transformation to apply."
  (interactive
   (list
    (expand-file-name
     (read-file-name "XSL Transformation: "
                     emacspeak-xslt-directory))))
  (declare (special emacspeak-w3m-xsl-transform))
  (setq emacspeak-w3m-xsl-transform xsl
	emacspeak-w3m-xsl-p	    t)
  (message "Will apply %s before displaying HTML pages."
           (file-name-sans-extension
            (file-name-nondirectory
             xsl)))
  (emacspeak-auditory-icon 'select-object))

(defun emacspeak-w3m-xsl-toggle ()
  "Toggle  XSL transformations before displaying HTML.
This uses XSLT Processor xsltproc available as part of the
libxslt package."
  (interactive)
  (declare (special emacspeak-w3m-xsl-p))
  (setq emacspeak-w3m-xsl-p
        (not emacspeak-w3m-xsl-p))
  (emacspeak-auditory-icon
   (if emacspeak-w3m-xsl-p 'on 'off))
  (message "Turned %s XSL"
           (if emacspeak-w3m-xsl-p 'on 'off)))
;;}}}
;;{{{ tvr: mapping font faces to personalities 

   
        
  


(voice-setup-set-voice-for-face 'w3m-arrived-anchor-face 'betty)
(voice-setup-set-voice-for-face 'w3m-anchor-face 'harry)
(voice-setup-set-voice-for-face 'w3m-bold-face 'bold)
(voice-setup-set-voice-for-face 'w3m-underline-face 'underlined)
(voice-setup-set-voice-for-face 'w3m-header-line-location-title-face
                                'harry)
(voice-setup-set-voice-for-face 'w3m-header-line-location-content-face
                                'paul-animated)
(voice-setup-set-voice-for-face 'w3m-form-button-face
                                'paul-smooth)
(voice-setup-set-voice-for-face 'w3m-form-button-pressed-face
                                'paul-animated)
(voice-setup-set-voice-for-face 'w3m-tab-unselected-face
                                'paul-monotone)
(voice-setup-set-voice-for-face 'w3m-tab-selected-face 'paul-animated)

(defun emacspeak-w3m-voiceify-faces-in-buffer ()
  "Map base fonts to voices."
  (interactive )
  (declare (special voice-lock-mode))
  (setq voice-lock-mode t)
  (voice-setup-face-to-voice (point-min) (point-max)))

(defadvice w3m-mode (after emacspeak pre act comp)
  "Set punctuation mode."
  (declare (special dtk-punctuation-mode))
  (setq dtk-punctuation-mode "some"))

(add-hook 'w3m-fontify-after-hook 'emacspeak-w3m-voiceify-faces-in-buffer)



;;}}}
(provide 'emacspeak-w3m)
;;{{{ end of file 

;;; emacspeak-w3m.el ends here

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

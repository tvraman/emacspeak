;;; emacspeak-speedbar.el --- Speech enable speedbar -- Tool for context-sensitive navigation
;;; $Id$
;;; $Author$ 
;;; Description: Auditory interface to speedbar
;;; Keywords: Emacspeak, Speedbar
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

;;; Copyright (c) 1995 -- 2003, T. V. Raman
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

;;{{{  Introduction

;;; Commentary:

;;; This module advises speedbar.el for use with Emacs.  The
;;; latest speedbar can be obtained from
;;; ftp://ftp.ultranet.com/pub/zappo/ This module ensures
;;; that speedbar works smoothly outside a windowing system
;;; in addition to speech enabling all interactive
;;; commands. Emacspeak also adds an Emacspeak environment
;;; specific entry point to speedbar
;;; --emacspeak-speedbar-goto-speedbar-- and binds this

;;; Code:

;;}}}
;;{{{  Required modules

(require 'emacspeak-preamble)
(require 'speedbar nil t)
;;}}}
;;{{{ custom

(defgroup emacspeak-speedbar nil
  "Speedbar on the Emacspeak audio desktop."
  :group 'emacspeak
  :group 'speedbar
  :prefix "emacspeak-speedbar")

;;}}}
;;{{{  work outside a windowing system

                                        ;(defadvice speedbar-frame-mode (around emacspeak pre act comp)
                                        ;"Wrapper to force speedbar to work outside a windowing system. "
                                        ;(let ((spoofing-p (not window-system)))
                                        ;ad-do-it
                                        ;     (setq voice-lock-mode t))
                                        ;   ad-return-value)

;;}}}
;;{{{ Helper:

(defun emacspeak-speedbar-speak-line()
  "Speak a line in the speedbar display"
  (let ((indent nil))  
    (save-excursion
      (beginning-of-line)
      (setq indent 
            (save-excursion
              (save-match-data
                (beginning-of-line)
                (string-to-number
                 (if (looking-at "[0-9]+")
                     (buffer-substring-no-properties
                      (match-beginning 0) (match-end 0))
                   "0")))))
      (setq indent 
            (if (zerop indent) "" indent))
      (dtk-speak 
       (concat indent 
               (thing-at-point 'line))))))

;;}}}
;;{{{ Advice interactive commands:

(defadvice speedbar-close-frame (after emacspeak pre act comp)
  "Cue buffer that becomes active"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice speedbar-next (around emacspeak pre act comp)
  "Provide reasonable spoken feedback"
  (cond
   ((interactive-p)
    (let ((emacspeak-speak-messages nil))
      ad-do-it
      (emacspeak-speedbar-speak-line)
      (emacspeak-auditory-icon 'select-object)))
   (t ad-do-it))
  ad-return-value)
(defadvice speedbar-prev (around emacspeak pre act comp)
  "Provide reasonable spoken feedback"
  (cond
   ((interactive-p)
    (let ((emacspeak-speak-messages nil))
      ad-do-it
      (emacspeak-speedbar-speak-line)
      (emacspeak-auditory-icon 'select-object)))
   (t ad-do-it))
  ad-return-value)
(defadvice speedbar-edit-line (after emacspeak pre act comp)
  "Speak line you jumped to"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice speedbar-tag-find (after emacspeak pre act comp)
  "Speak the line you jumped to"
  (emacspeak-speedbar-speak-line))

(defadvice speedbar-find-file (after emacspeak pre act comp)
  "Speak modeline of buffer we switched to"
  (emacspeak-auditory-icon 'select-object)
  (emacspeak-speak-mode-line))

(defadvice speedbar-expand-line (after emacspeak pre act
                                       comp)
  "Speak the line we just expanded"
  (when (interactive-p) 
    (emacspeak-speedbar-speak-line)
    (emacspeak-auditory-icon 'open-object)))

(defadvice speedbar-contract-line (after emacspeak pre act comp)
  "Speak the line we just contracted"
  (when (interactive-p) 
    (emacspeak-speedbar-speak-line)
    (emacspeak-auditory-icon 'close-object)))

(defadvice speedbar-up-directory (around emacspeak pre act comp)
  " Auditory icon and speech feedback indicate result of the
action"
  (cond
   ((interactive-p)
    ad-do-it
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speedbar-speak-line))
   (t ad-do-it))
  ad-return-value)

(defadvice speedbar-restricted-next (after emacspeak pre act
                                           comp)
  "Provide spoken feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speedbar-speak-line)))

(defadvice speedbar-restricted-prev (after emacspeak pre act
                                           comp)
  "Provide spoken feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speedbar-speak-line)))

;;}}}
;;{{{ additional navigation

(defvar emacspeak-speedbar-disable-updates t
  "Non nil means speedbar does not automatically update.
An automatically updating speedbar consumes resources.")

(defun emacspeak-speedbar-goto-speedbar ()
  "Switch to the speedbar"
  (interactive)
  (declare (special emacspeak-speedbar-disable-updates))
  (unless (get-buffer " SPEEDBAR")
    (speedbar-frame-mode))
  (pop-to-buffer (get-buffer " SPEEDBAR"))
  (set-window-dedicated-p (selected-window) nil)
  (setq voice-lock-mode t)
  (when emacspeak-speedbar-disable-updates 
    (speedbar-stealthy-updates)
    (speedbar-disable-update))
  (emacspeak-auditory-icon 'select-object)
  (dtk-speak
   (concat "Speedbar: "
           (let ((start nil))
             (save-excursion 
               (beginning-of-line)
               (setq start (point))
               (end-of-line)
               (buffer-substring start (point)))))))
  
  

(defun emacspeak-speedbar-click ()
  "Does the equivalent of the mouse click from the keyboard"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((target
           (if (get-text-property (point) 'speedbar-function)
               (point)
             (next-single-property-change (point)
                                          'speedbar-function)))
          (action-char nil))
      (cond 
       (target (goto-char target)
               (speedbar-do-function-pointer)
               (forward-char 1)
               (setq action-char (following-char))
               (emacspeak-speedbar-speak-line)
               (emacspeak-auditory-icon
                (case action-char
                  (?+ 'open-object)
                  (?- 'close-object)
                  (t 'large-movement))))
       (t (message "No target on this line"))))))

;;}}}
;;{{{  hooks
(declaim (special speedbar-key-map))
(eval-when (load)
  
  (emacspeak-keymap-remove-emacspeak-edit-commands speedbar-key-map))
(defun emacspeak-speedbar-enter-hook ()
  "Actions taken when we enter the Speedbar"
  (declare (special speedbar-key-map
                    speedbar-hide-button-brackets-flag))
  (dtk-set-punctuations "all")
  (setq speedbar-hide-button-brackets-flag t)
  (define-key speedbar-key-map "f"
    'emacspeak-speedbar-click)
                                        ;(define-key speedbar-key-map "\M-n"
                                        ;'emacspeak-speedbar-forward)
                                        ;(define-key speedbar-key-map "\M-p"
                                        ;'emacspeak-speedbar-backward)
  )

(add-hook 'speedbar-mode-hook
          'emacspeak-speedbar-enter-hook)

;;}}}
;;{{{  voice locking 
;;; Map speedbar faces to voices
;;
(defcustom emacspeak-speedbar-button-personality  voice-bolden
  "personality used for speedbar buttons"
  :type 'symbol
  :group 'emacspeak-speedbar)

(defcustom emacspeak-speedbar-selected-personality  voice-animate
  "Personality used to indicate speedbar selection"
  :type 'symbol
  :group 'emacspeak-speedbar)

(defcustom emacspeak-speedbar-directory-personality voice-lock-function-name-personality
  "Speedbar personality for directory buttons"
  :type 'symbol
  :group 'emacspeak-speedbar)

(defcustom emacspeak-speedbar-file-personality  'paul
  "Personality used for file buttons"
  :type 'symbol
  :group 'emacspeak-speedbar)

(defcustom emacspeak-speedbar-highlight-personality voice-animate
  "Personality used for for speedbar highlight."
  :type 'symbol
  :group 'emacspeak-speedbar)

(defcustom emacspeak-speedbar-tag-personality voice-monotone
  "Personality used for speedbar tags"
  :type 'symbol
  :group 'emacspeak-speedbar)

(defcustom emacspeak-speedbar-default-personality 'paul
  "Default personality used in speedbar buffers"
  :type 'symbol
  :group 'emacspeak-speedbar)

(defadvice speedbar-make-button (after emacspeak pre act comp)
  "Voiceify the button"
  (let ((start (ad-get-arg 0))
        (end (ad-get-arg 1))
        (face (ad-get-arg 2))
        (personality nil))
    (setq personality
          (cond
           ((eq face 'speedbar-button-face )
            emacspeak-speedbar-button-personality  )
           ((eq face 'speedbar-selected-face )
            emacspeak-speedbar-selected-personality  )
           ((eq face 'speedbar-directory-face )
            emacspeak-speedbar-directory-personality )
           ((eq face 'speedbar-file-face )
            emacspeak-speedbar-file-personality  )
           ((eq face 'speedbar-highlight-face )
            emacspeak-speedbar-highlight-personality )
           ((eq face 'speedbar-tag-face )
            emacspeak-speedbar-tag-personality )
           (t 'emacspeak-speedbar-default-personality)))
    (put-text-property start end 'personality personality)
    (save-excursion
      (save-match-data
        (beginning-of-line)))))

;;}}}
;;{{{ keys 
(declaim (special emacspeak-keymap))

					;(define-key emacspeak-keymap '[insert] 'emacspeak-speedbar-goto-speedbar)

;;}}}
(provide 'emacspeak-speedbar)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

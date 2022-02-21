;;; emacspeak-speedbar.el --- Speech enable speedbar -- Tool for context-sensitive navigation  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $ 
;;; Description: Auditory interface to speedbar
;;; Keywords: Emacspeak, Speedbar
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com 
;;; A speech interface to Emacs |
;;; $Date: 2008-06-21 10:50:41 -0700 (Sat, 21 Jun 2008) $ |
;;;  $Revision: 4532 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (c) 1995 -- 2021, T. V. Raman
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

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

(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'speedbar "speedbar" 'no-error)
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
       (concat indent (ems--this-line))))))

;;}}}
;;{{{ Advice interactive commands:

(defadvice speedbar-close-frame (after emacspeak pre act comp)
  "Cue buffer that becomes active"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice speedbar-next (around emacspeak pre act comp)
  "Provide reasonable spoken feedback"
  (cond
   ((ems-interactive-p)
    (let ((emacspeak-speak-messages nil))
      ad-do-it
      (emacspeak-speedbar-speak-line)
      (emacspeak-auditory-icon 'select-object)))
   (t ad-do-it))
  ad-return-value)
(defadvice speedbar-prev (around emacspeak pre act comp)
  "Provide reasonable spoken feedback"
  (cond
   ((ems-interactive-p)
    (let ((emacspeak-speak-messages nil))
      ad-do-it
      (emacspeak-speedbar-speak-line)
      (emacspeak-auditory-icon 'select-object)))
   (t ad-do-it))
  ad-return-value)
(defadvice speedbar-edit-line (after emacspeak pre act comp)
  "Speak line you jumped to"
  (when (ems-interactive-p)
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
  (when (ems-interactive-p) 
    (emacspeak-speedbar-speak-line)
    (emacspeak-auditory-icon 'open-object)))

(defadvice speedbar-contract-line (after emacspeak pre act comp)
  "Speak the line we just contracted"
  (when (ems-interactive-p) 
    (emacspeak-speedbar-speak-line)
    (emacspeak-auditory-icon 'close-object)))

(defadvice speedbar-up-directory (around emacspeak pre act comp)
  " Auditory icon and speech feedback indicate result of the
action"
  (cond
   ((ems-interactive-p)
    ad-do-it
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speedbar-speak-line))
   (t ad-do-it))
  ad-return-value)

(defadvice speedbar-restricted-next (after emacspeak pre act
                                           comp)
  "Speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speedbar-speak-line)))

(defadvice speedbar-restricted-prev (after emacspeak pre act
                                           comp)
  "Speak"
  (when (ems-interactive-p)
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
  (cl-declare (special emacspeak-speedbar-disable-updates))
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
                (cl-case action-char
                  (?+ 'open-object)
                  (?- 'close-object)
                  (t 'large-movement))))
       (t (message "No target on this line"))))))

;;}}}
;;{{{  hooks
(cl-declaim (special speedbar-mode-map))
(cl-eval-when (load)
  )
(defun emacspeak-speedbar-enter-hook ()
  "Actions taken when we enter the Speedbar"
  (cl-declare (special speedbar-mode-map
                       speedbar-hide-button-brackets-flag))
  (dtk-set-punctuations 'all)
  (setq speedbar-hide-button-brackets-flag t)
  (define-key speedbar-mode-map "f"
    'emacspeak-speedbar-click)
                                        ;(define-key speedbar-mode-map "\M-n"
                                        ;'emacspeak-speedbar-forward)
                                        ;(define-key speedbar-mode-map "\M-p"
                                        ;'emacspeak-speedbar-backward)
  )

(add-hook 'speedbar-mode-hook
          'emacspeak-speedbar-enter-hook)

;;}}}
;;{{{  voice locking 
;;; Map speedbar faces to voices
;;
(defvar emacspeak-speedbar-button-personality  voice-bolden
  "personality used for speedbar buttons")

(defvar emacspeak-speedbar-selected-personality  voice-animate
  "Personality used to indicate speedbar selection")

(defvar emacspeak-speedbar-directory-personality voice-bolden-medium
  "Speedbar personality for directory buttons"
  )

(defvar emacspeak-speedbar-file-personality  'paul
  "Personality used for file buttons")

(defvar emacspeak-speedbar-highlight-personality voice-animate
  "Personality used for for speedbar highlight.")

(defvar emacspeak-speedbar-tag-personality voice-monotone-extra
  "Personality used for speedbar tags")

(defvar emacspeak-speedbar-default-personality 'paul
  "Default personality used in speedbar buffers")

(defadvice speedbar-make-button (after emacspeak pre act comp)
  "Voiceify the button"
  (let ((start (ad-get-arg 0))
        (end (ad-get-arg 1))
        (face (ad-get-arg 2))
        (personality nil))
    (setq personality
          (cond
           ((eq face 'speedbar-button-face)
            emacspeak-speedbar-button-personality)
           ((eq face 'speedbar-selected-face)
            emacspeak-speedbar-selected-personality)
           ((eq face 'speedbar-directory-face)
            emacspeak-speedbar-directory-personality)
           ((eq face 'speedbar-file-face)
            emacspeak-speedbar-file-personality)
           ((eq face 'speedbar-highlight-face)
            emacspeak-speedbar-highlight-personality)
           ((eq face 'speedbar-tag-face)
            emacspeak-speedbar-tag-personality)
           (t 'emacspeak-speedbar-default-personality)))
    (put-text-property start end 'personality personality)
    (save-excursion
      (save-match-data
        (beginning-of-line)))))

;;}}}
;;{{{ keys 
(cl-declaim (special emacspeak-keymap))

                                        ;(define-key emacspeak-keymap '[insert] 'emacspeak-speedbar-goto-speedbar)

;;}}}
(provide 'emacspeak-speedbar)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}

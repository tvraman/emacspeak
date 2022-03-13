;;; emacspeak-man.el --- Speech enable Man mode -- Use this for UNIX Man pages  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $ 
;;; DescriptionEmacspeak extensions for man-mode
;;; Keywords:emacspeak, audio interface to emacs man 
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com 
;;; A speech interface to Emacs |
;;; $Date: 2008-06-21 10:50:41 -0700 (Sat, 21 Jun 2008) $ |
;;;  $Revision: 4546 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2021, T. V. Raman 
;;; Copyright (c) 1995 by T. V. Raman 
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

;;{{{  Introduction:
;;; Commentary:
;;; Provide additional advice to man-mode 
;;; Code:
;;; Code:

;;}}}
;;{{{ Required modules

;;; Code:
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'voice-setup)
(require 'man)

;;}}}
;;{{{  Configure man

(cl-declaim (special Man-switches system-type))

(when (eq system-type 'gnu/linux)
  (setq Man-switches "-a"))

;;}}}
;;{{{ Map Faces:

(voice-setup-add-map
 '(
   (Man-overstrike  voice-bolden-medium)
   (Man-reverse voice-animate)
   (Man-underline voice-lighten)))

;;}}}
;;{{{  advice interactive commands 

(defadvice  Man-mode (after emacspeak pre act comp)
  "Fixup variables paragraph-start and paragraph-separate.
Also provide an auditory icon"
  (setq paragraph-start "^[\011\012\014]*$"
        paragraph-separate "^[\011\012\014]*$")
  (modify-syntax-entry 10 " ")
  (setq imenu-generic-expression
        '((nil "\n\\([A-Z].*\\)" 1)     ; SECTION, but not TITLE
          ("*Subsections*" "^   \\([A-Z].*\\)" 1)))
  (dtk-set-punctuations 'all)
  (emacspeak-pronounce-refresh-pronunciations)
  (emacspeak-auditory-icon 'help))

(defadvice   Man-goto-section  (after emacspeak pre act comp)
  "Speak the line"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'section)
    (emacspeak-speak-line)))

(defadvice   Man-goto-page  (after emacspeak pre act comp)
  "Speak the line"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice   Man-next-manpage  (after emacspeak pre act comp)
  "Speak the line"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice   Man-previous-manpage  (after emacspeak pre act comp)
  "Speak the line"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice Man-next-section (after emacspeak pre act comp)
  "Speak the line"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'section)
    (emacspeak-speak-line)))

(defadvice Man-previous-section (after emacspeak pre act comp)
  "Speak the line"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'section)
    (emacspeak-speak-line)))

(defadvice Man-goto-see-also-section (after emacspeak pre act comp)
  "Speak the line"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice Man-quit (after emacspeak pre act comp)
  "Announce buffer that is current"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice Man-kill (after emacspeak pre act comp)
  "Announce buffer that is current"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice man (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{  Additional commands

(defun emacspeak-man-speak-this-section ()
  "Speak current section"
  (interactive)
  (save-excursion
    (let ((start (point))
          (end nil))
      (condition-case nil
          (progn
            (Man-next-section 1)
            (setq end (point)))
        (error (setq end (point-max))))
      (emacspeak-auditory-icon 'section)
      (emacspeak-speak-region start end))))

(defun emacspeak-man-browse-man-page ()
  "Browse the man page --read it a paragraph at a time"
  (interactive)
  (emacspeak-execute-repeatedly 'forward-paragraph))

(autoload 'emacspeak-view-line-to-top 
  "emacspeak-view" "Move current line to top of window"  t)

(cl-declaim (special  Man-mode-map))
(define-key Man-mode-map ";"
  'emacspeak-speak-current-window)
(define-key Man-mode-map "\M-j" 'imenu)
(define-key Man-mode-map "\M- " 'emacspeak-man-speak-this-section)
(define-key Man-mode-map "." 'emacspeak-man-browse-man-page)
(define-key Man-mode-map "t" 'emacspeak-view-line-to-top)
(define-key Man-mode-map "'" 'emacspeak-speak-rest-of-buffer)
(define-key Man-mode-map "N" 'emacspeak-speak-face-forward)
(define-key Man-mode-map "P" 'emacspeak-speak-face-backward)
(define-key Man-mode-map "[" 'backward-paragraph)
(define-key Man-mode-map "]" 'forward-paragraph)

;;}}}
(provide  'emacspeak-man)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}

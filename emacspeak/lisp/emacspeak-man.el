;;; emacspeak-man.el --- Speech enable Man mode -- Use this for UNIX Man pages
;;; $Id$
;;; $Author$ 
;;; DescriptionEmacspeak extensions for man-mode
;;; Keywords:emacspeak, audio interface to emacs man 
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
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}

;;{{{  Introduction:

;;; Provide additional advice to man-mode 

;;; Code:

;;}}}
;;{{{ Required modules

;;; Code:
(require 'emacspeak-preamble)
(require 'man)
;;}}}
;;{{{  Configure man

;;; Please show it to me when you are ready:
(declaim (special Man-notify
                  Man-switches
                  system-type))
(setq Man-notify 'bully)

(when (eq system-type 'gnu/linux)
  (setq Man-switches "-a"))

;;}}}
;;{{{  advice interactive commands 

(defadvice  Man-mode (after emacspeak pre act )
  "Fixup variables paragraph-start and paragraph-separate.
Also provide an auditory icon"
  (setq paragraph-start "^[\011\012\014]*$"
        paragraph-separate "^[\011\012\014]*$")
  (modify-syntax-entry 10 " ")
  (setq imenu-generic-expression
        '((nil "\n\\([A-Z].*\\)" 1)     ; SECTION, but not TITLE
          ("*Subsections*" "^   \\([A-Z].*\\)" 1)))
  (voice-lock-mode 1)
  (dtk-set-punctuations 'all)
  (emacspeak-pronounce-refresh-pronunciations)
  (emacspeak-auditory-icon 'help))

(defadvice   Man-goto-section  (after emacspeak pre act )
  "Speak the line"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice   Man-goto-page  (after emacspeak pre act )
  "Speak the line"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))
(defadvice   Man-next-manpage  (after emacspeak pre act )
  "Speak the line"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))
(defadvice   Man-previous-manpage  (after emacspeak pre act )
  "Speak the line"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice Man-next-section (after emacspeak pre act )
  "Speak the line"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice Man-previous-section (after emacspeak pre act )
  "Speak the line"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice Man-goto-see-also-section (after emacspeak pre act )
  "Speak the line"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice Man-quit (after emacspeak pre act )
  "Announce buffer that is current"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line )))

(defadvice manual-entry (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-speak-mode-line )))

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
      (emacspeak-auditory-icon 'select-object)
      (emacspeak-speak-region start end ))))

(defun emacspeak-man-browse-man-page ()
  "Browse the man page --read it a paragraph at a time"
  (interactive)
  (emacspeak-execute-repeatedly 'forward-paragraph))
(autoload 'emacspeak-view-line-to-top 
  "emacspeak-view" "Move current line to top of window"  t)
(declaim (special Man-mode-map))
(eval-when (load)
  (emacspeak-keymap-remove-emacspeak-edit-commands Man-mode-map))
(declaim (special  Man-mode-map))
(define-key Man-mode-map ";"
  'emacspeak-speak-current-window)
(define-key Man-mode-map "\M-j" 'imenu)
(define-key Man-mode-map "\M- " 'emacspeak-man-speak-this-section)
(define-key Man-mode-map "." 'emacspeak-man-browse-man-page)
(define-key Man-mode-map "t" 'emacspeak-view-line-to-top)
(define-key Man-mode-map "'" 'emacspeak-speak-rest-of-buffer)
(define-key Man-mode-map "[" 'backward-paragraph)
(define-key Man-mode-map "]" 'forward-paragraph)

;;}}}
(provide  'emacspeak-man)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

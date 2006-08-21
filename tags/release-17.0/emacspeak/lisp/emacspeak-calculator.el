;;; emacspeak-calculator.el --- Speech enable  desktop calculator
;;; $Id$
;;; $Author$
;;; Description:   extension to speech enable desktop calculator
;;; Keywords: Emacspeak, Audio Desktop
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

;;; Copyright (C) 1995 -- 2002, T. V. Raman<raman@cs.cornell.edu>
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

;;{{{ required modules

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-speak)
(require 'emacspeak-sounds)

;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; Speech enable desktop calculator 

;;; Code:

;;}}}
;;{{{  helpers 

(defun emacspeak-calculator-summarize ()
  "Summarize state of the calculator"
  (emacspeak-speak-line))

;;}}}
;;{{{  advice interactive commands 

(defadvice calculator (after emacspeak pre act comp)
  "Speech enable calculator."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Welcome to the pocket calculator.")))

(defadvice calculator-digit (around emacspeak pre act comp)
  "Speak the digit."
  (cond
   ((interactive-p)
    (let ((start (point)))
      ad-do-it
      (emacspeak-speak-region start (point))))
   (t ad-do-it))
  ad-return-value)

(defadvice calculator-exp (around emacspeak pre act comp)
  "Speak the digit."
  (cond
   ((interactive-p)
    (let ((start (point)))
      ad-do-it
      (emacspeak-speak-region start (point))))
   (t ad-do-it))
  ad-return-value)

(defadvice calculator-op (around emacspeak pre act comp)
  "Speak the digit."
  (cond
   ((interactive-p)
    ad-do-it
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-calculator-summarize))
   (t ad-do-it))
  ad-return-value)
(defadvice calculator-op-or-exp (around emacspeak pre act comp)
  "Speak the digit."
  (cond
   ((interactive-p)
    (let ((start (point)))
      ad-do-it
      (emacspeak-speak-region start (point))))
   (t ad-do-it))
  ad-return-value)
(defadvice calculator-open-paren (around emacspeak pre act comp)
  "Speak the digit."
  (cond
   ((interactive-p)
    (let ((start (point)))
      ad-do-it
      (emacspeak-speak-region start (point))))
   (t ad-do-it))
  ad-return-value)

(defadvice calculator-close-paren (around emacspeak pre act comp)
  "Speak the digit."
  (cond
   ((interactive-p)
    (let ((start (point)))
      ad-do-it
      (emacspeak-speak-region start (point))))
   (t ad-do-it))
  ad-return-value)

(defadvice calculator-saved-up (around emacspeak pre act comp)
  "Speak the digit."
  (cond
   ((interactive-p)
    ad-do-it
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-calculator-summarize))
   (t ad-do-it))
  ad-return-value)

(defadvice calculator-saved-down (around emacspeak pre act comp)
  "Speak the digit."
  (cond
   ((interactive-p)
    ad-do-it
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-calculator-summarize))
   (t ad-do-it))
  ad-return-value)

(defadvice calculator-save-on-list (after emacspeak pre act
                                          comp)
  "Provide speech feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'save-object)
    (emacspeak-calculator-summarize)))

(defadvice calculator-clear-saved (after emacspeak pre act
                                         comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-calculator-summarize)))

(defadvice calculator-enter (after emacspeak pre act comp)
  "Provide speech feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-calculator-summarize)))

(defadvice calculator-backspace (around emacspeak pre act)
  "Speak character you're deleting."
  (cond
   ((interactive-p )
    (dtk-tone 500 30 'force)
    (and emacspeak-backward-delete-char-speak-deleted-char
         (emacspeak-speak-this-char (preceding-char )))
    ad-do-it
    (and emacspeak-backward-delete-char-speak-current-char
         (emacspeak-speak-this-char (preceding-char))))
   (t ad-do-it))
  ad-return-value)

(defadvice calculator-clear (after emacspeak pre act
				   comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-calculator-summarize)))
(defadvice calculator-copy (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-current-kill 1)))

(defadvice calculator-paste (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object)))

(defadvice calculator-get-register (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-calculator-summarize)))
(defadvice calculator-quit (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line )))
(defadvice calculator-save-and-quit (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line )))

(defadvice calculator-update-display (after emacspeak pre
                                            act comp)
  "Speak the updated  display. "
  (emacspeak-speak-line))

;;}}}
;;{{{  keys 
(declaim (special calculator-mode-map))
(when (boundp 'calculator-mode-map)
  (define-key calculator-mode-map "k" 'calculator-copy)
  (define-key calculator-mode-map "p" 'calculator-paste)
  (define-key calculator-mode-map "\d" 'calculator-backspace)
  )
;;}}}
(provide 'emacspeak-calculator)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

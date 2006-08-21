;;; emacspeak-sawfish.el --- speech-enable sawfish mode
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak extension to speech-enable
;;; sawfish mode
;;; Keywords: Emacspeak, sawfish interaction 
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
(require 'advice)
(require 'emacspeak-speak)
(require 'emacspeak)
(require 'voice-lock)
(require 'emacspeak-fix-interactive)
(require 'emacspeak-sounds)

;;}}}
;;{{{  Introduction:

;;; Sawfish is a Gnome and KDE compliant window manager.
;;; It is the window manager I use and is fully configurable
;;; via Lisp.
;;; sawfish.el is an emacs package for interacting with the
;;; sawfish window manager.
;;; This module speech-enables sawfish interaction from
;;; Emacs

;;; Code:

;;}}}
;;{{{  advise interactive functions 

;;{{{ Evaluating sawfish expressions 

(defadvice sawfish-eval-region (after emacspeak pre act
                                      comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))
(defadvice sawfish-eval-buffer (after emacspeak pre act
                                      comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))

(defadvice sawfish-eval-defun (after emacspeak pre act
				     comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))

(defadvice sawfish-eval-expression (after emacspeak pre act
					  comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))

(defadvice sawfish-eval-last-sexp (after emacspeak pre act
					 comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-message-again)))

(defadvice sawfish-eval-print-last-sexp (after emacspeak pre act
					       comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))

;;}}}
;;{{{  looking up documentation 

(defadvice sawfish-describe-show (after emacspeak pre act comp)
  "Speak displayed documentation. "
  (when (get-buffer sawfish-help-buffer )
    (save-excursion
      (set-buffer sawfish-help-buffer)
      (emacspeak-auditory-icon 'help)
      (emacspeak-speak-buffer))))

(defadvice sawfish-info-function (after emacspeak pre act
                                        comp)
  "Speak the info documentation. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'help)
    (emacspeak-speak-buffer)))

(defadvice sawfish-info-variable (after emacspeak pre act
                                        comp)
  "Speak the info documentation. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'help)
    (emacspeak-speak-buffer)))    

(defadvice sawfish-info (after emacspeak pre act
			       comp)
  "Speak the info documentation. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'help)
    (emacspeak-speak-buffer)))    

(defadvice sawfish-rep-info (after emacspeak pre act
				   comp)
  "Speak the info documentation. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'help)
    (emacspeak-speak-buffer)))    

;;}}}
;;{{{  interaction 

(defadvice sawfish-interaction (after emacspeak pre act
                                      comp)
  "Provide auditory feedback. "
  (when (interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'open-object)))

 

(defadvice sawfish-complete-symbol (around emacspeak pre act)
  "Say what you completed."
  (let ((prior (point ))
        (dtk-stop-immediately dtk-stop-immediately))
    (when dtk-stop-immediately (dtk-stop))
    ad-do-it
    (when (> (point) prior)
      (setq dtk-stop-immediately nil)
      (dtk-speak (buffer-substring prior (point ))))
    ad-return-value))

;;}}}
;;{{{ setup sawfish mode 

(defadvice sawfish-mode (after emacspeak pre act comp)
  "Setup sawfish mode"
  (emacspeak-setup-programming-mode))

;;}}}

;;}}}
(provide 'emacspeak-sawfish)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

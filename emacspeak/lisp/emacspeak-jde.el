;;; emacspeak-jde.el --- Speech enable JDE -- An integrated Java Development Environment
;;; $Id$
;;; $Author$ 
;;; Description: Auditory interface to JDE
;;; Keywords: Emacspeak, Speak, Spoken Output, Java
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

;;{{{  Required modules
(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction

;;;Speech enable Java IDE.
;;; The Java IDE --JDE-- can be found at 
;;;http://sunsite.auc.dk/jde/

;;}}}
;;{{{ voice lock 
(def-voice-font  emacspeak-jde-number-personality voice-lock-constant-personality
  'jde-java-font-lock-number-face
  "Personality used for numbers."
  :group 'emacspeak-jde)

(def-voice-font emacspeak-jde-operator-personality voice-animate
  'jde-java-font-lock-operator-face
  "Personality used for java operators."
  :group 'emacspeak-jde)

(def-voice-font emacspeak-jde-constant-personality voice-lock-constant-personality
  'jde-java-font-lock-constant-face
  "Personality used for constants."
  :group 'emacspeak-jde)

(def-voice-font emacspeak-jde-api-personality voice-animate
  'jde-java-font-lock-api-face
  "Personality used for user defined API names."
  :group 'emacspeak-jde)

(def-voice-font emacspeak-jde-package-personality voice-monotone
  'jde-java-font-lock-package-face
  "Personality used for package names.")

(def-voice-font emacspeak-jde-italic-personality 'italic
  'jde-java-font-lock-italic-face
  "Personality used for italics."
  :group 'emacspeak-jde)

(def-voice-font emacspeak-jde-underline-personality 'underline
  'jde-java-font-lock-underline-face
  "Underline personality."
  :group 'emacspeak-jde)

(def-voice-font emacspeak-jde-bold-personality 'bold
  'jde-java-font-lock-bold-face
  "Personality used for bold."
  :group 'emacspeak-jde)

                                        ;jde-java-font-lock-link-face
                                        ;jde-java-font-lock-doc-tag-face
                                        ;jde-java-font-lock-modifier-face
                                        ;jde-java-font-lock-pre-face
                                        ;jde-java-font-lock-code-face

;;}}}
;;{{{ Advice interactive commands:
(defadvice jde-open-class-source (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice jde-open-source-for-symbol (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice jde-open-base-class-source (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))
(defadvice jde-complete-popup-message (before emacspeak pre act comp)
  "Provide auditory feedback."
  (message "%s" (ad-get-arg 0))
  (emacspeak-auditory-icon 'help))

(defadvice jde-complete-at-point (around emacspeak pre act)
  "Say what you completed."
  (let ((emacspeak-speak-messages nil))
    (when dtk-stop-immediately (dtk-stop))
    ad-do-it
    (dtk-speak emacspeak-last-message)
    ad-return-value))

(defadvice jde-compile (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (dtk-speak "Compiling current java project")))

(defadvice bsh (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (setq emacspeak-comint-autospeak nil)
    (emacspeak-speak-mode-line)))

(defadvice jde-run (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (setq emacspeak-comint-autospeak nil)
    (emacspeak-speak-mode-line)))

(defadvice jde-db (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{ jdebug 

(defadvice jde-debug (after emacspeak pre act comp)
  "Speak the line where we eventually stop. "
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice jde-bug-step-over (after emacspeak pre act comp)
  "Speak the line we stepped to "
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice jde-bug-step-into (after emacspeak pre act comp)
  "Speak the line we stepped to "
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice jde-bug-step-out (after emacspeak pre act comp)
  "Speak the line we stepped to "
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice jde-bug-continue (after emacspeak pre act comp)
  "Speak the line we stop  to "
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice jde-bug-exit (after emacspeak pre act comp)
  "Produce auditory icon indicating successful exit "
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))

(defadvice jde-bug-clear-breakpoint (after emacspeak pre act comp)
  "Produce auditory icon."
  (emacspeak-auditory-icon 'off))

(defadvice jde-bug-toggle-breakpoint (after emacspeak pre
                                            act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (message "toggled breakpoint.")))

(defadvice jde-bug-set-breakpoint (after emacspeak pre act comp)
  "Speak the line we set the break point at "
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice jde-bug-clear-breakpoint (after emacspeak pre act comp)
  "Speak the line we nuked the breakpoint  "
  (when (interactive-p)
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-speak-line)))

(defadvice jde-bug-highlight-breakpoint (after emacspeak pre act comp)
  "Annotate line with an auditory icon. "
  (let ((start nil))
    (save-excursion
      (if (ad-get-arg 0)
          (goto-line (ad-get-arg 0)))
      (beginning-of-line)
      (setq  start (point))
      (end-of-line)
      (ems-modify-buffer-safely
       (put-text-property start (point)
                          'auditory-icon 'mark-object)))))

(defadvice jde-bug-remove-breakpoint-highlight (after emacspeak pre act comp)
  "Clear auditory annotation"
  (let ((start nil))
    (save-excursion
      (beginning-of-line)
      (setq  start (point))
      (end-of-line)
      (ems-modify-buffer-safely
       (remove-text-properties
        start (point)
        (list 'auditory-icon 'mark-object))))))

(defadvice jde-bug-up-stack (after emacspeak pre act comp)
  "Speak the line we stepped to "
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice jde-bug-down-stack (after emacspeak pre act comp)
  "Speak the line we stepped to "
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

;;}}}
;;{{{ speech enable jdb interaction 

(loop for command in
      '(
        jde-debug-step-into
        jde-debug-step-out
	jde-debug-step-over
	jde-debug-up
	jde-debug-down)
      do
      (eval
       (`
        (defadvice (, command) (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (interactive-p)
            (emacspeak-speak-line)
            (emacspeak-auditory-icon 'select-object))))))

(defadvice jde-db-run (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice jde-debug-cont (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (message "Continuing execution.")))
(defadvice jde-debug-quit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Quit debugger.")))
(defadvice jde-debug-set-breakpoint (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice jde-debug-toggle-breakpoint (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'button)))
(defadvice jde-debug-clear-breakpoints (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (message "Cleared all break points.")))

    
;;}}}
(provide 'emacspeak-jde )
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

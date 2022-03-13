;;; emacspeak-jdee.el --- Speech enable JDEE -- An integrated Java Development Environment  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $ 
;;; Description: Auditory interface to JDEE
;;; Keywords: Emacspeak, Speak, Spoken Output, Java
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com 
;;; A speech interface to Emacs |
;;; $Date: 2007-09-01 15:30:13 -0700 (Sat, 01 Sep 2007) $ |
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

;;{{{  Required modules
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction
;;; Commentary:
;;;Speech enable Java IDE.
;;; The Java IDE --JDEE-- can be found at 
;;;http://sunsite.auc.dk/jdee/
;;; Code:
;;}}}
;;{{{ voice lock 

(voice-setup-add-map
 '(
   (jdee-java-font-lock-number-face voice-lighten)
   (jdee-java-font-lock-operator-face voice-animate)
   (jdee-java-font-lock-constant-face voice-lighten)
   (jdee-java-font-lock-api-face voice-animate)
   (jdee-java-font-lock-package-face voice-monotone-extra)
   (jdee-java-font-lock-italic-face voice-animate)
   (jdee-java-font-lock-underline-face voice-brighten-medium)
   (jdee-java-font-lock-bold-face voice-bolden)
   (jdee-java-font-lock-pre-face voice-monotone-extra)
   (jdee-java-font-lock-code-face voice-monotone-extra)
   ))

                                        ;jdee-java-font-lock-link-face
                                        ;jdee-java-font-lock-doc-tag-face
                                        ;jdee-java-font-lock-modifier-face

;;}}}
;;{{{ Advice interactive commands:
(defadvice jdee-open-class-source (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice jdee-open-source-for-symbol (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice jdee-open-base-class-source (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))
(defadvice jdee-complete-popup-message (before emacspeak pre act comp)
  "speak."
  (message "%s" (ad-get-arg 0))
  (emacspeak-auditory-icon 'help))

(defadvice jdee-complete-at-point (around emacspeak pre act comp)
  "Say what you completed."
  (let ((emacspeak-speak-messages nil))
    (when dtk-stop-immediately (dtk-stop))
    ad-do-it
    (dtk-speak emacspeak-last-message)
    ad-return-value))

(defadvice jdee-compile (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (dtk-speak "Compiling current java project")))

(defadvice bsh (after emacspeak pre act comp)
  "speak"
  (cl-declare (special emacspeak-comint-autospeak))
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (setq emacspeak-comint-autospeak nil)
    (emacspeak-speak-mode-line)))

(defadvice jdee-run (after emacspeak pre act comp)
  "speak"
  (cl-declare (special emacspeak-comint-autospeak))
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (setq emacspeak-comint-autospeak nil)
    (emacspeak-speak-mode-line)))

(defadvice jdee-db (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{ jdeebug 

(defadvice jdee-debug (after emacspeak pre act comp)
  "Speak the line where we eventually stop. "
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice jdee-bug-step-over (after emacspeak pre act comp)
  "Speak the line we stepped to "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice jdee-bug-step-into (after emacspeak pre act comp)
  "Speak the line we stepped to "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice jdee-bug-step-out (after emacspeak pre act comp)
  "Speak the line we stepped to "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice jdee-bug-continue (after emacspeak pre act comp)
  "Speak the line we stop  to "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice jdee-bug-exit (after emacspeak pre act comp)
  "Produce auditory icon indicating successful exit "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))

(defadvice jdee-bug-clear-breakpoint (after emacspeak pre act comp)
  "Produce auditory icon."
  (emacspeak-auditory-icon 'off))

(defadvice jdee-bug-toggle-breakpoint (after emacspeak pre
                                             act comp)
  "speak."
  (when (ems-interactive-p)
    (message "toggled breakpoint.")))

(defadvice jdee-bug-set-breakpoint (after emacspeak pre act comp)
  "Speak the line we set the break point at "
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice jdee-bug-clear-breakpoint (after emacspeak pre act comp)
  "Speak the line we nuked the breakpoint  "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-speak-line)))

(defadvice jdee-bug-highlight-breakpoint (after emacspeak pre act comp)
  "Annotate line with an auditory icon. "
  (let ((start nil))
    (save-excursion
      (if (ad-get-arg 0)
          (goto-line (ad-get-arg 0)))
      (beginning-of-line)
      (setq  start (point))
      (end-of-line)
      (with-silent-modifications
        (put-text-property start (point)
                           'auditory-icon 'mark-object)))))

(defadvice jdee-bug-remove-breakpoint-highlight (after emacspeak pre act comp)
  "Clear auditory annotation"
  (let ((start nil))
    (save-excursion
      (beginning-of-line)
      (setq  start (point))
      (end-of-line)
      (with-silent-modifications
        (remove-text-properties
         start (point)
         (list 'auditory-icon 'mark-object))))))

(defadvice jdee-bug-up-stack (after emacspeak pre act comp)
  "Speak the line we stepped to "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice jdee-bug-down-stack (after emacspeak pre act comp)
  "Speak the line we stepped to "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

;;}}}
;;{{{ speech enable jdb interaction 

(cl-loop for command in
         '(
           jdee-debug-step-into
           jdee-debug-step-out
           jdee-debug-step-over
           jdee-debug-up
           jdee-debug-down)
         do
         (eval
          `(defadvice ,command (after emacspeak pre act comp)
             "speak."
             (when (ems-interactive-p)
               (emacspeak-speak-line)
               (emacspeak-auditory-icon 'select-object)))))

(defadvice jdee-db-run (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice jdee-debug-cont (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (message "Continuing execution.")))
(defadvice jdee-debug-quit (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Quit debugger.")))
(defadvice jdee-debug-set-breakpoint (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice jdee-debug-toggle-breakpoint (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'button)))
(defadvice jdee-debug-clear-breakpoints (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (message "Cleared all break points.")))

;;}}}
;;{{{ advice jdee-xref
(defadvice jdee-xref-first-caller(after emacspeak pre act comp)
  "Speak line we jumped to."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice jdee-xref-next-caller(around emacspeak pre act comp)
  "Speak line we jumped to.
If we are on the last call, do nothing."
  (cl-declare (special jdee-xref-stack))
  (cond
   ((and (ems-interactive-p)
         (car jdee-xref-stack))
    ad-do-it
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line))
   (t ad-do-it))
  ad-return-value)

;;}}}
;;{{{ Advice EFC widgets:

(defadvice efc-option-dialog (after emacspeak pre act comp)
  "Announce dialog box we just opened."
  (emacspeak-auditory-icon 'open-object)
  (dtk-speak
   (ad-get-arg 0)))

;;}}}
;;{{{ camel case deletion

(defadvice jdee-kill-camel-tok (before emacspeak pre act comp)
  "Speak word before killing it."
  (when (ems-interactive-p)
    (dtk-speak
     (buffer-substring
      (point)
      (save-excursion (jdee-end-of-camel-tok))))))

;;}}}

(provide 'emacspeak-jdee)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}

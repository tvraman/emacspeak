;;; emacspeak-python.el --- Speech enable Python development environment
;;; $Id$
;;; $Author$ 
;;; Description: Auditory interface to python mode
;;; Keywords: Emacspeak, Speak, Spoken Output, python
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

;;; Copyright (c) 1995 -- 2004, T. V. Raman
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

;;;Speech enable python mode.
;;; If you have python installed
;;; you will find python-mode.el in the Python distribution.
;;; If you dont have python, then you dont need this either.

;;}}}
;;{{{ Advice interactive commands:
;;{{{  electric editing

(defadvice py-electric-colon (after emacspeak pre act comp)
  "Speak what you inserted"
  (when (interactive-p)
    (dtk-say " colon ")))

(defadvice py-electric-backspace (around emacspeak pre act)
  "Speak character you're deleting."
  (cond
   ((interactive-p )
    (dtk-tone 500 30 'force)
    (emacspeak-speak-this-char (preceding-char ))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice py-electric-delete (around emacspeak pre act)
  "Speak character you're deleting."
  (cond
   ((interactive-p )
    (dtk-tone 500 30 'force)
    (emacspeak-speak-this-char (preceding-char ))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

;;}}}
;;{{{ interactive programming

(defadvice py-shell (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (message "Launched Python interpreter")))

(defadvice py-clear-queue (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice py-execute-region (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice py-execute-buffer (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice py-goto-exception(after emacspeak pre act comp)
  "Speak line you moved to"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice py-down-exception(after emacspeak pre act comp)
  "Speak line you moved to"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice py-up-exception(after emacspeak pre act comp)
  "Speak line you moved to"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

;;}}}
;;{{{  whitespace management and indentation

(defadvice py-newline-and-indent(after emacspeak pre act comp)
  "Speak line so we know current indentation"
  (when (interactive-p)
    (dtk-speak-using-voice voice-annotate
                           (format
                            "indent %s"
                            (current-column)))
    (dtk-force)))

(defadvice py-shift-region-left (after emacspeak pre act comp)
  "Speak number of lines that were shifted"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (dtk-speak
     (format "Left shifted block  containing %s lines"
             (count-lines  (region-beginning)
                           (region-end))))))

(defadvice py-shift-region-right (after emacspeak pre act comp)
  "Speak number of lines that were shifted"
  (when (interactive-p)
    (dtk-speak
     (format "Right shifted block  containing %s lines"
             (count-lines  (region-beginning)
                           (region-end))))))
(defadvice py-indent-region (after emacspeak pre act comp)
  "Speak number of lines that were shifted"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (dtk-speak
     (format "Indented region   containing %s lines"
             (count-lines  (region-beginning)
                           (region-end))))))

(defadvice py-comment-region (after emacspeak pre act comp)
  "Speak number of lines that were shifted"
  (when (interactive-p)
    (dtk-speak
     (format "Commented  block  containing %s lines"
             (count-lines  (region-beginning)
                           (region-end))))))

;;}}}
;;{{{  buffer navigation
(defadvice py-previous-statement (after emacspeak pre act comp)
  "Speak current statement after moving"
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))
(defadvice py-next-statement (after emacspeak pre act comp)
  "Speak current statement after moving"
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice py-goto-block-up (after emacspeak pre act comp)
  "Speak current statement after moving"
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))
(defadvice beginning-of-python-def-or-class (after emacspeak pre act comp)
  "Speak current statement after moving"
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice end-of-python-def-or-class (after emacspeak pre act comp)
  "Speak current statement after moving"
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice py-mark-block (after emacspeak pre act comp)
  "Speak number of lines marked"
  (when (interactive-p)
    (dtk-speak
     (format "Marked block containing %s lines"
             (count-lines (region-beginning)
                          (region-end))))
    (emacspeak-auditory-icon 'mark-object)))

(defadvice py-mark-def-or-class (after emacspeak pre act comp)
  "Speak number of lines marked"
  (when (interactive-p)
    (dtk-speak
     (format "Marked block containing %s lines"
             (count-lines (region-beginning)
                          (region-end))))
    (emacspeak-auditory-icon 'mark-object)))

(defadvice py-forward-into-nomenclature(after emacspeak pre act comp)
  "Speak rest of current word"
  (when (interactive-p)
    (emacspeak-speak-word 1)))

(defadvice py-backward-into-nomenclature(after emacspeak pre act comp)
  "Speak rest of current word"
  (when (interactive-p)
    (emacspeak-speak-word 1)))

;;}}}
;;{{{ the process buffer

(defadvice py-process-filter (around emacspeak pre act)
  "Make comint in Python speak its output. "
  (declare (special emacspeak-comint-autospeak))
  (let ((prior (point ))
        (dtk-stop-immediately nil))
    ad-do-it 
    (when (and  emacspeak-comint-autospeak
                (window-live-p
                 (get-buffer-window (process-buffer (ad-get-arg 0)))))
      (condition-case nil
          (emacspeak-speak-region prior (point ))
        (error (emacspeak-auditory-icon 'scroll)
               (dtk-stop ))))
    ad-return-value))

;;}}}
;;}}}
;;{{{ Additional navigation
(defun emacspeak-py-previous-block()
  "Move backward to the beginning of the current block.
If already at the beginning then move to previous block."
  (interactive)
  (let ((start (point)))
    (beginning-of-python-def-or-class)
    (unless (eq start (point))
      (beginning-of-line)
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'large-movement))))

(defun emacspeak-py-next-block()
  "Move forward to the beginning of the next block."
  (interactive)
  (end-of-python-def-or-class)
  (skip-syntax-forward " ")
  (forward-line 1)
  (beginning-of-line)
  (emacspeak-speak-line)
  (emacspeak-auditory-icon 'large-movement))

;;}}}
;;{{{  hooks

(defun emacspeak-python-mode-hook ()
  "Hook added by Emacspeak to python mode"
  (unless emacspeak-audio-indentation
    (emacspeak-toggle-audio-indentation)))

(add-hook 'py-mode-hook
          'emacspeak-python-mode-hook)

;;}}}
;;{{{ keybindings

(progn
  (declaim (special  py-mode-map))
  (define-key py-mode-map "\M-a" 'beginning-of-python-def-or-class)
  (define-key py-mode-map "\M-e" 'end-of-python-def-or-class)
  (define-key py-mode-map "\M-n" 'py-next-statement)
  (define-key py-mode-map "\M-p" 'py-previous-statement)
  (define-key py-mode-map "\C-\M-u" 'py-goto-block-up)
  (define-key py-mode-map "\C-\M-n" 'emacspeak-py-next-block)
  (define-key py-mode-map "\C-\M-p" 'emacspeak-py-previous-block)
  )

;;}}}
(provide 'emacspeak-python )
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

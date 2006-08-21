;;; emacspeak-view-process.el --- Speech enable View Processes -- A powerful task manager
;;; $Id$
;;; $Author$ 
;;; Description: Emacspeak extension for flexible viewing of processes
;;; Keywords:emacspeak, audio interface to emacs administering processes
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
;;;Copyright (C) 1995 -- 2003, T. V. Raman 
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

;;{{{  Introduction

;;; Powerful speech interface to viewing and administering processes

;;}}}
;;{{{ requires
(require 'emacspeak-preamble)

;;}}}
;;{{{  keybindings

(add-hook 'View-process-mode-hook
          (function
           (lambda ()
             (declare (special View-process-mode-map))
             (define-key View-process-mode-map ";" 'emacspeak-view-process-speak-current-field)
             (define-key View-process-mode-map "\C-m"
               'emacspeak-view-process-goto-current-field-next-line)
             )))

;;}}}
;;{{{ helper

(defsubst emacspeak-view-process-get-current-field ()
  (let ((start nil ))
    (save-excursion
      (skip-syntax-backward "^ ")
      (setq start (point ))
      (skip-syntax-forward "^ ")
      (buffer-substring start (point)))))

;;}}}
;;{{{ additional commands 

(defun emacspeak-view-process-goto-current-field-next-line ()
  "Set point to the current field in the next line."
  (interactive)
  (let ((col (current-column)))
    (View-process-goto-first-field-next-line)
    (forward-char col)
    (emacspeak-auditory-icon 'select-object)
    (View-process-show-pid-and-command)))

(defsubst emacspeak-view-process-speak-current-field()
  "Speak current field"
  (interactive)
  (let ((field-name (View-process-translate-field-position-to-name
                     (View-process-current-field-number))))
    (put-text-property 0 (length field-name)
                       'personality voice-annotate field-name)
    (dtk-speak
     (concat
      field-name
      ":"
      (emacspeak-view-process-get-current-field)))))

;;}}}
;;{{{ Advice interactive commands:

(defadvice View-process-mode (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (voice-lock-mode t)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defadvice View-process-goto-first-field-next-line (after emacspeak pre act
                                                          comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (View-process-show-pid-and-command)))

(defadvice  View-process-next-field (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-view-process-speak-current-field)))

(defadvice  View-process-previous-field (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-view-process-speak-current-field)))

(defadvice View-process-sort-by-current-field-g (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (message "Sorted processes by current field")))

(defadvice View-process-sort-output-by-current-field (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (message "Sorted processes by current field")))

(defadvice View-process-reverse-output (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (message "Reversed output lines")))

(defadvice View-process-quit (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))
(defadvice View-process-output-end (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (View-process-show-pid-and-command)))

(defadvice View-process-output-start (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (View-process-show-pid-and-command)))

(defadvice View-process-start-itimer (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when  (interactive-p)
    (emacspeak-auditory-icon 'on)
    (message "Started itimer")))

(defadvice View-process-delete-itimer (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when  (interactive-p)
    (emacspeak-auditory-icon 'off)
    (message "Deleted itimer")))
(defadvice View-process-mark-childs-in-current-line (around emacspeak pre act comp)
  "Display number of processes marked"
  (cond
   ((interactive-p)
    (let ((count (length View-process-pid-mark-alist)))
      ad-do-it
      (emacspeak-auditory-icon 'mark-object)
      (message "Marked %s child processes"
               (- (length View-process-pid-mark-alist)
                  (1+ count)))))
   (t ad-do-it))
  ad-return-value)

(defadvice View-process-unmark-all (around emacspeak pre act comp)
  "Display number of processes were unmarked"
  (cond
   ((interactive-p)
    (let ((count (length View-process-pid-mark-alist)))
      ad-do-it
      (emacspeak-auditory-icon 'deselect-object)
      (message "Unmarked %s  processes"
               count)))
   (t ad-do-it))
  ad-return-value)

(defadvice View-process-unmark-current-line(after emacspeak
                                                  pre act comp)
  "Provide auditory icon"
  (when (interactive-p)
    (emacspeak-auditory-icon 'deselect-object)
    (View-process-show-pid-and-command)))
(defadvice View-process-mark-current-line(after emacspeak
                                                pre act comp)
  "Provide auditory icon"
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (View-process-show-pid-and-command)))

;;}}}
;;{{{ voice locking

(defgroup emacspeak-view-process nil
  "Task manager for the Emacspeak Desktop."
  :group 'emacspeak
  :prefix "emacspeak-view-process-")
  
(defcustom View-process-child-line-personality 'kid
  "personality for child process "
  :type 'symbol
  :group 'emacspeak-view-process)

(defcustom View-process-parent-line-personality voice-bolden
  "Personality for parent "
  :type 'symbol
  :group 'emacspeak-view-process)

(defcustom View-process-single-line-personality voice-monotone
  "Personality for voice lock in view process mode"
  :type 'symbol
  :group 'emacspeak-view-process)

(defcustom View-process-signal-line-personality voice-smoothen
  "Indicate a signal"
  :type 'symbol
  :group 'emacspeak-view-process)

(defcustom View-process-signaled-line-personality voice-animate
  "Personality for indicating a signalled process"
  :type 'symbol
  :group 'emacspeak-view-process)

	 
(defcustom View-process-renice-line-personality voice-monotone-medium
  "Indicate a reniced process"
  :type 'symbol
  :group 'emacspeak-view-process)

(declaim (special View-process-child-line-mark
                  View-process-parent-line-mark
                  View-process-single-line-mark
                  View-process-signaled-line-mark
                  View-process-signal-line-mark
                  View-process-renice-line-mark))

;;}}}
(provide  'emacspeak-view-process)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

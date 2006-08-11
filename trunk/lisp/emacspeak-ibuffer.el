;;; emacspeak-ibuffer.el --- speech-enable ibuffer buffer selection
;;; $Id$
;;; $Author$
;;; Description:   extension to speech enable ibuffer
;;; Keywords: Emacspeak, Audio Desktop
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision: 24.0 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1995 -- 2004, T. V. Raman<raman@cs.cornell.edu>
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

(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; speech-enable ibuffer.el
;;; this is an alternative to buffer-menu
;;; Code:

;;}}}
;;{{{ helpers 

(defun emacspeak-ibuffer-speak-buffer-line ()
  "Speak information about this buffer"
  (interactive)
  (declare (special dtk-stop-immediately
                    list-buffers-directory))
  (unless (eq major-mode 'ibuffer-mode)
    (error "This command can only be used in buffer menus"))
  (emacspeak-speak-line))

;;}}}
;;{{{ summarizers

(defun emacspeak-ibuffer-summarize-line ()
  "Summarize current line."
  (emacspeak-speak-line))

;;}}}
;;{{{ speech enable interactive commands 

(defadvice ibuffer-quit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))
(defadvice ibuffer-backward-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-ibuffer-summarize-line)
    (emacspeak-auditory-icon 'select-object)))
(defadvice ibuffer-forward-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-ibuffer-summarize-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice ibuffer-backward-filter-group (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-ibuffer-summarize-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice ibuffer-forward-filter-group (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-ibuffer-summarize-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice ibuffer-backwards-next-marked (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-ibuffer-summarize-line)
    (emacspeak-auditory-icon 'select-object)))
(defadvice ibuffer-forward-next-marked (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-ibuffer-summarize-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice ibuffer-visit-buffer (after emacspeak pre act comp)
  "Provide spoken status information."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defadvice ibuffer-visit-buffer-other-window (after emacspeak pre act
                                                    comp)
  "Provide spoken status information."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defadvice ibuffer-visit-buffer-other-frame (after emacspeak pre act
                                                   comp)
  "Provide spoken status information."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defadvice ibuffer-diff-with-file (after emacspeak pre act
                                         comp)
  "Provide spoken feedback."
  (when (interactive-p)
    (message "Displayed differences in other window.")
    (emacspeak-auditory-icon 'task-done)))
(defadvice ibuffer-limit-disable (after emacspeak pre act
                                        comp)
  "Speak status information."
  (when (interactive-p)
    (message "Disabled limiting.")))

(defadvice ibuffer-do-view (after emacspeak pre act comp)
  "Speak status information."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (emacspeak-speak-mode-line)))

(defadvice ibuffer-do-view-other-frame (after emacspeak pre act comp)
  "Speak status information."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (emacspeak-speak-mode-line)))

(defadvice ibuffer-do-save (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (message "Saving marked buffers.")
    (emacspeak-auditory-icon 'save-object)))
(defadvice  ibuffer-occur-goto-occurence (after emacspeak
                                                pre act
                                                comp)
  "Speak line that becomes current."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))
(defadvice  ibuffer-occur-display-occurence (after emacspeak
                                                   pre act
                                                   comp)
  "Speak line that becomes current."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'task-done)))

(defadvice ibuffer-mark-forward (after emacspeak pre act
                                       comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-ibuffer-speak-buffer-line)))

(defadvice ibuffer-unmark-forward (after emacspeak pre act
                                         comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-ibuffer-speak-buffer-line)))

(defadvice ibuffer-unmark-backward (after emacspeak pre act
                                          comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-ibuffer-speak-buffer-line)))

;;}}}
(provide 'emacspeak-ibuffer)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

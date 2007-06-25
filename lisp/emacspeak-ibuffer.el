;;; emacspeak-ibuffer.el --- speech-enable ibuffer buffer selection
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:   extension to speech enable ibuffer
;;; Keywords: Emacspeak, Audio Desktop
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1995 -- 2007, T. V. Raman<raman@cs.cornell.edu>
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

(defadvice ibuffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice ibuffer-other-window (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice ibuffer-list-buffers (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice ibuffer-update (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'modified-object)))

(defadvice ibuffer-customize (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice ibuffer-bury-buffer (around emacspeak pre act comp)
  "Provide auditory feedback."
  (let ((buf (ibuffer-current-buffer t)))
    (when (interactive-p)
      ad-do-it
      (emacspeak-auditory-icon 'select-object)
      (message "Buried buffer %s" buf))))

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

(defadvice ibuffer-visit-buffer-1-window (after emacspeak pre act comp)
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

(defadvice ibuffer-visit-buffer-other-window-noselect (after emacspeak 
                                                             pre act comp)
  "Provide spoken status information."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (dtk-speak "Opened buffer in other window.")))

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

(defadvice ibuffer-do-view-horizontally (after emacspeak pre act comp)
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

(defadvice ibuffer-unmark-all (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-ibuffer-speak-buffer-line)))

(defadvice ibuffer-toggle-marks (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))

(defadvice ibuffer-mark-for-delete (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-ibuffer-speak-buffer-line)))

(defadvice ibuffer-mark-for-delete-backwards (after emacspeak 
                                                    pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-ibuffer-speak-buffer-line)))

(defadvice ibuffer-interactive-filter-by-mode (after emacspeak 
                                                     pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'modified-object)
    (dtk-speak 
     (concat "Filtered by "
             (format "%s" 
                     ;; the following returns a string,, how to remove
                     ;; the parens?
                     ibuffer-filtering-qualifiers)))))

(defadvice ibuffer-recompile-formats (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (dtk-speak "Recompiled formats")))

(defadvice ibuffer-switch-format  (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (dtk-speak "Switched formats")))

(defadvice ibuffer-toggle-filter-group (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (let ((name (get-text-property (point) 'ibuffer-filter-group-name)))
      (emacspeak-auditory-icon 'modified-object)
      (dtk-speak 
       (concat "Toggled group " 
               (format "%s" name))))))

(defadvice ibuffer-do-shell-command-pipe-replace (after emacspeak 
                                                        pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice ibuffer-do-shell-command-pipe (after emacspeak 
                                                pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice ibuffer-do-shell-command-file (after emacspeak 
                                                pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice ibuffer-do-rename-uniquely (after emacspeak 
                                             pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice ibuffer-do-replace-regexp (after emacspeak 
                                            pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice ibuffer-filters-to-filter-group (after emacspeak 
                                                  pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (dtk-speak "Group added.")))

(defadvice ibuffer-set-filter-groups-by-mode (after emacspeak 
                                                    pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (dtk-speak "Filtered by major mode.")))

(defadvice ibuffer-pop-filter-group (around emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (let ((name (car (car ibuffer-filter-groups))))
      ad-do-it
      (emacspeak-auditory-icon 'task-done)
      (dtk-speak 
       (concat "Popped group " 
               (format "%s" name))))))

(defadvice ibuffer-clear-filter-groups (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (dtk-speak "Cleared all filter groups.")))

(defadvice ibuffer-jump-to-filter-group (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-ibuffer-speak-buffer-line)))

(defadvice ibuffer-kill-filter-group (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    ;; name is a variable in scope in the advised function
    (dtk-speak (format "Killed %s group." name))))

(defadvice ibuffer-yank-filter-group (around emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (let ((name (car (car ibuffer-filter-group-kill-ring))))
      (emacspeak-auditory-icon 'yank-object)
      ad-do-it
      (dtk-speak (format "Yanked %s group." name)))))

(defadvice ibuffer-filter-disable (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (dtk-speak "Disabled all filters.")))

(defadvice ibuffer-filter-by-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice ibuffer-filter-by-used-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice ibuffer-filter-by-name (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice ibuffer-filter-by-filename (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice ibuffer-filter-by-size-gt (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice ibuffer-filter-by-size-lt (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice ibuffer-filter-by-content (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice ibuffer-filter-by-predicate (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice ibuffer-filter-by-predicate (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice ibuffer-toggle-sorting-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice ibuffer-toggle-sorting-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice ibuffer-invert-sorting (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice ibuffer-do-sort-by-major-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (dtk-speak "Sorted by major mode.")))

(defadvice ibuffer-do-sort-by-alphabetic (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (dtk-speak "Sorted alphabetically.")))

(defadvice ibuffer-do-sort-by-size (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (dtk-speak "Sorted by size.")))

(defadvice ibuffer-bs-show (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice ibuffer-bs-toggle-all (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (dtk-speak "Toggled show all.")))

(defadvice ibuffer-add-to-tmp-hide (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (dtk-speak "Buffer hidden.")))

(defadvice ibuffer-add-to-tmp-show (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (dtk-speak "Buffer added.")))

(defadvice ibuffer-do-kill-lines (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice ibuffer-jump-to-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-ibuffer-speak-buffer-line)))

(defadvice ibuffer-copy-filename-as-kill (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (dtk-speak "Buffer added.")))

(defadvice ibuffer-copy-filename-as-kill (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (dtk-speak (format "copied %s filenames." (ibuffer-count-marked-lines)))))

(defadvice ibuffer-mark-by-name-regexp (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice ibuffer-mark-by-mode-regexp (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice ibuffer-mark-by-file-name-regexp (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice ibuffer-mark-by-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice ibuffer-mark-modified-buffers (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice ibuffer-mark-unsaved-buffers (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice ibuffer-mark-dissociated-buffers (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice ibuffer-mark-help-buffers (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice ibuffer-mark-compressed-file-buffers (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice ibuffer-mark-old-buffers (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice ibuffer-mark-special-buffers (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice ibuffer-mark-read-only-buffers (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice ibuffer-mark-dired-buffers (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)))

;;;;;;;;;;;;;;;;;;;;;;
;; now, go back to ibuffer-yank and add the missing functions:

;; (defun ibuffer-yank ()
;; (defun ibuffer-yank-filter-group (name)
;; (defun ibuffer-or-filter (&optional reverse)
;; (defun ibuffer-save-filter-groups (name groups)
;; (defun ibuffer-delete-saved-filter-groups (name)
;; (defun ibuffer-switch-to-saved-filter-groups (name)
;; (defun ibuffer-pop-filter ()
;; (defun ibuffer-exchange-filters ()
;; (defun ibuffer-negate-filter ()
;; (defun ibuffer-save-filters (name filters)
;; (defun ibuffer-delete-saved-filters (name)
;; (defun ibuffer-add-saved-filters (name)
;; (defun ibuffer-switch-to-saved-filters (name)
;; (defun ibuffer-do-occur (regexp &optional nlines)

;;}}}
(provide 'emacspeak-ibuffer)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

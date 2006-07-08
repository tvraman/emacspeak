;;; emacspeak-buff-menu.el --- Speech enable Buffer Menu Mode -- used to manage buffers
;;; $Id$
;;; $Author$ 
;;; Description: Auditory interface to buff-menu
;;; Keywords: Emacspeak, Speak, Spoken Output, buff-menu
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

;;{{{  Introduction 

;;;Advice buffer menu commands

;;}}}
;;{{{  Required modules

;;; Code:

(require 'emacspeak-preamble)

;;}}}
;;{{{ voice personalities
(voice-setup-add-map
 '(
   (Buffer-menu-buffer-face voice-bolden)
   ))

;;}}}
;;{{{  list buffers 

(defun emacspeak-list-buffers-speak-buffer-name ()
  "Speak the name of the buffer on this line"
  (interactive)
  (cond
   ((eq major-mode 'Buffer-menu-mode)
    (let*((buffer (Buffer-menu-buffer t)))
      (if (get-buffer buffer)
          (dtk-speak (buffer-name  buffer))
        (error "No valid buffer on this line"))))
   (t (error "This command can be used only in buffer menus"))))

(defun emacspeak-list-buffers-speak-buffer-line ()
  "Speak information about this buffer"
  (interactive)
  (declare (special list-buffers-directory
                    dtk-stop-immediately))
  (cond
   ((eq major-mode 'Buffer-menu-mode)
    (let((buffer (Buffer-menu-buffer t)))
      (cond
       ((get-buffer buffer)
        (when dtk-stop-immediately (dtk-stop))
        (let ((name (buffer-name buffer))
              (file (buffer-file-name buffer))
              this-buffer-read-only
              this-buffer-modified-p
              this-buffer-size
              this-buffer-mode-name
              this-buffer-directory
              (dtk-stop-immediately nil))
          (save-excursion
            (set-buffer buffer)
            (setq this-buffer-read-only buffer-read-only)
            (setq this-buffer-modified-p (buffer-modified-p))
            (setq this-buffer-size (buffer-size))
            (setq this-buffer-mode-name mode-name)
            (or file
                ;; No visited file.  Check local value of
                ;; list-buffers-directory.
                (if (and (boundp 'list-buffers-directory)
                         list-buffers-directory)
                    (setq this-buffer-directory list-buffers-directory))))
                                        ;format and speak the line
          (when this-buffer-modified-p (dtk-tone 700 70))
          (when this-buffer-read-only (dtk-tone 250 50))
          (dtk-speak
           (format  "%s a %s  document  %s with size  %s"
                    name
                    this-buffer-mode-name
                    (if (or file this-buffer-directory)
                        (format "visiting %s"
                                (or file this-buffer-directory))
                      "")
                    this-buffer-size))))
       (t(emacspeak-auditory-icon 'error)
         (emacspeak-speak-line)))))
   (t (error "This command can be used only in buffer menus"))))

(defun emacspeak-list-buffers-next-line (count)
  "Speech enabled buffer menu navigation"
  (interactive "p")
  (next-line count)
  (emacspeak-list-buffers-speak-buffer-line))

(defun emacspeak-list-buffers-previous-line (count)
  "Speech enabled buffer menu navigation"
  (interactive "p")
  (previous-line count)
  (emacspeak-list-buffers-speak-buffer-line))

(defadvice list-buffers (after emacspeak pre act )
  "Provide auditory feedback"
  (declare (special Buffer-menu-mode-map))
  (when (interactive-p)
    (other-window 1)
    (goto-char (point-min))
    (forward-line 2)
    (define-key Buffer-menu-mode-map "," 'emacspeak-list-buffers-speak-buffer-name)
    (define-key Buffer-menu-mode-map "."
      'emacspeak-list-buffers-speak-buffer-line)
    (define-key Buffer-menu-mode-map "n" 'emacspeak-list-buffers-next-line)
    (define-key Buffer-menu-mode-map "p" 'emacspeak-list-buffers-previous-line)
    (emacspeak-list-buffers-speak-buffer-line)
    (emacspeak-auditory-icon 'task-done)))

(defadvice buffer-menu (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (message "Displayed list of buffers in other window")))

;;{{{  buffer manipulation commands 
(defadvice Buffer-menu-bury (after emacspeak pre act)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-list-buffers-speak-buffer-line )))

(defadvice Buffer-menu-delete-backwards (after emacspeak pre act)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-list-buffers-speak-buffer-line )))

(defadvice Buffer-menu-delete (after emacspeak pre act)
  "Provide spoken and auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-list-buffers-speak-buffer-line )))

(defadvice Buffer-menu-mark (after emacspeak pre act)
  "Provide spoken and auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-list-buffers-speak-buffer-line )))

(defadvice Buffer-menu-quit (after emacspeak pre act)
  "Speak the modeline of the newly visible buffer."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice Buffer-menu-save (after emacspeak pre act)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'save-object)
    (emacspeak-list-buffers-speak-buffer-line )))

(defadvice Buffer-menu-select (after emacspeak pre act)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line )))

(defadvice Buffer-menu-unmark (after emacspeak pre act)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-list-buffers-speak-buffer-line )))

(defadvice Buffer-menu-backup-unmark (after emacspeak pre act)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-list-buffers-speak-buffer-line )))

(defadvice Buffer-menu-execute (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice Buffer-menu-toggle-read-only (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-list-buffers-speak-buffer-line )))

(defadvice Buffer-menu-not-modified (after emacspeak pre act )
  "Provide auditory feedback "
  (when (interactive-p)
    (emacspeak-list-buffers-speak-buffer-line)
    (if (ad-get-arg 0)
        (emacspeak-auditory-icon 'modified-object )
      (emacspeak-auditory-icon 'unmodified-object))))

(defadvice Buffer-menu-visit-tags-table (before emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (message "Visiting tags table on current line")))

;;}}}
;;{{{  display buffers 

(defadvice Buffer-menu-1-window (after emacspeak pre act)
  "Announce the newly selected buffer."
  (when (interactive-p )
    (emacspeak-speak-mode-line )
    (emacspeak-auditory-icon 'select-object)))

(defadvice Buffer-menu-2-window (after emacspeak pre act)
  "Announce the newly selected buffer."
  (when (interactive-p )
    (emacspeak-speak-mode-line )
    (emacspeak-auditory-icon 'select-object)))

(defadvice Buffer-menu-this-window (after emacspeak pre act)
  "Announce the newly selected buffer."
  (when (interactive-p )
    (emacspeak-speak-mode-line )
    (emacspeak-auditory-icon 'select-object)))
(defadvice Buffer-menu-other-window (after emacspeak pre act)
  "Provide auditory feedback"
  (when (interactive-p )
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

;;}}}

;;}}}
(provide 'emacspeak-buff-menu)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

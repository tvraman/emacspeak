;;; emacspeak-bs.el --- speech-enable bs buffer selection
;;; $Id$
;;; $Author$
;;; Description:   extension to speech enable bs
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
(require 'emacspeak-fix-interactive)

;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; speech-enable bs.el
;;; this is an alternative to list-buffers
;;; Code:

;;}}}
;;{{{ helpers 

(defun emacspeak-bs-speak-buffer-line ()
  "Speak information about this buffer"
  (interactive)
  (declare (special dtk-stop-immediately
                    list-buffers-directory))
  (unless (eq major-mode 'bs-mode)
    (error "This command can only be used in buffer menus"))
  (let((buffer (bs--current-buffer)))
    (cond
     ((get-buffer buffer)
      (when dtk-stop-immediately (dtk-stop))
      (let ((document " document ")
            (with "with size ")
            (name (buffer-name buffer))
            (file (buffer-file-name buffer))
            this-buffer-read-only
            this-buffer-modified-p
            this-buffer-size
            this-buffer-mode-name
	    mode-name
            this-buffer-directory
            (dtk-stop-immediately nil))
        (put-text-property 0 (length document)
                           'personality 'paul-smooth
                           document)
        (put-text-property 0 (length with)
                           'personality 'paul-smooth with)
        (save-excursion
          (set-buffer buffer)
          (setq this-buffer-read-only buffer-read-only)
          (setq this-buffer-modified-p (buffer-modified-p))
          (setq this-buffer-size (buffer-size))
          (setq this-buffer-mode-name (copy-sequence mode-name))
          (or file
              ;; No visited file.  Check local value of
              ;; list-buffers-directory.
              (if (and (boundp 'list-buffers-directory)
                       list-buffers-directory)
                  (setq this-buffer-directory list-buffers-directory))))
                                        ;format and speak the line
        (when this-buffer-modified-p (dtk-tone 700 70))
        (when this-buffer-read-only (dtk-tone 250 50))
        (put-text-property 0 (length this-buffer-mode-name)
			   'personality 'paul-smooth
			   this-buffer-mode-name)
        (dtk-speak
         (concat 
          name
          " "
          this-buffer-mode-name
          document 
          (if (or file this-buffer-directory)
              (format " visiting %s "
                      (or file this-buffer-directory))
            "")
          with
          (format " %s "this-buffer-size)))))
     (t(emacspeak-auditory-icon 'error)
       (emacspeak-speak-line)))))
   

;;}}}
;;{{{ speech enable interactive commands 

(defadvice bs-mode (after emacspeak pre act comp)
  "Speech-enable bs mode"
  (setq voice-lock-mode t))

(defadvice bs-kill (after emacspeak pre act comp)
  "Speech-enable bs mode"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice bs-abort (after emacspeak pre act comp)
  "Speech-enable bs mode"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-spea-mode-line)))
(defadvice bs-set-configuration-and-refresh (after emacspeak pre act comp)
  "Speech-enable bs mode"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))
(defadvice bs-refresh (after emacspeak pre act comp)
  "Speech-enable bs mode"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))

(defadvice bs-view (after emacspeak pre act comp)
  "Speech-enable bs mode"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))
(defadvice bs-select (after emacspeak pre act comp)
  "Speech-enable bs mode"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))
(defadvice bs-select-other-window (after emacspeak pre act comp)
  "Speech-enable bs mode"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice bs-tmp-select-other-window (after emacspeak pre act comp)
  "Speech-enable bs mode"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice bs-select-other-frame (after emacspeak pre act comp)
  "Speech-enable bs mode"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice bs-select-in-one-window (after emacspeak pre act comp)
  "Speech-enable bs mode"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice bs-bury-buffer (after emacspeak pre act comp)
  "Speech-enable bs mode"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)))
(defadvice bs-save (after emacspeak pre act comp)
  "Speech-enable bs mode"
  (when (interactive-p)
    (emacspeak-auditory-icon 'save-object)))
(defadvice bs-toggle-current-to-show (after emacspeak pre act comp)
  "Speech-enable bs mode"
  (when (interactive-p)
    (emacspeak-auditory-icon 'button)
    (emacspeak-bs-speak-buffer-line)))
(defadvice bs-set-current-buffer-to-show-never (after emacspeak pre act comp)
  "Speech-enable bs mode"
  (when (interactive-p)
    (emacspeak-auditory-icon 'button)
    (emacspeak-bs-speak-buffer-line)))
(defadvice bs-mark-current (after emacspeak pre act comp)
  "Speech-enable bs mode"
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-bs-speak-buffer-line)))
(defadvice bs-unmark-current (after emacspeak pre act comp)
  "Speech-enable bs mode"
  (when (interactive-p)
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-bs-speak-buffer-line)))

(defadvice bs-delete (after emacspeak pre act comp)
  "Speech-enable bs mode"
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-bs-speak-buffer-line)))
(defadvice bs-delete-backward (after emacspeak pre act comp)
  "Speech-enable bs mode"
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-bs-speak-buffer-line)))

(defadvice bs-up (after emacspeak pre act comp)
  "Speech-enable bs mode"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-bs-speak-buffer-line)))
(defadvice bs-down (after emacspeak pre act comp)
  "Speech-enable bs mode"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-bs-speak-buffer-line)))

(defadvice bs-cycle-next (after emacspeak pre act comp)
  "Speech-enable bs mode"
  (when (interactive-p)
    (let ((emacspeak-speak-messages nil))
      (emacspeak-auditory-icon 'select-object)
      (emacspeak-speak-mode-line))))

(defadvice bs-cycle-previous (after emacspeak pre act comp)
  "Speech-enable bs mode"
  (when (interactive-p)
    (let ((emacspeak-speak-messages nil))
      (emacspeak-auditory-icon 'select-object)
      (emacspeak-speak-mode-line))))

;;}}}

(provide 'emacspeak-bs)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

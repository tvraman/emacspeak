;;; emacspeak-dired.el --- Speech enable Dired Mode -- A powerful File Manager
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak extension to speech enable dired
;;; Keywords: Emacspeak, Dired, Spoken Output
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
;;;Copyright (C) 1995 -- 2001, T. V. Raman 
;;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
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

;;; Commentary:
;;{{{  Introduction:

;;; This module speech enables dired.
;;; It reduces the amount of speech you hear:
;;; Typically you hear the file names as you move through the dired buffer
;;; Voicification is used to indicate directories, marked files etc.

;;}}}
;; 
;;; Code:
(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'advice)
(require 'voice-lock)
(eval-when-compile
  (require 'dired)
  (require 'emacspeak-keymap))
(require 'dtk-speak)
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
;;{{{  configure dired

(declaim (special dired-listing-switches ))
(if dired-listing-switches
(setq dired-listing-switches
      (concat "-alg"
              (substring dired-listing-switches 1)))
(setq dired-listing-switches "-alg"))

;;}}}
;;{{{  functions:

(defun emacspeak-dired-speak-line ()
  "Speak the dired line intelligently."
  (declare (special emacspeak-speak-last-spoken-word-position))
  (let ((filename (dired-get-filename t t ))
        (personality (get-text-property (point) 'personality)))
     (cond
      (filename
         (put-text-property  0  (length filename)
                             'personality personality filename )
         (dtk-speak filename)
         (setq emacspeak-speak-last-spoken-word-position (point)))
       (t (emacspeak-speak-line )))))

;;}}}
;;{{{  advice:

(defadvice dired-query (before emacspeak pre act comp)
"Produce auditory icon."
(emacspeak-auditory-icon 'ask-short-question))
(defadvice dired-quit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice dired (after emacspeak pre act)
"Produce an auditory icon."
(when (interactive-p)
(voice-lock-mode 1)
(emacspeak-dired-label-fields)
(emacspeak-auditory-icon 'open-object )))

(defadvice dired-find-file  (around  emacspeak pre act)
  "Produce an auditory icon."
  (cond
   ((interactive-p)
    (let ((directory-p (file-directory-p (dired-get-filename t t ))))
      ad-do-it
      (when directory-p
        (voice-lock-mode 1)
        (emacspeak-dired-label-fields))
      (emacspeak-auditory-icon 'open-object )))
   (t ad-do-it))
  ad-return-value)

(defadvice dired-tree-up (after emacspeak pre act)
  "Speak the filename."
  (when (interactive-p )
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-dired-speak-line)))

(defadvice dired-tree-down (after emacspeak pre act)
  "Speak the filename."
  (when (interactive-p )
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-dired-speak-line)))

(defadvice dired-next-line (after emacspeak pre act)
  "Speak the filename name."
  (when (interactive-p )
    (emacspeak-dired-speak-line)))

(defadvice dired-previous-line (after emacspeak pre act)
  "Speak the filename name."
  (when (interactive-p )
    (emacspeak-dired-speak-line)))

(defadvice dired-next-marked-file (after emacspeak pre act)
  "Speak the filename name."
  (when (interactive-p )
    (emacspeak-dired-speak-line)))

(defadvice dired-prev-marked-file  (after emacspeak pre act)
  "Speak the filename name."
  (when (interactive-p )
    (emacspeak-dired-speak-line)))
(defadvice dired-prev-subdir (after emacspeak pre act)
  "Speak the filename name."
  (when (interactive-p )
    (emacspeak-dired-speak-line)))

(defadvice dired-next-subdir (after emacspeak pre act)
  "Speak the filename name."
  (when (interactive-p )
    (emacspeak-dired-speak-line)))

(defadvice dired-mark (after emacspeak pre act)
  "Speak the filename name."
  (when (interactive-p )
    (emacspeak-dired-speak-line))
  )

(defadvice dired-flag-file-deletion (after emacspeak pre act)
  "Speak the filename name."
  (when (interactive-p )
    (emacspeak-dired-speak-line))
  )

(defadvice dired-next-dirline (after emacspeak pre act)
  "Speak the filename name."
  (when (interactive-p )
    (emacspeak-dired-speak-line))
  )

(defadvice dired-prev-dirline (after emacspeak pre act)
  "Speak the filename name."
  (when (interactive-p )
    (emacspeak-dired-speak-line))
  )

(defadvice dired-unmark-backward (after emacspeak pre act)
  "Speak the filename name."
  (when (interactive-p )
    (emacspeak-dired-speak-line)))

;;; Producing auditory icons:
;;; These dired commands do some action that causes a state change:
;;; e.g. marking a file, and then change
;;; the current selection, ie
;;; move to the next line:
;;; We speak the line moved to, and indicate the state change
;;; with an auditory icon.

(defadvice dired-mark (after emacspeak pre act)
"Produce an auditory icon."
(when (interactive-p)
(emacspeak-auditory-icon 'mark-object )
(emacspeak-dired-speak-line)))

(defadvice dired-flag-file-deletion (after emacspeak pre act )
"Produce an auditory icon indicating that a file was marked for deletion."
(when (interactive-p )
(emacspeak-auditory-icon 'delete-object )
(emacspeak-dired-speak-line )))

(defadvice dired-unmark (after emacspeak pre act)"Give speech feedback.
Also provide an auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'deselect-object )
    (emacspeak-dired-speak-line)))

;;}}}
;;{{{  labeling fields in the dired buffer:

(defun emacspeak-dired-label-fields-on-current-line ()
  "Labels the fields on a dired line.
Assumes that `dired-listing-switches' contains  -al"
  (let ((start nil)
        (fields (list "permissions"
                      "links"
                      "owner"
                      "group"
                      "size"
                      "modified in"
                      "modified on"
                      "modified at"
                      "name")))
    (save-excursion
      (beginning-of-line)
      (skip-syntax-forward " ")
      (while (and fields
                  (not (eolp)))
        (setq start (point))
        (skip-syntax-forward "^ ")
        (put-text-property start (point)
                           'field-name (car fields ))
        (setq fields (cdr fields ))
        (skip-syntax-forward " ")))))

(defun emacspeak-dired-label-fields ()
  "Labels the fields of the listing in the dired buffer.
Currently is a no-op  unless
unless `dired-listing-switches' contains -al"
  (interactive)
  (declare (special dired-listing-switches))
  (when (save-match-data
          (string-match  "al" dired-listing-switches))
    (let ((read-only buffer-read-only))
      (unwind-protect
          (progn
            (setq buffer-read-only nil)
            (save-excursion
              (goto-char (point-min))
              (dired-goto-next-nontrivial-file)
              (while (not (eobp))
                (emacspeak-dired-label-fields-on-current-line )
                (forward-line 1 ))))
        (setq buffer-read-only read-only )))))

;;}}}
;;{{{ Additional status speaking commands

(defun emacspeak-dired-show-file-type ()
  "Displays type of current file by running command file."
  (interactive)
  (let ((filename (dired-get-filename t t)))
    (if filename 
        (shell-command 
         (format "file %s"
                 filename))
      (message "No file on this line"))))

(defun emacspeak-dired-speak-header-line()
  "Speak the header line of the dired buffer. "
  (interactive)
  (emacspeak-auditory-icon 'select-object)
  (save-excursion (goto-char (point-min))
    (forward-line 2)
(emacspeak-speak-region (point-min) (point))))

(defun emacspeak-dired-speak-file-size ()
  "Speak the size of the current file.
On a directory line, run du -s on the directory to speak its size."
  (interactive)
  (let ((filename (dired-get-filename nil t))
        (size 0)
        (dtk-stop-immediately nil))
    (cond
     ((and filename
           (file-directory-p filename))
      (emacspeak-auditory-icon 'progress)
      (shell-command (format "du -s %s" filename )))
     (filename
      (setq size (nth 7 (file-attributes filename )))
                                        ; check for ange-ftp
      (when (= size -1)
        (setq size
              (nth  4
                    (split-string (thing-at-point 'line)))))
      (emacspeak-auditory-icon 'select-object)
      (message "File size %s"
               size))
     (t (message "No file on current line")))))

(defun emacspeak-dired-speak-file-modification-time ()
  "Speak modification time  of the current file."
  (interactive)
  (let ((filename (dired-get-filename nil t)))
    (cond
     (filename
      (emacspeak-auditory-icon 'select-object)
      (message "Modified on : %s"
               (format-time-string
                emacspeak-speak-time-format-string
                (nth 5 (file-attributes filename )))))
     (t (message "No file on current line")))))

(defun emacspeak-dired-speak-file-access-time ()
  "Speak access time  of the current file."
  (interactive)
  (let ((filename (dired-get-filename nil t)))
    (cond
     (filename
      (emacspeak-auditory-icon 'select-object)
      (message "Last accessed   on  %s"
               (format-time-string
                emacspeak-speak-time-format-string
                (nth 4 (file-attributes filename )))))
     (t (message "No file on current line")))))
(defun emacspeak-dired-speak-symlink-target ()
  "Speaks the target of the symlink on the current line."
  (interactive)
  (let ((filename (dired-get-filename nil t)))
    (cond
     (filename
      (emacspeak-auditory-icon 'select-object)
      (cond
       ((file-symlink-p filename)
        (message "Target is %s"
                 (file-chase-links filename)))
       (t (message "%s is not a symbolic link" filename))))
     (t (message "No file on current line")))))
(defun emacspeak-dired-speak-file-permissions ()
  "Speak the permissions of the current file."
  (interactive)
  (let ((filename (dired-get-filename nil t)))
    (cond
     (filename
      (emacspeak-auditory-icon 'select-object)
      (message "Permissions %s"
               (nth 8 (file-attributes filename ))))
     (t (message "No file on current line")))))

;;}}}
;;{{{  keys
(eval-when (load)
  (emacspeak-keymap-remove-emacspeak-edit-commands dired-mode-map))

(add-hook 'dired-mode-hook
          (function
           (lambda ()
             (declare (special dired-mode-map ))
             (define-key dired-mode-map "'" 'emacspeak-dired-show-file-type)
             (define-key  dired-mode-map "/"
               'emacspeak-dired-speak-file-permissions)
             (define-key  dired-mode-map ";"
               'emacspeak-dired-speak-header-line)
             (define-key  dired-mode-map ":" 'emacspeak-dired-speak-file-size)
             (define-key  dired-mode-map "a"
               'emacspeak-dired-speak-file-access-time)
             (define-key dired-mode-map "c"
               'emacspeak-dired-speak-file-modification-time)
             (define-key dired-mode-map "z"
               'emacspeak-dired-speak-file-size)
             (define-key dired-mode-map "t"
               'emacspeak-dired-speak-symlink-target)
             (define-key dired-mode-map "\C-i"
               'emacspeak-speak-next-field)
             (define-key dired-mode-map  "," 'emacspeak-speak-previous-field)
             (define-key dired-mode-map '[up] 'dired-previous-line)
             (define-key dired-mode-map '[down] 'dired-next-line))))

;;}}}
(provide 'emacspeak-dired)
;;{{{ emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

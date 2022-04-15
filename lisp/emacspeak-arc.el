;;; emacspeak-arc.el --- Speech enable archive-mode -- a  Emacs interface to zip and friends  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $ 
;;; Description: Auditory interface to archive mode
;;; Keywords: Emacspeak, Speak, Spoken Output, archive
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com 
;;; A speech interface to Emacs |
;;; $Date: 2008-06-21 10:50:41 -0700 (Sat, 21 Jun 2008) $ |
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
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Required modules
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'arc-mode)
;;}}}
;;{{{  Introduction 
;;; Commentary:
;;; Auditory interface to archive mode
;;; This lets Emacs manipulate package files such as .zip and .jar files.
;;; Code:
;;}}}
;;{{{ Helpers

(defun emacspeak-archive-speak-line ()
  "Speak line in archive mode intelligently"
  (end-of-line)
  (cond
   ((null (char-after (1+ (point))))
    (emacspeak-speak-line))
   (t (skip-syntax-backward "^ ")  
      (emacspeak-speak-line 1))))

;;}}}
;;{{{ fix interactive commands that need fixing 

;;}}}
;;{{{ Advice

(defadvice archive-mark (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-archive-speak-line)))

(defadvice archive-next-line (after emacspeak pre act comp)
  "Speak"
  (when (ems-interactive-p)
    (emacspeak-archive-speak-line)))

(defadvice archive-previous-line (after emacspeak pre act comp)
  "Speak"
  (when (ems-interactive-p)
    (emacspeak-archive-speak-line)))

(defadvice archive-flag-deleted (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-archive-speak-line)))

(defadvice archive-unflag (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-archive-speak-line)))
(defadvice archive-unflag-backwards (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-archive-speak-line)))

(defadvice archive-extract (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice archive-extract-other-window (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice archive-view (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{ interactive commands

(defvar emacspeak-arc-header-list-format nil
  "Field names in the header line")

(defun emacspeak-arc-get-header-line-format ()
  "Return  header line format vector, after
first initializing it if necessary."
  (cl-declare (special emacspeak-arc-header-list-format))
  (unless emacspeak-arc-header-list-format
    (let ((line nil)
          (fields nil))
      (save-excursion
        (goto-char (point-min))
        (setq line (ems--this-line)))
      (setq fields (split-string line))
      (cl-loop for f in fields 
               and i from 0
               do
               (setq emacspeak-arc-header-list-format
                     (cons
                      (list f i)
                      emacspeak-arc-header-list-format)))))
  emacspeak-arc-header-list-format)
(defun emacspeak-arc-get-field-index (field)
  (let ((marked-p
         (save-excursion
           (beginning-of-line)
           (= ?\  (following-char))))
        (pos (cadr (assoc field (emacspeak-arc-get-header-line-format)))))
    (if marked-p (1- pos) pos)))

(defun emacspeak-arc-speak-file-name ()
  "Speak the name of the file on current line"
  (interactive)
  (unless (eq major-mode 'archive-mode)
    (error "This command should be called only in archive mode"))
  (let ((entry (archive-get-descr 'no-error)))
    (cond
     ((null entry)
      (message "No file on this line"))
     (t
      (message "File: %s"
               (nth  (emacspeak-arc-get-field-index "File")
                     (split-string (ems--this-line))))))))

(defun emacspeak-arc-speak-file-size ()
  "Speak the size of the file on current line"
  (interactive)
  (unless (eq major-mode 'archive-mode)
    (error "This command should be called only in archive mode"))
  (let ((entry (archive-get-descr 'no-error)))
    (cond
     ((null entry)
      (message "No file on this line"))
     (t
      (message "Size: %s"
               (nth  (emacspeak-arc-get-field-index "Length")
                     (split-string (ems--this-line))))))))

(defun emacspeak-arc-speak-file-modification-time ()
  "Speak modification time of the file on current line"
  (interactive)
  (unless (eq major-mode 'archive-mode)
    (error "This command should be called only in archive mode"))
  (let ((entry (archive-get-descr 'no-error)))
    (cond
     ((null entry)
      (message "No file on this line"))
     (t
      (let* ((fields (split-string (ems--this-line)))
             (date (nth  (emacspeak-arc-get-field-index "Date")
                         fields))
             (time (nth  (emacspeak-arc-get-field-index "Time")
                         fields)))
        (message "Modified on %s at %s"
                 date time))))))

(defun emacspeak-arc-speak-file-permissions()
  "Speak permissions of file current entry "
  (interactive)
  (unless (eq major-mode 'archive-mode)
    (error "This command should be called only in archive mode"))
  (let ((entry (archive-get-descr 'no-error))
        (mode nil))
    (cond
     ((null entry)
      (message "No file on this line"))
     (t
      (setq mode
            (file-modes-number-to-symbolic
             (aref entry 3)))
      (message  "Permissions  %s "
                mode)))))
(defun emacspeak-arc-setup-keys ()
  "Setup emacspeak keys for arc mode"
  (cl-declare (special archive-mode-map))
  (define-key archive-mode-map "." 'emacspeak-arc-speak-file-name)
  (define-key archive-mode-map "c" 'emacspeak-arc-speak-file-modification-time)
  (define-key archive-mode-map "z" 'emacspeak-arc-speak-file-size)
  (define-key archive-mode-map "/"
    'emacspeak-arc-speak-file-permissions)
  )

(cl-eval-when (load)
  (emacspeak-arc-setup-keys))
;;}}}
(provide 'emacspeak-arc)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}

;;; emacspeak-proced.el --- Speech-enable PROCED -*- lexical-binding: t; -*-
;;
;; $Author: tv.raman.tv $
;; Description:  Speech-enable PROCED A Task manager for Emacs
;; Keywords: Emacspeak,  Audio Desktop proced Task Manager
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; 
;;  $Revision: 4532 $ |
;; Location undetermined
;; 

;;}}}
;;{{{  Copyright:
;; Copyright (C) 1995 -- 2022, T. V. Raman
;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
;; All Rights Reserved.
;; 
;; This file is not part of GNU Emacs, but the same permissions apply.
;; 
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNPROCED FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;; PROCED ==  Process Editor
;; A new Task Manager for Emacs.
;; Proced is part of emacs 23.

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Variables

(defvar emacspeak-proced-minibuffer-history nil
  "History variable to track minibuffer usage in proced.")

(defvar emacspeak-proced-fields nil
  "Association list holding field-name . column-position pairs.")
(defvar emacspeak-proced-process-cache nil
  "Cache of processes that are displayed.")

;;}}}
;;{{{ Helpers and actions

(defun emacspeak-proced-update-fields ()
  "Updates cache of field-name .column-positions alist."
  (cl-declare (special proced-header-line emacspeak-proced-fields))
  (let ((positions nil)
        (next nil)
        (header proced-header-line)
        (start 0)
        (end 0))
    (setq start (string-match "[A-Za-z%]" header))
    (while (and (<  end (length header))
                (setq end (string-match " " header start)))
      (setq next (string-match "[A-Za-z%]" header end))
      (push
       (cons (substring header start end)
             (cons start (1- next)))
       positions)
      (setq start next))
    (push
     (cons (substring header start)
           (cons start  (window-width)))
     positions)
    (setq emacspeak-proced-fields
          (nreverse positions))))

;; Destructuring: (field-name . (start . end))
(defun emacspeak-proced-field-name (entry)
  "Return field name."
  (car entry))

(defun emacspeak-proced-field-start (entry)
  "Return start column."
  (cadr entry))

(defun emacspeak-proced-field-end (entry)
  "Return end column."
  (cddr entry))

(defun emacspeak-proced-field-to-position (field)
  "Return column position of this field."
  (cl-declare (special emacspeak-proced-fields))
  (cdr (assoc-string field emacspeak-proced-fields)))

(defun emacspeak-proced-position-to-field (pos)
  "Return field  for this position."
  (cl-declare (special emacspeak-proced-fields))
  (let ((fields emacspeak-proced-fields)
        (field nil)
        (range nil)
        (found nil))
    (while (and fields (not found))
      (setq field (car fields))
      (setq range (cdr field))
      (setq fields (cdr fields))
      (when (and
             (<= (car range) pos)
             (<= pos (cdr range)))
        (setq found t)))
    field))

(defun emacspeak-proced-speak-this-field (&optional position)
  "Speak field at specified column --- defaults to current column."
  (interactive)
  (setq position (or position (current-column)))
  (let ((field (emacspeak-proced-position-to-field position))
        (start nil)
        (end nil))
    (save-excursion
      (goto-char
       (+ (line-beginning-position)
          (emacspeak-proced-field-start field)))
      (setq start (point))
      (when (looking-at "[^ ]")
        (skip-syntax-backward "^ ")
        (setq start (point)))
      (skip-syntax-forward " ")
      (skip-syntax-forward "^ ")
      (setq end (point))
      (when (equal field (car (last emacspeak-proced-fields)))
        (setq end (line-end-position)))
      (message
       "%s: %s"
       (emacspeak-proced-field-name field)
       (buffer-substring start end)))))

(defun emacspeak-proced-speak-that-field ()
  "Speak desired field via single keystroke."
  (interactive)
  (cl-case (read-char "?")
    (?u (emacspeak-proced-speak-field 'user))
    (?p (emacspeak-proced-speak-field 'pid))
    (?c (emacspeak-proced-speak-field 'pcpu))
    (?m (emacspeak-proced-speak-field 'pmem))
    (?v (emacspeak-proced-speak-field 'vsz))
    (?r (emacspeak-proced-speak-field 'rss))
    (?T (emacspeak-proced-speak-field 'tty))
    (?S (emacspeak-proced-speak-field 'stat))
    (?s (emacspeak-proced-speak-field 'start))
    (?t (emacspeak-proced-speak-field 'time))
    (?a (emacspeak-proced-speak-field 'args))
    (otherwise (message "Pick field using mnemonic chars"))
    (sit-for 1)))
(defun emacspeak-proced-speak-args ()
  "Speak command  invocation  for this process."
  (interactive)
  (emacspeak-proced-speak-field 'args))

(defun emacspeak-proced-next-field ()
  "Navigate to next field."
  (interactive)
  (cl-declare (special emacspeak-proced-fields))
  (let ((tabs emacspeak-proced-fields))
    (while
        (and tabs
             (>= (current-column) (emacspeak-proced-field-start (car tabs))))
      (setq tabs (cdr tabs)))
    (cond
     ((null tabs) (error "On last field "))
     (t
      (goto-char
       (+ (line-beginning-position)
          (emacspeak-proced-field-start (car tabs))))
      (emacspeak-auditory-icon 'large-movement)
      (when (called-interactively-p 'interactive)
        (emacspeak-proced-speak-this-field))))))

(defun emacspeak-proced-previous-field ()
  "Navigate to previous field."
  (interactive)
  (cl-declare (special emacspeak-proced-fields))
  (let ((tabs emacspeak-proced-fields)
        (target nil))
    (forward-char -1)
    (while
        (and tabs
             (>= (current-column) (emacspeak-proced-field-start (car tabs))))
      (setq target (car tabs)
            tabs (cdr tabs)))
    (cond
     ((null target) (error "On first field "))
     (t
      (goto-char
       (+ (line-beginning-position)
          (emacspeak-proced-field-start target)))
      (when (called-interactively-p 'interactive)
        (emacspeak-auditory-icon 'large-movement)
        (emacspeak-proced-speak-this-field))))))

(defun emacspeak-proced-speak-field (field-name)
  "Speak value of specified field in current line."
  (interactive
   (list
    (let ((completion-ignore-case t))
      (intern
       (completing-read
        "Field: "
        (mapcar
         #'car
         (cdr (assoc (get-text-property (point) 'proced-pid)
                     proced-process-alist)))
        nil t nil)))))
  (cl-declare (special proced-process-alist))
  (let ((value
         (cdr
          (assoc
           field-name
           (assoc (get-text-property (point) 'proced-pid)
                  proced-process-alist)))))
    (message "%s: %s" field-name value)))

(defun emacspeak-proced-add-keys ()
  "Add additional keybindings for emacspeak."
  (cl-declare (special proced-mode-map))
  (define-key proced-mode-map "a" 'emacspeak-proced-speak-args)
  (define-key proced-mode-map "n" 'emacspeak-proced-next-line)
  (define-key proced-mode-map "p" 'emacspeak-proced-previous-line)
  (define-key proced-mode-map "j" 'emacspeak-proced-jump-to-process)
  (define-key proced-mode-map "\t" 'emacspeak-proced-next-field)
  (define-key proced-mode-map [S-tab] 'emacspeak-proced-previous-field)
  (define-key proced-mode-map [backtab] 'emacspeak-proced-previous-field)
  (define-key proced-mode-map "." 'emacspeak-proced-speak-field)
  (define-key proced-mode-map "<" 'beginning-of-buffer)
  (define-key proced-mode-map ">" 'end-of-buffer)
  (define-key proced-mode-map ";" 'emacspeak-proced-speak-that-field)
  (define-key proced-mode-map "," 'emacspeak-proced-speak-this-field))

(add-hook 'proced-mode-hook #'emacspeak-proced-add-keys)

(defun emacspeak-proced-update-process-cache ()
  "Update cache of processes we are displaying."
  (cl-declare (special emacspeak-proced-process-cache))
  (let ((cache nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (goto-char (+ (line-beginning-position)
                      (car (emacspeak-proced-field-to-position "Args"))))
        (push
         (buffer-substring-no-properties (point)
                                         (line-end-position))
         cache)
        (forward-line 1))
      (setq emacspeak-proced-process-cache (nreverse cache)))))

(defun emacspeak-proced-jump-to-process (name)
  "Jump to process by name."
  (interactive
   (list
    (completing-read
     "Jump to process: "
     emacspeak-proced-process-cache)))
  (cl-declare (special emacspeak-proced-process-cache))
  (let ((pos (cl-position name  emacspeak-proced-process-cache
                          :test #'string-equal)))
    (cond
     (pos
      (forward-line (1+ pos))
      (emacspeak-proced-speak-this-field))
     (t (error "Can't find %s" name)))))

;;}}}
;;{{{ Advice interactive commands:

(defadvice proced-mark (before emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-proced-speak-this-field)))

(defadvice proced-unmark (before emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-proced-speak-this-field)))

(defadvice proced-mark-all (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (message "Marked all processes. ")
    (emacspeak-auditory-icon 'mark-object)))

(defadvice proced-unmark-all (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (message "Removed all marks. ")
    (emacspeak-auditory-icon 'deselect-object)))

(cl-loop
 for f in
 '(proced proced-update)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Update cache of field positions."
     (let ((emacspeak-speak-messages nil))
       ad-do-it
       (emacspeak-proced-update-fields)
       (emacspeak-proced-update-process-cache)
       (when (ems-interactive-p)
         (emacspeak-auditory-icon 'open-object)
         (funcall-interactively #'emacspeak-speak-mode-line))))))

(cl-loop
 for f  in
 '(proced-sort-pcpu proced-sort-start
                    proced-sort-time proced-sort-interactive
                    proced-sort-user  proced-sort-pmem
                    proced-sort-pid)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedbak."
     (when (ems-interactive-p)
       (emacspeak-proced-speak-this-field)
       (emacspeak-auditory-icon 'task-done)))))

;;}}}
;;{{{ additional commands:

(defun emacspeak-proced-next-line ()
  "Move to next line and speak a summary."
  (interactive)
  (goto-char (line-end-position))
  (cond
   ((eobp) (error "On last line."))
   (t (forward-line 1)
      (skip-syntax-forward " ")
      (emacspeak-proced-speak-field 'args))))

(defun emacspeak-proced-previous-line ()
  "Move to next line and speak a summary."
  (interactive)
  (goto-char (line-beginning-position))
  (cond
   ((bobp) (error "On first line"))
   (t (forward-line -1)
      (beginning-of-line)
      (skip-syntax-forward " ")
      (emacspeak-proced-speak-field 'args))))

;;}}}
(provide 'emacspeak-proced)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}

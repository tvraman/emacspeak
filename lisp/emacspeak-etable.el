;;; emacspeak-etable.el --- Speech enable table.el  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $ 
;;; DescriptionEmacspeak extensions for table.el
;;; Keywords:emacspeak, audio interface to emacs Tables
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com 
;;; A speech interface to Emacs |
;;; $Date: 2008-06-21 14:58:40 -0700 (Sat, 21 Jun 2008) $ |
;;;  $Revision: 4532 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2021, T. V. Raman 
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}

;;{{{  Introduction:
;;; Commentary:
;;; table.el provides rich table editing for emacs.
;;; this module speech-enables table.el
;;; Code:
;;}}}
;;{{{ required modules 

(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
  
(require 'table )
;;}}}
;;{{{ Update command remap list.
(defadvice table--make-cell-map(after emacspeak pre act comp)
  "Set up emacspeak for table.el"
  (cl-declare (special table-cell-map))
  (when  table-cell-map
    (cl-loop for k in
             (where-is-internal 'emacspeak-self-insert-command (list table-cell-map))
             do
             (define-key table-cell-map k '*table--cell-self-insert-command))
    (cl-loop for k in
             '(
               ("S-TAB" table-backward-cell)
               ("\C-e." emacspeak-etable-speak-cell))
             do
             (emacspeak-keymap-update table-cell-map k))))

;;}}}
;;{{{ Advice edit commands

(defadvice *table--cell-delete-char (around emacspeak pre act comp)
  "Speak character you're deleting."
  (cond
   ((ems-interactive-p)
    (dtk-tone 500 100 'force)
    (emacspeak-speak-char t)
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice *table--cell-delete-backward-char (around emacspeak pre act comp)
  "Speak character you're deleting."
  (cond
   ((ems-interactive-p)
    (dtk-tone 500 100 'force)
    (emacspeak-speak-this-char (preceding-char))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice *table--cell-self-insert-command (after emacspeak pre act comp)
  "Provide spoken output."
  (when  (ems-interactive-p)
    (cond
     ((and (= 32 last-input-event)
           emacspeak-word-echo)
      (save-excursion
        (let ((orig (point)))
          (table--finish-delayed-tasks)
          (backward-word 1)
          (emacspeak-speak-region orig (point)))))
     (emacspeak-character-echo
      (dtk-stop)
      (emacspeak-speak-this-char last-input-event)))))

(defadvice *table--cell-quoted-insert  (after emacspeak pre act comp)
  "Speak the character that was inserted."
  (when (ems-interactive-p)
    (table--finish-delayed-tasks)
    (emacspeak-speak-this-char (preceding-char))))

(defadvice *table--cell-newline (before emacspeak pre act comp)
  "Speak the previous line if line echo is on.
See command \\[emacspeak-toggle-line-echo].  Otherwise cue the user to
the newly created blank line."
  (cl-declare (special emacspeak-line-echo))
  (when (ems-interactive-p)
    (table--finish-delayed-tasks)
    (cond
     (emacspeak-line-echo (emacspeak-speak-line))
     (t(if dtk-stop-immediately (dtk-stop))
       (dtk-tone 225 120 'force)))))

(defadvice *table--cell-newline-and-indent (around emacspeak pre act comp)
  "Speak the previous line if line echo is on.
See command \\[emacspeak-toggle-line-echo].
Otherwise cue user to the line just created."
  (cl-declare (special emacspeak-line-echo))
  (cond
   ((ems-interactive-p)
    (cond
     (emacspeak-line-echo (emacspeak-speak-line))
     (t (dtk-speak-using-voice voice-annotate
                               (format
                                "indent %s"
                                (current-column)))
        (dtk-force)))))
  ad-do-it
  ad-return-value)

(defadvice *table--cell-open-line (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (let ((count (ad-get-arg 0)))
      (emacspeak-auditory-icon 'open-object)
      (message "Opened %s blank line%s"
               (if (= count 1) "a" count)
               (if (= count 1) "" "s")))))

;;}}}

;;{{{ speak cell contents:

(defun emacspeak-etable-speak-cell ()
  "Speak current cell."
  (interactive)
  (let ((cell (table--probe-cell 'no-error)))
    (cond
     (cell
      (emacspeak-speak-rectangle
       (car cell)
       (cdr cell)))
     (t (error "Can't identify cell.")))))

(cl-loop for f in
         '(table-forward-cell table-backward-cell)
         do
         (eval
          `(defadvice ,f (after emacspeak pre act comp)
             "speak by speaking current cell
      contents."
             (when (ems-interactive-p)
               (table--finish-delayed-tasks)
               (emacspeak-auditory-icon 'select-object)
               (emacspeak-etable-speak-cell)))))

;;}}}
(provide  'emacspeak-etable)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}

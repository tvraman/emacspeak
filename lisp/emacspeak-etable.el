;;; emacspeak-etable.el --- Speech enable table.el
;;; $Id$
;;; $Author$ 
;;; DescriptionEmacspeak extensions for table.el
;;; Keywords:emacspeak, audio interface to emacs Tables
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision: 4532 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, T. V. Raman 
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

;;{{{  Introduction:

;;; table.el provides rich table editting for emacs.
;;; this module speech-enables table.el

;;}}}
;;{{{ required modules 

(require 'emacspeak-preamble)
(eval-when-compile
  (condition-case nil
      (require 'table)
    (error "table.el is only available in newer Emacsuns")))

;;}}}
;;{{{ Update command remap list.
(defadvice table--make-cell-map(after emacspeak pre act comp)
  "Set up emacspeak for table.el"
  (declare (special table-cell-map))
  (when  table-cell-map
    (loop for k in
          (where-is-internal 'emacspeak-self-insert-command table-cell-map)
          do
          (define-key table-cell-map k '*table--cell-self-insert-command ))
    (loop for k in
          '(
            ([(shift tab)] table-backward-cell)
            ("\C-e." emacspeak-etable-speak-cell))
          do
          (emacspeak-keymap-update table-cell-map k))))

;;}}}
;;{{{ Advice edit commands

(defadvice *table--cell-delete-char (around emacspeak pre act)
  "Speak character you're deleting."
  (cond
   ((interactive-p )
    (dtk-tone 500 30 'force)
    (emacspeak-speak-char t)
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice *table--cell-delete-backward-char (around emacspeak pre act)
  "Speak character you're deleting."
  (cond
   ((interactive-p )
    (dtk-tone 500 30 'force)
    (emacspeak-speak-this-char (preceding-char ))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice *table--cell-self-insert-command (after emacspeak pre act comp)
  "Provide spoken output."
  (when  (interactive-p)
    (cond
     ((and (= 32 last-input-char)
           emacspeak-word-echo)
      (save-excursion
        (let ((orig (point)))
          (table--finish-delayed-tasks)
          (backward-word 1)
          (emacspeak-speak-region orig (point)))))
     (emacspeak-character-echo
      (when dtk-stop-immediately-while-typing (dtk-stop))
      (emacspeak-speak-this-char last-input-char )))))

(defadvice *table--cell-quoted-insert  (after emacspeak pre act )
  "Speak the character that was inserted."
  (when (interactive-p)
    (table--finish-delayed-tasks)
    (emacspeak-speak-this-char (preceding-char ))))

(defadvice *table--cell-newline (before emacspeak pre act)
  "Speak the previous line if line echo is on.
See command \\[emacspeak-toggle-line-echo].  Otherwise cue the user to
the newly created blank line."
  (declare (special emacspeak-line-echo ))
  (when (interactive-p)
    (table--finish-delayed-tasks)
    (cond
     (emacspeak-line-echo (emacspeak-speak-line ))
     (t(if dtk-stop-immediately (dtk-stop))
       (dtk-tone 225 120 'force   )))))

(defadvice *table--cell-newline-and-indent (around emacspeak pre act)
  "Speak the previous line if line echo is on.
See command \\[emacspeak-toggle-line-echo].
Otherwise cue user to the line just created."
  (declare (special emacspeak-line-echo ))
  (cond
   ((interactive-p)
    (cond
     (emacspeak-line-echo
      (emacspeak-speak-line )
      ad-do-it)
     (t ad-do-it
        (dtk-speak-using-voice voice-annotate
                               (format
                                "indent %s"
                                (current-column)))
        (dtk-force))))
   (t ad-do-it))
  ad-return-value)

(defadvice *table--cell-open-line (after emacspeak pre act )
  "Provide auditory feedback."
  (when (interactive-p)
    (let ((count (ad-get-arg 0)))
      (emacspeak-auditory-icon 'open-object)
      (message "Opened %s blank line%s"
               (if (= count 1) "a" count)
               (if (= count 1 ) "" "s")))))

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
     (t (error "Cant identify cell.")))))

(loop for f in
      '(table-forward-cell table-backward-cell)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback by speaking current cell
      contents."
          (when (interactive-p)
            (table--finish-delayed-tasks)
            (emacspeak-auditory-icon 'select-object)
            (emacspeak-etable-speak-cell)))))

;;}}}
(provide  'emacspeak-etable)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

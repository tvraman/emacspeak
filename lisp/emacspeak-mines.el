;;; emacspeak-mines.el --- Speech-enable MINES  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable MINES An Emacs Interface to mines
;; Keywords: Emacspeak,  Audio Desktop mines
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
;; Copyright (C) 1995 -- 2007, 2011, T. V. Raman
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
;; MERCHANTABILITY or FITNMINES FOR A PARTICULAR PURPOSE.  See the
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
;; MINES == Minesweeper game in emacs. The game itself provides a
;; fully keyboard driven interface. In addition, Emacspeak provides
;; these additional interactive commands:
;; @itemize @bullet
;; @item @kbd{SPC} Speak current cell.
;; @item @kbd{.} Speak neighbors of current cell.
;; @item @kbd{,} Speak number of marks
;; @item @kbd{a} Move to beginning of row.
;; @item @kbd{e} Move to end of row.
;; @item @kbd{g} Move to specified cell 
;; @item @kbd{s} Move to next uncovered cell.
;; @item @kbd{/} Speak number of remaining uncovered cells.
;; @item @kbd{'} Speaks entire board.
;; @end itemize
;; 
;; Speaking cell neighbors uses appropriate clause boundaries to group
;; related cells --- neighbors are read left-to-right, top-to-bottom.
;; Moving to the left/right edge of the board produces an appropriate
;; auditory icon.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'mines "mines" 'no-error)
;;}}}
;;{{{ Interactive Commands:

(defun emacspeak-mines-speak-cell ()
  "Speak current cell."
  (interactive)
  (let* ((pos (mines-index-2-matrix (mines-current-pos)))
         (row (cl-first pos))
         (column (cl-second pos)))
    (when (= 0 column) (emacspeak-auditory-icon 'left))
    (when (= 7 column) (emacspeak-auditory-icon 'right))
    (when (or (= row 0) (= row 7)) (emacspeak-auditory-icon 'large-movement))
    (dtk-speak
     (format "%c in row %s column %s" (following-char) row column))))

(defun emacspeak-mines-speak-uncovered-count ()
  "Speak number of uncovered cells."
  (interactive)
  (cl-declare (special mines-number-mines))
  (cl-declare (special mines-state))
  (dtk-speak
   (format "%d mines with %d uncovered cells remaining."
           mines-number-mines (cl-count-if #'null mines-state))))

(defun emacspeak-mines-jump-to-uncovered-cell (from-beginning)
  "Jump to next uncovered cell. With interactive prefix-arg, jump
to beginning of board before searching."
  (interactive "P")
  (when from-beginning (mines-goto 0))
  (forward-char 1)
  (let ((found (search-forward "."nil t)))
    (when found (backward-char 1))
    (if found
        (emacspeak-mines-speak-cell)
      (message "No uncovered cell here. "))))

(defun emacspeak-mines-goto (index)
  "Move to specified cell."
  (interactive "nCell: ")
  (mines-goto index)
  (emacspeak-mines-speak-cell))

(defun emacspeak-mines-speak-mark-count  ()
  "Count and speak number of marks."
  (interactive)
  (cl-declare (special mines-flagged-cell-char))
  (let ((count 0) ;;; fix over-counting 
        (m (format "%c" mines-flagged-cell-char)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward  m nil t) (cl-incf count) (forward-char 1)))
    (message "%d marks" count)))
(defun emacspeak-mines-speak-board ()
  "Speak the board."
  (interactive)
  (cl-declare (special  mines-number-cols mines-grid))
  (let ((cells nil))
    (save-excursion
      (setq cells
            (cl-loop
             for i from 0 to (1- (length mines-state)) collect
             (progn
               (mines-goto i)
               (let ((v (aref mines-state i))
                     (n (aref mines-grid i)))
                 (cond
                  ((and (null v)(get-text-property (point) 'flag))
                   " M")
                  ((null v) "dot")
                  ((and v (numberp n))  (format "%d" n))
                  ((eq '@ v)  "at")
                  (t (message "Should not  get here"))))))))
    (dtk-speak-list cells mines-number-cols)))

(defun emacspeak-mines-init ()
  "Setup additional keys for playing minesweeper."
  (cl-declare (special mines-mode-map mines-flagged-cell-char))
  (setq mines-flagged-cell-char ?M)
  (cl-loop
   for b in
   '(("." emacspeak-mines-speak-neighbors)
     ("," emacspeak-mines-speak-mark-count)
     ("SPC" emacspeak-mines-speak-cell)
     ("/" emacspeak-mines-speak-uncovered-count)
     ("'" emacspeak-mines-speak-board)
     ("a" emacspeak-mines-beginning-of-row)
     ("e" emacspeak-mines-end-of-row)
     ("g" emacspeak-mines-goto)
     ("s" emacspeak-mines-jump-to-uncovered-cell))
   do
   (define-key mines-mode-map (ems-kbd (cl-first b)) (cl-second b))))

(eval-after-load  "mines"
  `(progn (emacspeak-mines-init)))

(defun emacspeak-mines-cell-flagged-p (c)
  "Predicate to check if cell at index c is flagged."
  (save-excursion
    (mines-goto c)
    (get-text-property (point) 'flag)))

(defun emacspeak-mines-speak-neighbors ()
  "Speak neighboring cells in sorted order."
  (interactive)
  (cl-declare (special mines-state mines-grid))
  (let* ((current (mines-current-pos))
         (cells (sort (mines-get-neighbours current) #'<))
         (pos (mines-index-2-matrix current))
         (row (cl-first pos))
         (count (length cells))
         (values (mapcar #'(lambda (c) (aref mines-state c)) cells))
         (numbers (mapcar #'(lambda (c) (aref mines-grid c)) cells))
         (result nil)
         (group nil))
    (cl-loop
     for c in cells
     and v in values
     and n in numbers do
     (cond
      ((and (null v) (emacspeak-mines-cell-flagged-p c))
       (push "M" result))
      ((null v) (push "dot" result))
      ((and v (numberp n)) (push (format "%d" n) result))
      ((eq '@ v) (push "at" result))
      (t (message "Should not  get here"))))
    (setq
     group
     (cond
      ((and (= 3 count) (= 0 row)) ;;; top corners
       '(1 2))
      ((and (= 3 count) (= 7 row)) ;;; bottom corners
       '(2 1))
      ((and (= 5 count) (= 0 row)) ;;; top
       '(2 3))
      ((and (= 5 count) (= 7 row)) ;;; bottom
       '(3 2))
      ((= 5 count) ;;; left/right edge
       '(2 1 2))
      (t '(3 2 3))))
    (dtk-speak-list (nreverse result) group)))
(defun emacspeak-mines-beginning-of-row  ()
  "Move to beginning of row"
  (interactive)
  (let ((row (cl-first (mines-index-2-matrix (mines-current-pos)))))
    (mines-goto (* row mines-number-cols))
    (emacspeak-mines-speak-cell)))

(defun emacspeak-mines-end-of-row  ()
  "Move to end of row"
  (interactive)
  (let ((row (cl-first (mines-index-2-matrix (mines-current-pos)))))
    (mines-goto (+ (1- mines-number-cols)(* row mines-number-cols)))
    (emacspeak-mines-speak-cell)))

;;}}}
;;{{{ Advice Interactive Commands
(defadvice mines (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (dtk-speak "New Minesweeper game")
    (emacspeak-auditory-icon 'open-object)))

(cl-loop
 for f in
 '(mines-go-down
   mines-go-left
   mines-go-right
   mines-go-up)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-mines-speak-cell)))))

(defadvice mines-dig (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (unless mines-game-over (emacspeak-mines-speak-cell))))

(defadvice mines-flag-cell (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)

    (if (eq t (aref mines-grid (mines-current-pos)))
        (emacspeak-auditory-icon 'close-object)
      (emacspeak-auditory-icon 'mark-object))
    (emacspeak-mines-speak-cell)))

(defadvice mines-game-over (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'shutdown)))

(defadvice mines-game-completed(after emacspeak pre act comp)
  "Provide an auditory icon."
  (emacspeak-auditory-icon 'task-done))

;;}}}

(provide 'emacspeak-mines)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}

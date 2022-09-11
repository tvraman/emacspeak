;;; emacspeak-sudoku.el --- Play SuDoku  -*- lexical-binding: t; -*- 
;;
;; $Author: tv.raman.tv $
;; Description: Playing SuDoku ;;; Keywords: Emacspeak, sudoku
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

;; Copyright (c) 1995 -- 2022, T. V. Raman
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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ Introduction:

;;; Commentary:

;; Playing SuDoku using speech output.
;; Written to discover what type of feedback one needs for  this
;; task.
;; See http://emacspeak.blogspot.com/2006/02/playing-sudoku-using-auditory-feedback.html

;;}}}
;;{{{  Required modules
;;; Code:
(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'sudoku "sudoku" 'no-error)
;;}}}
;;{{{Forward Decl:

(declare-function sudoku-column "sudoku" (board n))
(declare-function sudoku-subsquare "sudoku" (board n))
(declare-function sudoku-get-cell-from-point "sudoku" (num))
(declare-function sudoku-row "sudoku" (board n))
(declare-function sudoku-cell "sudoku" (board x y))
(declare-function sudoku-cell-possibles "sudoku" (board x y))
(declare-function sudoku-remaining-cells "sudoku" (board))
(declare-function sudoku-goto-cell "sudoku" (coords))
(declare-function sudoku-change-cell "sudoku" (board x y input))
(declare-function sudoku-board-print "sudoku" (board message))

;;}}}
;;{{{ Define additional speak commands:

(defun emacspeak-sudoku-board-summarizer ()
  "Dispatch to  appropriate summarizer.

d   Number Distribution
r   Row Distribution
c   Column Distribution
s   Sub-square Distribution.
"
  (interactive)
  (let ((c (read-char "Summary: ")))
    (cl-case c
      (?d (call-interactively 'emacspeak-sudoku-board-distribution-summarize))
      (?r (call-interactively 'emacspeak-sudoku-board-rows-summarize))
      (?c (call-interactively
           'emacspeak-sudoku-board-columns-summarize))
      (?s (call-interactively
           'emacspeak-sudoku-board-sub-squares-summarize))
      (otherwise (message "Unknown summary type?")))))

(defun emacspeak-sudoku-board-distribution-summarize ()
  "Shows distribution of filled numbers."
  (interactive)
  (cl-declare (special current-board))
  (let ((counts (make-vector 9 0)))
    (cl-loop for row in current-board
             do
             (cl-loop for v in row
                      do
                      (if (> v 0)
                          (cl-incf (aref counts (1- v))))))
    (dtk-speak-list
     (cl-loop for i across counts collect i)
     3)))

(defun emacspeak-sudoku-board-rows-summarize ()
  "Summarize rows --- speaks number of remaining cells."
  (interactive)
  (cl-declare (special current-board))
  (dtk-speak-list
   (cl-loop for r in current-board
            collect  (cl-count 0 r))
   3))

(defun emacspeak-sudoku-board-columns-summarize ()
  "Summarize columns --- speaks number of remaining cells."
  (interactive)
  (cl-declare (special current-board))
  (dtk-speak-list
   (cl-loop for c from 0 to 8
            collect  (cl-count 0 (sudoku-column current-board c)))
   3))

(defun emacspeak-sudoku-board-sub-squares-summarize ()
  "Summarize sub-squares --- speaks number of remaining cells."
  (interactive)
  (cl-declare (special current-board))
  (dtk-speak-list
   (cl-loop for s from 0 to 8
            collect  (cl-count 0 (sudoku-subsquare current-board s)))
   3))

(defun emacspeak-sudoku-speak-current-cell-coordinates ()

  "speak current cell coordinates."
  (interactive)
  (let ((row (cl-second (sudoku-get-cell-from-point (point))))
        (column (cl-first (sudoku-get-cell-from-point (point)))))
    (message
     (format "Row %s Column %s"
             row column))))

(defun emacspeak-sudoku-speak-current-row ()
  "Speak current row."
  (interactive)
  (cl-declare (special current-board))
  (let ((cell (sudoku-get-cell-from-point (point))))
    (dtk-speak-list (sudoku-row current-board
                                (cl-second cell))
                    3)))

(defun emacspeak-sudoku-speak-current-column ()
  "Speak current column."
  (interactive)
  (cl-declare (special current-board))
  (let ((cell (sudoku-get-cell-from-point (point))))
    (dtk-speak-list (sudoku-column  current-board
                                    (cl-first cell))
                    3)))

(defun emacspeak-sudoku-cell-sub-square (cell)
  "Return sub-square that this cell is in."
  (let ((row (cl-second cell))
        (column (cl-first cell)))
    (+ (* 3 (/ row 3))
       (/ column 3))))

(defun emacspeak-sudoku-speak-current-sub-square ()
  "Speak current sub-square."
  (interactive)
  (cl-declare (special current-board))
  (let ((cell (sudoku-get-cell-from-point (point))))
    (dtk-speak-list
     (sudoku-subsquare  current-board
                        (emacspeak-sudoku-cell-sub-square cell))
     3)))

(defun emacspeak-sudoku-speak-current-cell-value ()
  "Speak value in current cell."
  (interactive)
  (cl-declare (special current-board))
  (let ((cell (sudoku-get-cell-from-point (point))))
    (dtk-speak
     (sudoku-cell current-board (cl-first cell) (cl-second cell)))))

(defun emacspeak-sudoku-hint ()
  "Provide hint for current cell."
  (interactive)
  (cl-declare (special current-board))
  (let* ((cell (sudoku-get-cell-from-point (point)))
         (possibles (sudoku-cell-possibles
                     current-board
                     (cl-first cell)
                     (cl-second cell))))
    (cond
     (possibles 
      (dtk-speak-list possibles))
     (t (message "Dead End")))))

(defun emacspeak-sudoku-speak-remaining-in-row ()
  "Speaks number of remaining cells in current row."
  (interactive)
  (cl-declare (special current-board))
  (let ((cell (sudoku-get-cell-from-point (point))))
    (dtk-speak
     (cl-count 0
               (sudoku-row current-board (cl-second cell))))))

(defun emacspeak-sudoku-speak-remaining-in-column ()
  "Speaks number of remaining cells in current column."
  (interactive)
  (cl-declare (special current-board))
  (let ((cell (sudoku-get-cell-from-point (point))))
    (dtk-speak
     (cl-count 0
               (sudoku-column current-board  (cl-first cell))))))

(defun emacspeak-sudoku-speak-remaining-in-sub-square ()
  "Speaks number of remaining cells in current sub-square."
  (interactive)
  (cl-declare (special current-board))
  (let ((cell (sudoku-get-cell-from-point (point))))
    (dtk-speak
     (cl-count 0
               (sudoku-subsquare current-board
                                 (emacspeak-sudoku-cell-sub-square cell))))))
(defun emacspeak-sudoku-how-many-remaining ()
  "Speak number of remaining squares to fill."
  (interactive)
  (cl-declare (special current-board))
  (message
   "%s squares remain"
   (sudoku-remaining-cells current-board)))

;;}}}
;;{{{ additional navigation by sub-square

(defun emacspeak-sudoku-move-to-sub-square (step)
  "Move to sub-square specified as delta from current
  sub-square."
  (let* ((cell  (sudoku-get-cell-from-point (point)))
         (this (emacspeak-sudoku-cell-sub-square cell)))
    (setq this
          (max 0 (min 8 (+ this step))))
    (sudoku-goto-cell
     (list (* (% this 3) 3)
           (* (/ this 3) 3)))
    (if (eq (get-text-property  (point) 'face) 'bold)
        (emacspeak-auditory-icon 'item)
      (emacspeak-auditory-icon 'select-object))
    (emacspeak-sudoku-speak-current-cell-value)))

(defun emacspeak-sudoku-up-sub-square ()
  "Move to top-left corner of  sub-square above current one."
  (interactive)
  (emacspeak-sudoku-move-to-sub-square -3))

(defun emacspeak-sudoku-down-sub-square ()
  "Move to top-left corner of  sub-square below current one."
  (interactive)
  (emacspeak-sudoku-move-to-sub-square 3))

(defun emacspeak-sudoku-next-sub-square ()
  "Move to top-left corner of next sub-square."
  (interactive)
  (emacspeak-sudoku-move-to-sub-square 1))
(defun emacspeak-sudoku-previous-sub-square ()
  "Move to top-left corner of previous sub-square."
  (interactive)
  (emacspeak-sudoku-move-to-sub-square -1))

;;}}}
;;{{{ erase rows, columns or sub-squares:

(defun emacspeak-sudoku-erase-these-cells (cell-list)
  "Erase cells in cell-list taking account of original values."
  (cl-declare (special start-board current-board
                       sudoku-onscreen-instructions))
  (let ((original (sudoku-get-cell-from-point (point))))
    (cl-loop for cell in cell-list
             do
             (let ((x (car cell))
                   (y (cadr  cell)))
               (when (= (sudoku-cell start-board x y) 0)
                 (setq current-board (sudoku-change-cell current-board x y 0)))))
    (setq buffer-read-only nil)
    (erase-buffer)
    (sudoku-board-print current-board
                        sudoku-onscreen-instructions)
    (sudoku-goto-cell original)
    (setq buffer-read-only t)))

(defun emacspeak-sudoku-erase-current-row ()
  "Erase current row."
  (interactive)
  (cl-declare (special current-board))
  (let ((cell (sudoku-get-cell-from-point (point))))
    (emacspeak-sudoku-erase-these-cells
     (cl-loop for i from 0 to  8
              collect  (list i (cl-second cell)))))
  (when (called-interactively-p 'interactive)
    (emacspeak-auditory-icon 'delete-object)))

(defun emacspeak-sudoku-erase-current-column ()
  "Erase current column."
  (interactive)
  (cl-declare (special current-board))
  (let ((cell (sudoku-get-cell-from-point (point))))
    (emacspeak-sudoku-erase-these-cells
     (cl-loop for i from 0 to  8
              collect  (list (cl-first cell) i))))
  (when (called-interactively-p 'interactive)
    (emacspeak-auditory-icon 'delete-object)))

(defun emacspeak-sudoku-sub-square-cells (square)
  "Return list of cells in sub-square."
  (let ((row-start (* (/ square 3)  3)) 
        (col-start (* (% square 3)  3)))
    
    (cl-loop for r from row-start to (+ 2 row-start)
             nconc
             (cl-loop  for c from col-start to (+ 2 col-start)
                       collect (list c r)))))

(defun emacspeak-sudoku-erase-current-sub-square ()
  "Erase current sub-square."
  (interactive)
  (let* ((square
          (emacspeak-sudoku-cell-sub-square
           (sudoku-get-cell-from-point (point))))
         (square-cells (emacspeak-sudoku-sub-square-cells square)))
    (emacspeak-sudoku-erase-these-cells square-cells))
  (when (called-interactively-p 'interactive)
    (emacspeak-auditory-icon 'delete-object)))

;;}}}
;;{{{ advice motion:

(cl-loop for f   in
         '(
           sudoku-move-point-left 
           sudoku-move-point-leftmost 
           sudoku-move-point-right 
           sudoku-move-point-rightmost 
           sudoku-move-point-up 
           sudoku-move-point-upmost 
           sudoku-move-point-down 
           sudoku-move-point-downmost)
         do
         (eval
          `(defadvice ,f (after emacspeak pre act comp)
             "Produce auditory output."
             (when (ems-interactive-p)
               (emacspeak-sudoku-speak-current-cell-value)
               (if (eq (get-text-property  (point) 'face) 'bold)
                   (emacspeak-auditory-icon 'item)
                 (emacspeak-auditory-icon 'select-object))))))

;;}}}
;;{{{ advice interaction:

(defadvice sudoku (after emacspeak pre act comp)
  "Speech-enable SuDoKu.
See
  http://emacspeak.blogspot.com/2006/02/playing-sudoku-using-auditory-feedback.html
  for details."
  (when (ems-interactive-p)
    (dtk-set-punctuations 'some)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-sudoku-speak-current-cell-value)))
(defvar emacspeak-sudoku-history-stack nil
  "Holds history of interesting board configurations.")

(defadvice sudoku-new (after emacspeak pre act comp)
  "Reset history stack."
  (cl-declare (special emacspeak-sudoku-history-stack))
  (setq emacspeak-sudoku-history-stack nil))

(defadvice sudoku-restart (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-sudoku-speak-current-cell-value)))

;;}}}
;;{{{ implement history stack:

(make-variable-buffer-local 'emacspeak-sudoku-history-stack)

(defun emacspeak-sudoku-history-push ()
  "Push current state on to history stack."
  (interactive)
  (cl-declare (special emacspeak-sudoku-history-stack
                       current-board))
  (push current-board emacspeak-sudoku-history-stack)
  (emacspeak-auditory-icon 'mark-object)
  (message "Saved state on history stack."))

(defun emacspeak-sudoku-history-pop ()
  "Pop saved state off stack and redraw board."
  (interactive)
  (cl-declare (special emacspeak-sudoku-history-stack
                       sudoku-onscreen-instructions
                       start-board
                       current-board))
  (let ((original (sudoku-get-cell-from-point (point))))
    (cond
     ((null emacspeak-sudoku-history-stack) ;start board
      (setq current-board start-board))
     (t (setq current-board
              (pop emacspeak-sudoku-history-stack))))
    (setq buffer-read-only nil)
    (erase-buffer)
    (sudoku-board-print current-board sudoku-onscreen-instructions)
    (sudoku-goto-cell original)
    (setq buffer-read-only t)
    (emacspeak-auditory-icon 'yank-object)
    (message "Reset board from history  %s squares remain."
             (sudoku-remaining-cells current-board))))

;;}}}
;;{{{ setup keymap:

(when (and (boundp 'sudoku-mode-map) (keymapp sudoku-mode-map))
  (cl-loop for k in
           '(
             ("u" emacspeak-sudoku-up-sub-square)
             ("d" emacspeak-sudoku-down-sub-square)
             ("/" emacspeak-sudoku-how-many-remaining)
             ("n" emacspeak-sudoku-next-sub-square)
             ("p" emacspeak-sudoku-previous-sub-square)
             ("h" sudoku-move-point-left)
             ("l" sudoku-move-point-right)
             ("j" sudoku-move-point-down)
             ("k" sudoku-move-point-up)
             ("R" emacspeak-sudoku-speak-remaining-in-row)
             ("S" emacspeak-sudoku-speak-remaining-in-sub-square)
             ("C" emacspeak-sudoku-speak-remaining-in-column)
             ("?" emacspeak-sudoku-hint)
             ("<home>" sudoku-move-point-leftmost)
             ("<end>" sudoku-move-point-rightmost)
             ("a" sudoku-move-point-leftmost)
             ("e" sudoku-move-point-rightmost)
             ("b" sudoku-move-point-downmost)
             ("t" sudoku-move-point-upmost)
             ("." emacspeak-sudoku-speak-current-cell-value)
             ("=" emacspeak-sudoku-speak-current-cell-coordinates)
             ("\C-e" emacspeak-prefix-command)
             ("r" emacspeak-sudoku-speak-current-row)
             ("c" emacspeak-sudoku-speak-current-column)
             ("s" emacspeak-sudoku-speak-current-sub-square)
             ("\M-s" emacspeak-sudoku-erase-current-sub-square)
             ("\M-r" emacspeak-sudoku-erase-current-row)
             ("\M-c" emacspeak-sudoku-erase-current-column)
             (","  emacspeak-sudoku-board-summarizer)
             ("m" emacspeak-sudoku-history-push)
             ("M" emacspeak-sudoku-history-pop)
             )
           do
           (define-key  sudoku-mode-map (cl-first k) (cl-second k))))

;;}}}
(provide 'emacspeak-sudoku)
;;{{{ end of file 

;; local variables:
;; folded-file: t
;; end: 

;;}}}

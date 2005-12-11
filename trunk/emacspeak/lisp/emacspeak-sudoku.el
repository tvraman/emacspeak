;;; emacspeak-sudoku.el --- Play SuDoku 
;;; $Id$
;;; $Author$
;;; Description: Playing SuDoku ;;; Keywords: Emacspeak, sudoku
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

;;{{{ Introduction:

;;; Commentary:

;;; Playing SuDoku using speech output.
;;; Written to discover what type of feedback one needs for  this
;;; task

;;}}}
;;{{{ Define additional speak commands:

(defun emacspeak-sudoku-speak-current-cell-coordinates ()

  "speak current cell coordinates."
  (interactive)
  (let ((row (second (sudoku-get-cell-from-point (point))))
        (column (first (sudoku-get-cell-from-point (point)))))
  (message
   (format "Row %s Column %s"
   row column))))

(defun emacspeak-sudoku-speak-current-row ()
  "Speak current row."
  (interactive)
  (let ((cell (sudoku-get-cell-from-point (point))))
    (dtk-speak-list (sudoku-row current-board
                           (second cell )))))

(defun emacspeak-sudoku-speak-current-column ()
  "Speak current column."
  (interactive)
  (let ((cell (sudoku-get-cell-from-point (point))))
    (dtk-speak-list (sudoku-column  current-board
                           (first cell )))))

(defsubst emacspeak-sudoku-cell-sub-square (cell)
  "Return sub-square that this cell is in."
  (let ((row (second cell))
        (column (first cell)))
          (+ ( * 3 (/ row 3))
             (/ column 3))))

(defun emacspeak-sudoku-speak-current-sub-square ()
  "Speak current sub-square."
  (interactive)
  (let ((cell (sudoku-get-cell-from-point (point))))
    (dtk-speak-list
     (sudoku-subsquare  current-board
                           (emacspeak-sudoku-cell-sub-square cell)))))

(defun emacspeak-sudoku-speak-current-cell-value ()
  "Speak value in current cell."
  (interactive)
  (let ((cell (sudoku-get-cell-from-point (point))))
    (dtk-speak
     (sudoku-cell current-board (first cell) (second cell)))))

(defun emacspeak-sudoku-hint ()
  "Provide hint for current cell."
  (interactive)
  (let* ((cell (sudoku-get-cell-from-point (point)))
        (possibles (sudoku-cell-possibles
    current-board
    (first cell)
    (second cell))))
    (cond
     (possibles 
  (dtk-speak-list possibles))
     (t (message "Dead End")))))

;;}}}
;;{{{ advice motion:

(loop for f   in
      '(
sudoku-move-point-left 
sudoku-move-point-leftmost 
sudoku-move-point-right 
sudoku-move-point-rightmost 
sudoku-move-point-up 
sudoku-move-point-upmost 
sudoku-move-point-down 
sudoku-move-point-downmost )
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Produce auditory output."
          (when (interactive-p)
            (emacspeak-sudoku-speak-current-cell-value)
            (emacspeak-auditory-icon 'select-object)))))

;;}}}
;;{{{ setup keymap:

(declaim (special sudoku-mode-map))
(loop for k in
      '(
        ("?" emacspeak-sudoku-hint)
        ([home] sudoku-move-point-leftmost)
        ([end] sudoku-move-point-rightmost)
        ("." emacspeak-sudoku-speak-current-cell-value )
        ("=" emacspeak-sudoku-speak-current-cell-coordinates)
("\C-e" emacspeak-prefix-command)
      ("r" emacspeak-sudoku-speak-current-row)
      ("c" emacspeak-sudoku-speak-current-column)
      ("s" emacspeak-sudoku-speak-current-sub-square)
)
      do
      (define-key  sudoku-mode-map (first k) (second k)))

;;}}}
;;{{{  Required modules

(require 'emacspeak-preamble)
(require 'desktop)
(require 'dired)
;;}}}

(provide 'emacspeak-sudoku)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

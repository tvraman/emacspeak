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
;;{{{  Required modules

(require 'emacspeak-preamble)
(require 'desktop)
(require 'dired)
;;}}}
;;{{{ Define additional speak commands:
(defun emacspeak-sudoku-board-summarize ()
  "Shows distribution of filled numbers."
  (interactive)
  (declare (special current-board))
  (let ((msg "")
        (counts (make-vector 10 0))
        (values (loop for i in current-board
                      nconc
                      (loop for j in i
                            collect j))))
    (loop for v in values
          do
          (incf (aref counts v)))
    (loop for i from 1 to 9
          do
          (setq msg
                (concat msg
                        (format " %s "
                                 (aref counts i)))))
    (message msg)))
  
   
    
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
  (declare (special current-board))
  (let ((cell (sudoku-get-cell-from-point (point))))
    (dtk-speak-list (sudoku-row current-board
				(second cell )))))

(defun emacspeak-sudoku-speak-current-column ()
  "Speak current column."
  (interactive)
  (declare (special current-board))
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
  (declare (special current-board))
  (let ((cell (sudoku-get-cell-from-point (point))))
    (dtk-speak-list
     (sudoku-subsquare  current-board
			(emacspeak-sudoku-cell-sub-square cell)))))

(defun emacspeak-sudoku-speak-current-cell-value ()
  "Speak value in current cell."
  (interactive)
  (declare (special current-board))
  (let ((cell (sudoku-get-cell-from-point (point))))
    (dtk-speak
     (sudoku-cell current-board (first cell) (second cell)))))

(defun emacspeak-sudoku-hint ()
  "Provide hint for current cell."
  (interactive)
  (declare (special current-board))
  (let* ((cell (sudoku-get-cell-from-point (point)))
	 (possibles (sudoku-cell-possibles
		     current-board
		     (first cell)
		     (second cell))))
    (cond
     (possibles 
      (dtk-speak-list possibles))
     (t (message "Dead End")))))

(defun emacspeak-sudoku-speak-remaining-in-row ()
  "Speaks number of remaining cells in current row."
  (interactive)
  (declare (special current-board))
  (let ((cell (sudoku-get-cell-from-point (point))))
    (dtk-speak
     (count 0
            (sudoku-row current-board (second cell))))))

(defun emacspeak-sudoku-speak-remaining-in-column ()
  "Speaks number of remaining cells in current column."
  (interactive)
  (declare (special current-board))
  (let ((cell (sudoku-get-cell-from-point (point))))
    (dtk-speak
     (count 0
            (sudoku-column current-board  (first cell))))))

(defun emacspeak-sudoku-speak-remaining-in-sub-square ()
  "Speaks number of remaining cells in current sub-square."
  (interactive)
  (declare (special current-board))
  (let ((cell (sudoku-get-cell-from-point (point))))
    (dtk-speak
     (count 0
            (sudoku-subsquare current-board
			      (emacspeak-sudoku-cell-sub-square cell))))))
(defun emacspeak-sudoku-how-many-remaining ()
  "Speak number of remaining squares to fill."
  (interactive)
  (declare (special current-board))
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
  (declare (special current-board))
  (let ((original (sudoku-get-cell-from-point (point))))
    (loop for cell in cell-list
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
  (declare (special current-board))
  (let ((cell (sudoku-get-cell-from-point (point))))
    (emacspeak-sudoku-erase-these-cells
     (loop for i from 0 to  8
           collect  (list i (second cell)))))
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)))

(defun emacspeak-sudoku-erase-current-column ()
  "Erase current column."
  (interactive)
  (declare (special current-board))
  (let ((cell (sudoku-get-cell-from-point (point))))
    (emacspeak-sudoku-erase-these-cells
     (loop for i from 0 to  8
           collect  (list (first cell) i))))
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)))

(defsubst emacspeak-sudoku-sub-square-cells (square)
  "Return list of cells in sub-square."
  (let ((row-start (* (/ square 3)  3)) 
        (col-start (* (% square 3)  3)))
        
    (loop for r from row-start to (+ 2 row-start)
          nconc
          (loop  for c from col-start to (+ 2 col-start)
                 collect (list c r )))))

(defun emacspeak-sudoku-erase-current-sub-square ()
  "Erase current sub-square."
  (interactive)
  (let* ((square
          (emacspeak-sudoku-cell-sub-square
           (sudoku-get-cell-from-point (point))))
         (square-cells (emacspeak-sudoku-sub-square-cells square)))
    (emacspeak-sudoku-erase-these-cells square-cells))
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)))
     

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
            (if (eq (get-text-property  (point) 'face) 'bold)
		(emacspeak-auditory-icon 'item)
	      (emacspeak-auditory-icon 'select-object))))))

;;}}}
;;{{{ advice interaction:

(defadvice sudoku (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-sudoku-speak-current-cell-value)))

(defadvice sudoku-restart (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-sudoku-speak-current-cell-value)))

;;}}}
;;{{{ setup keymap:

(declaim (special sudoku-mode-map))
(loop for k in
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
        ([home] sudoku-move-point-leftmost)
        ([end] sudoku-move-point-rightmost)
        ("a" sudoku-move-point-leftmost)
        ("e" sudoku-move-point-rightmost)
        ("b" sudoku-move-point-downmost)
        ("t" sudoku-move-point-upmost)
        ("." emacspeak-sudoku-speak-current-cell-value )
        ("=" emacspeak-sudoku-speak-current-cell-coordinates)
	("\C-e" emacspeak-prefix-command)
	("r" emacspeak-sudoku-speak-current-row)
	("c" emacspeak-sudoku-speak-current-column)
	("s" emacspeak-sudoku-speak-current-sub-square)
        ("\M-s" emacspeak-sudoku-erase-current-sub-square)
        ("\M-r" emacspeak-sudoku-erase-current-row)
        ("\M-c" emacspeak-sudoku-erase-current-column)
	)
      do
      (define-key  sudoku-mode-map (first k) (second k)))

;;}}}
(provide 'emacspeak-sudoku)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

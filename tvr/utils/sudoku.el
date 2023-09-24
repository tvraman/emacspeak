;;; sudoku.el -- Simple sudoku game, can download puzzles from the web.

;; Filename: sudoku.el
;; Copyright (C) 2005 Jesse Rosenthal
;; Author: Jesse Rosenthal <jesse.k.rosenthal@gmail.com>
;; Maintainer: Jesse Rosenthal <jesse.k.rosenthal@gmail.com>
;; Created: 29 Oct 2005
;; Description: Uses either local or downloaded sudoku for a simple puzzle game
;; Version 0.2

;; Latest version always available from:
;; www.columbia.edu/~jr2075/elisp/sudoku.el

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; `sudoku-mode' is a major mode for solving sudoku puzzles. The rules
;; are simple: only one of each number in each of the nine rows,
;; columns, and subsquares. It has a few nifty features, the most
;; notable being that you can download puzzles on the fly from
;; websudoku.com (there are parsing functions to take care of the
;; html). The 200 included puzzles were generated using the
;; python-based generator from Thomas Hinkle's gnome-sudoku
;; (http://gnome-sudoku.sourceforge.net).
;;
;; I've added customization options, You can also now customize via
;; the dropdown menu. If you really want to write in to your .emacs
;; file, though, the three variables are `sudoku-level'
;; ({"easy"|"medium"|"hard"|"evil"}, `sudoku-download' (boolean), and
;; `sudoku-download-method'
;; ({"lynx"|"wget"|"w3"|"native-url-lib"}). But you can do all this
;; interactively. The only thing you need to add is:
;;
;; (require 'sudoku)
;;
;; UPDATE: Downloading files no longer requires lynx! This now offers
;; four options: lynx, wget, the native url library in emacs >=22, and
;; w3 (which seems to be largely obsolete). This is set through the
;; `sudoku-download-method' variable, which is also available through
;; the configuration options. The default is to use "native-url-lib"
;; if gnu emacs is above version 22, and lynx otherwise. If anyone has
;; any suggestions for why another option should be the default,
;; please let me know.
;;
;;
;; The defaults are for `sudoku-level' to be easy and
;; `sudoku-download' to be nil. But there are only about fifty puzzles
;; of each level included, so the chances of you repeating one are
;; pretty good. You're probably better off setting download on, if
;; you're online.
;; 
;;; ChangeLog:
;; v0.1 - 29 0ct 2005 
;;    * First version 
;;
;; v0.1.1 - 30 Oct 2005 - 
;;    * Added erase function.  
;;    * Changed the (if (a) b) statements to (when (a) b). Thanks to
;;      Thomas Gehrlein for pointing this convention out to me.
;;    * Added some new directions (leftmost, downmost, rightmost,
;;      upmost) and associated keybinding.
;;    * Added credit to Thomas Hinkle for puzzle generator.
;;    * Changed (undo-copy-list) to (copy-tree)
;;    * Changed permanent online location to 
;;      www.columbia.edu/~jr2075/sudoku.el
;;
;; v0.1.2 - 4 Nov 2005 -
;;    * Added radio buttons to menu.
;;    * Added hint function, bound to "/C-c/C-h"
;;    * Added Emacs Customization code
;;
;; v0.1.3 - 15 Nov 2005 -
;;    * Fixed an annoying issue where the
;;     compatibility-with-old-versions code called the sudoku-level
;;     variable too early, if it wasn't set in the .emacs file. Now it
;;     should work fine with or without .emacs customization. (Thanks
;;     to Andrew Scott for pointing this out to me.)
;;
;; v0.2 - 18 Nov 2005 - 
;;   * Downloading files no longer requires lynx!  There are now four
;;     options: the native url library (in emacs >= 22), w3, lynx, and
;;     wget. The first two are pure elisp, so they should be platform
;;     independent. Thanks to Wojciech Komornicki for getting me
;;     started on this.
;;   * Added a save-options entry to the drop-down menu. You really
;;     don't need to (and maybe shouldn't) customize by hand anymore. 
;;     Either use the dropdown menu, or customiz-group <RET> sudoku.
;;
;;
;;; TODO:
;;  - Add a solver
;;  - Add more information about current settings onscreen
;;  - Add color?
;;  - Clean up what is no doubt quite brutal amateur code.

;;; Code:

(require 'cl)
(require 'easymenu)

;; This has some compatibility things built in, like propertize...
(when (featurep 'xemacs)
  (require 'easy-mmode))

(defgroup sudoku nil
  "Sudoku - web-enabled puzzle game"
  :group  'games
  :prefix "sudoku-")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom sudoku-level "easy"
  "*Level of difficulty for sudoku."
  :group 'sudoku
  :type '(radio (const "easy")
		(const "medium")
		(const "hard")
		(const "evil")))

;; This is just a compatibility thing from the old setup, when the
;; difficulty level was a symbol instead of a string
(when (symbolp sudoku-level)
  (setq sudoku-level (symbol-name sudoku-level)))

(defcustom sudoku-download nil
  "*Should sudoku download puzzles from the web?"
  :type  'boolean
  :group 'sudoku)

(defcustom sudoku-download-method "lynx"
  "*Method for downloading new puzzles."
  :group 'sudoku
  :type '(radio (const "native-url-lib")
		(const "w3")
		(const "lynx")
		(const "wget")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non-customizable variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sudoku-mode 'nil)
(make-variable-buffer-local 'sudoku-mode)

(defvar sudoku-mode-hooks 'nil)
(make-variable-buffer-local 'sudoku-mode-hooks)

(defconst blank-cell "_")

(defconst sudoku-buffer-name "*sudoku*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic mode functions 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sudoku ()
  "Used to start a new game and change to sudoku-mode"
  (interactive)
  (sudoku-new)
  (sudoku-mode))

(defun sudoku-mode ()
  "A mode for playing `sudoku' The key bindings for sudoku-mode
are: \\{sudoku-mode-map}"
  (interactive)
  ;(sudoku-new)
  (kill-all-local-variables)
  (use-local-map sudoku-mode-map)
  (setq major-mode 'sudoku-mode
        mode-name  "sudoku")
  ;(run-mode-hooks 'sudoku-mode-hook)
  (setq buffer-read-only t)
  (buffer-disable-undo))

(defvar sudoku-mode-hook nil)

(defvar sudoku-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-n" 'sudoku-new)
    (define-key map "\C-c\C-c" 'sudoku-quit)
    (define-key map "\C-c\C-r" 'sudoku-restart)
    
    (define-key map [up] 'sudoku-move-point-up)
    (define-key map "\C-p" 'sudoku-move-point-up)
    (define-key map "k" 'sudoku-move-point-up)
    (define-key map [down] 'sudoku-move-point-down)
    (define-key map "\C-n" 'sudoku-move-point-down)
    (define-key map "j" 'sudoku-move-point-down)
    (define-key map [return] 'sudoku-move-point-down)
    (define-key map [left] 'sudoku-move-point-left)
    (define-key map "\C-b" 'sudoku-move-point-left)
    (define-key map "h" 'sudoku-move-point-left)
    (define-key map [right] 'sudoku-move-point-right)
    (define-key map "\C-f" 'sudoku-move-point-right)
    (define-key map "l" 'sudoku-move-point-right)
    (define-key map [tab] 'sudoku-move-point-right)

    (define-key map "\C-a" 'sudoku-move-point-leftmost)
    (define-key map "\C-e" 'sudoku-move-point-rightmost)
    (define-key map [prior] 'sudoku-move-point-upmost)
    (define-key map [next] 'sudoku-move-point-downmost)
    (define-key map [home] '(lambda () (interactive) 
			     (sudoku-move-point-upmost) (sudoku-move-point-leftmost)))
    (define-key map [end] '(lambda () (interactive) 
			     (sudoku-move-point-downmost) (sudoku-move-point-rightmost)))

    (define-key map "\C-d" 'sudoku-cell-erase)
    (define-key map "_" 'sudoku-cell-erase)
    (define-key map " " 'sudoku-cell-erase)
    (define-key map "0" 'sudoku-cell-erase)
    (define-key map [backspace] 'sudoku-cell-erase)

    (define-key map "\C-c\C-h" 'sudoku-hint)
    
    ;; Disabled in sudoku mode
    
    (define-key map "\C-v" 'sudoku-disabled-key)
    (define-key map "\M-v" 'sudoku-disabled-key)
    (define-key map [mouse-1] 'sudoku-disabled-key)
    (define-key map [down-mouse-1] 'sudoku-disabled-key)
    (define-key map [drag-mouse-1] 'sudoku-disabled-key)
    (define-key map [double-mouse-1] 'sudoku-disabled-key)
    
    (define-key map "\C-k" 'sudoku-disabled-key)
    (define-key map "\C-xh" 'sudoku-disabled-key)
    (define-key map "\M-h" 'sudoku-disabled-key)
    (define-key map "\M-<" 'sudoku-disabled-key)
    (define-key map "\M->" 'sudoku-disabled-key)

    ;;I want to figure out how to make it only go to valid cells, but
    ;;for the time being...

    (define-key map "1" '(lambda () (interactive) (sudoku-change-point 1)))
    (define-key map "2" '(lambda () (interactive) (sudoku-change-point 2)))
    (define-key map "3" '(lambda () (interactive) (sudoku-change-point 3)))
    (define-key map "4" '(lambda () (interactive) (sudoku-change-point 4)))
    (define-key map "5" '(lambda () (interactive) (sudoku-change-point 5)))
    (define-key map "6" '(lambda () (interactive) (sudoku-change-point 6)))
    (define-key map "7" '(lambda () (interactive) (sudoku-change-point 7)))
    (define-key map "8" '(lambda () (interactive) (sudoku-change-point 8)))
    (define-key map "9" '(lambda () (interactive) (sudoku-change-point 9)))
    map)
  "Keymap for sudoku mode")

(easy-menu-add-item nil '("tools" "games") ["Sudoku" sudoku t])

(easy-menu-define sudoku-mode-menu sudoku-mode-map "sudoku menu."
  '("Sudoku"
    ["New game"               sudoku-new t]
    ["Reset game"            sudoku-restart t]
    ["Quit game"              sudoku-quit t]
    "---"
    ("Set level"
     ["Easy"  (setq sudoku-level "easy") 
      :style radio :selected (string= sudoku-level "easy")]

     ["Medium" (setq sudoku-level "medium") 
      :style radio :selected (string= sudoku-level "medium")]
     ["Hard"  (setq sudoku-level "hard") 
      :style radio :selected (string= sudoku-level "hard")]
     ["Evil" (setq sudoku-level "evil") 
      :style radio :selected (string= sudoku-level "evil")])
    ("Download"
     ["On" (setq sudoku-download t) 
      :style radio :selected sudoku-download]
     ["Off" (setq sudoku-download nil)
      :style radio :selected (null sudoku-download)])
    ("Download Method"
     :active sudoku-download
     ["lynx"  (setq sudoku-download-method "lynx") 
      :style radio :selected (string= sudoku-download-method "lynx")]
     ["w3"  (setq sudoku-download-method "w3") 
      :style radio :selected (string= sudoku-download-method "w3")]
     ["Native Url Library (cvs only)"  (setq sudoku-download-method "native-url-lib") 
      :style radio :selected (string= sudoku-download-method "native-url-lib")]
     ["wget"  (setq sudoku-download-method "wget") 
      :style radio :selected (string= sudoku-download-method "wget")])
    "---"
    ["Save Options" 
     (mapcar #'(lambda (var) (eval `(customize-save-variable (quote ,var) ,var)))
	     '(sudoku-level
	       sudoku-download
	       sudoku-download-method))]))

(defun sudoku-new ()
  "Sets the \"current-board\" variable, using the
  \"sudoku-current-board\" function, and then runs
  \"sudoku-initialize\", which does the rest."
  (interactive)
  (setq current-board (sudoku-current-board sudoku-level sudoku-download))
  (sudoku-initialize))

(defun sudoku-initialize ()
  "Makes the board, based on the \"current board\" variable, and
  sets the buffer for read-only. Used by \"sudoku-new\"."
  (switch-to-buffer (get-buffer-create sudoku-buffer-name))
  (when buffer-read-only
      (setq buffer-read-only nil))
  (erase-buffer)
  ;;(sudoku-mode)
  (setq start-board current-board)
  (sudoku-board-print current-board sudoku-onscreen-instructions)
  (setq cell-point-list (sudoku-get-cell-points))
  (sudoku-goto-cell '(0 0))
  (when (null buffer-read-only)
      (setq buffer-read-only t)))

(defun sudoku-current-board (level &optional download)
  "Checks both the \"sudoku-download\" variable, and the
  \"sudoku-level\" variable. Uses these to either choose a random
  included board (if download is nil) or to download one from
  websudoku.com"
  (cond (download
	 (cond ((string= level 'easy)
		(setq current-board (sudoku-download-new-puzzle 1)))
	       ((string= level 'medium)
		(setq current-board (sudoku-download-new-puzzle 2)))
	       ((string= level 'hard)
		(setq current-board (sudoku-download-new-puzzle 3)))
	       ((string= level 'evil)
		(setq current-board (sudoku-download-new-puzzle 4)))))
	 (t
	  (let ((n (mod (random t) 50)))
	    ;; This is about as close to a good random number as we
	    ;; can get in emacs
	    (cond ((string= level 'easy)
		   (setq current-board (nth n easy-puzzles)))
		  ((string= level 'medium)
		   (setq current-board (nth n medium-puzzles)))
		  ((string= level 'hard)
		   (setq current-board (nth n hard-puzzles)))
		  ((string= level 'evil)
		   (setq current-board (nth n evil-puzzles))))))))

(defun sudoku-quit-immediately ()
  "Quit without a prompt. Designed to be used by other functions."
  (kill-buffer sudoku-buffer-name))

(defun sudoku-quit ()
  "Quit with confirmation."
  (interactive)
  (when (y-or-n-p "Are you sure you want to quit sudoku? ")
      (sudoku-quit-immediately)))

(defun sudoku-restart ()
  "Return the board to its original state."
  (interactive)
  (if (null start-board)
      (message "You have to start before you can restart.")
    (progn 
      (setq current-board start-board)
      (sudoku-initialize))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic sudoku functions (can also be used for a solver)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sudoku-row (board n)
  "Returns the nth row of a board."
  (nth n board))

(defun sudoku-column (board n)
  "Returns the nth column of a board"
  (let ((column nil))
    (dolist (row board)
      (setq column (cons (nth n row) column)))
    (reverse column)))

(defun sudoku-subsquare (board n)
  "Returns the nth subsquare of a board (as a flat list)"
  (let ((rows (list (sudoku-row board (* 3 (/ n 3)))
		    (sudoku-row board (+ (* 3 (/ n 3)) 1))
		    (sudoku-row board (+ (* 3 (/ n 3)) 2))))
	(col-start-num (* (mod n 3) 3))
	(subsquare nil)
	(subsquare-flat nil))
    (dolist (row rows)
      (setq subsquare 
	    (cons 
	     (butlast (nthcdr col-start-num row) (- 6 col-start-num)) 
	     subsquare)))
    (dolist (row subsquare)
      (dolist (elt (reverse row))
	(setq subsquare-flat (cons elt subsquare-flat))))
    subsquare-flat))

(defun sudoku-cell (board x y)
  "Returns the (x,y) cell of a board"
  (nth x (sudoku-row board y)))

(defun sudoku-cell-elts (board x y)
  "Returns the row, column, and subsquare containing cell (x,y)"
  (let ((subsquare-num 
	 (+ (* (/ y 3) 3) (/ x 3))))
    (list (sudoku-row board y)
	  (sudoku-column board x)
	  (sudoku-subsquare board subsquare-num))))

(defun sudoku-cell-elts-flat (board x y)
  "Returns the row, column and subsquare containing cell (x,y) in
a flat list"
  (let ((result nil))
    (dolist (elt (sudoku-cell-elts board x y))
      (dolist (atom elt)
	(setq result (cons atom result))))
    result))

(defun sudoku-cell-possibles (board x y)
  "Returns all the possible values for a cell (i.e., those not
already in the row, column, and subsquare containing it."
  (let ((possibilities nil))
    (if (/= (sudoku-cell start-board x y) 0)
	(setq possibilities (cons (sudoku-cell board x y) possibilities))
      (progn
	(dotimes (i 9)
	  (let ((n (+ i 1)))
	    (when (not (member n (remove 0 (sudoku-cell-elts-flat board x y))))
		(setq possibilities (cons n possibilities))))))
    possibilities)))

(defun sudoku-cell-valid (board x y input)
  "Tests to see if a cell's input is valid."
  (if (member input (sudoku-cell-possibles board x y))
      t
    nil))

	   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for displaying the board on-screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sudoku-onscreen-instructions
  "
Commands:

Arrow Keys:\t\tMove around the board
1-9:\t\t\tEnter a numerical value into the current cell
\[SPC\]:\t\t\tRemove the numerical value from the current cell
C-c C-h:\t\tHint (possible values)
C-c C-c:\t\tQuit the game
C-c C-n:\t\tStart a new puzzle
C-c C-r:\t\tReset the puzzle
")

(defun sudoku-row-output (row row-num separator)
  "This takes care of most of the outputting work. It makes a
  string of with a separator, and then three numbers, another
  separator, and so on. It also replaces all zeros with the
  `blank-cell' constant. So, if separator and black-cell are
  \"|\" and \"_\", we would get:
  
  (1 2 0 3 4 0 5 6 0) -> | 1 2 _ | 3 4 _ | 5 6 _ |"

  (let ((output-string nil))
    (setq output-string separator)
    (dotimes (i 3)
      (dotimes (j 3)
	(let ((value (nth (+ (* 3 i) j) row)))
	  (cond ((= value 0)
		 ;; If it's equal to 0, we use the blank-cell
		 ;; character.
		 (setq output-string (concat output-string 
					     (format " %s " blank-cell))))
		((/= (sudoku-cell start-board (+ (* 3 i) j) row-num) 0)
		 ;; If it's one of the original numbers, we bold it.
		 (let ((string (propertize (int-to-string value) 'face 'bold)))
		   (setq output-string (concat output-string 
					    (format " %s " string )))))
		(t
		 ;; If it's any other number, we just input.
		 (setq output-string (concat output-string 
					(format " %s " value )))))))
      (setq output-string (concat output-string separator)))
    output-string))

(defun sudoku-board-output (board)
  "Outputs the visible board. Uses sudoku-row-output to do most
of the work."
  (let ((corner "+")
	(horiz "---")
	(vert "|")
	(top-piece nil)
	(output-string nil))
    (dotimes (i 3)
      (setq top-piece (concat top-piece corner))
      (dotimes (j 3)
	(setq top-piece (concat top-piece horiz))))
    (setq top-piece (concat top-piece corner))
    (setq output-string (concat top-piece "\n"))
    (dotimes (i 3)
      (dotimes (j 3)
	(let ((row-num (+ (* 3 i) j)))
	(setq output-string (concat output-string
				    (sudoku-row-output 
				     (sudoku-row board row-num) row-num vert)
				    "\n"))))
      (setq output-string (concat output-string top-piece "\n")))
    output-string))

(defun sudoku-board-print (board message)
  "Prints the board and the message beneath the board
  together. Usually the message will be the moves. The only other
  message right now is the \"You Win!\" message."
  (save-excursion
    (goto-char (point-min))
    (insert (sudoku-board-output board))
    (insert message)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions relating to changing cells
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sudoku-change-cell (board x y input)
  "Changes a specific cell"
  (let ((newboard (copy-tree board)))
    ((lambda (lst index val) 
       (setcar (last lst (- (length lst) index)) val))
     (sudoku-row newboard y) x input)
    newboard))

(defun sudoku-test-and-change (board x y input)
  "Tests whether a change is valid. If it is, enters the cell and
redraws the board."
  (cond ((member input (sudoku-cell-possibles board x y))
	 (setq old-board board)
	 (setq current-board (sudoku-change-cell board x y input))
	 (erase-buffer)
	 (sudoku-board-print current-board sudoku-onscreen-instructions))
	(t
	 (if (/= (sudoku-cell start-board x y) 0)
	     (message "Original value. Can't change.")
	   (message "Not a valid move")))))

(defun sudoku-get-cell-points ()
 "This reads a printed board and returns the point of each
 number, counting from 0. So, for a 9x9 board, we should get 81
 pairs. We can then use this to turn each number into a cell
 [e.g. 0 -> (0 0), 1 -> (1 0), 9 -> (0 1)] so we can match up
 each cell with a point on the screen. This function is used once
 at initialization to make the cell-point-list, and then that is
 consulted, so we don't have to keep running this over and over
 again."
  (save-excursion
  (goto-char (point-min))
  (let ((counter 0)
	(point-list nil))
    (dotimes (i (count-lines (point-min) (point-max)))
      (beginning-of-line)
      (while (not (eolp))
	(cond ((or (looking-at "[0-9]") (looking-at blank-cell))
	       (setq point-list (cons (list (point) counter) point-list))
	       (incf counter)))
	(forward-char 1))
      (next-line 1))
    (reverse point-list))))

(defun sudoku-number-to-cell (num)
  "This takes the numbers from 0 to 80 and turns them into
 coords.\n TODO: Abstract this using (length board) to make this
 not be 9-dependent"
  (let ((x (mod num 9))
	(y (/ num 9)))
    (list x y)))

(defun sudoku-cell-to-number (coords)
  "This turns any cell into a number from 0 to 80."
  (let ((x (car coords))
	(y (car (cdr coords))))
    (+ (* 9 y) x)))
	
(defun sudoku-get-cell-from-point (num)
  "This uses the \"cell-point-list\" made at initialization to
  return a cell for a point on the screen."
  (let ((result nil))
    (dolist (i cell-point-list)
      (when (= num (car i))
	  (setq result (car (cdr i)))))
    (if (null result)
	nil
      (sudoku-number-to-cell result))))

(defun sudoku-get-point-from-cell (coords)
  "Returns a point on the screen for a given cell."
  (let ((result nil))
    (dolist (i cell-point-list)
      (when (= (sudoku-cell-to-number coords) (car (cdr i)))
	  (setq result (car i))))
    result))

(defun sudoku-change-point (input)
  "Changes the value at a point, after running tests to ensure
  that the change is a valid one. Checks to see how many cells
  are remaining. If none are, runs the
  sudoku-completion-routine (i.e., \"You Win!\")."
  (let ((cell (sudoku-get-cell-from-point (point))))
    (save-excursion
      (when buffer-read-only
	  (setq buffer-read-only nil))
      (when (not (null cell))
	  (let* ((cell (sudoku-get-cell-from-point (point)))
		 (x (car cell))
		 (y (car (cdr cell))))
	    (sudoku-test-and-change current-board x y input))))
    (sudoku-goto-cell cell))
  (when (null buffer-read-only)
      (setq buffer-read-only t))
  (let ((remaining (sudoku-remaining-cells current-board)))
    (when (= remaining 0)
	(sudoku-completion-routine)
      ;(message "%d empty cells left" remaining)
      )))

(defun sudoku-cell-erase ()
  (interactive)
  (let* ((cell (sudoku-get-cell-from-point (point)))
	   (x (car cell))
	   (y (car (cdr cell))))
      (if (= (sudoku-cell start-board x y) 0)
	  (setq current-board (sudoku-change-cell current-board x y 0))
	(message "Original value. Can't erase."))
      (setq buffer-read-only nil)
      (erase-buffer)
      (sudoku-board-print current-board sudoku-onscreen-instructions)
      (sudoku-goto-cell cell)
      (setq buffer-read-only t)))
    

(defun sudoku-remaining-cells (board)
  "Tests to see how many cells are remaining"
  (let ((remaining 0))
    (dolist (row board)
      (setq remaining (+ remaining (count 0 row))))
    remaining))
  

(defun sudoku-completion-routine ()
  "Runs when there are no cells remaining. Gives a message of
victory, and then asks if you want to play again."
  (let ((victory-message "\n\nYOU WIN!"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (sudoku-board-print current-board victory-message)
    (setq buffer-read-only t))
  (if (y-or-n-p "Start another puzzle? ")
      (sudoku-new)
    (sudoku-quit-immediately)))

(defun sudoku-hint ()
  (interactive)
  (let* ((cell (sudoku-get-cell-from-point (point)))
	 (cell-x (car cell))
	 (cell-y (car (cdr cell))))
    (cond ((/= (sudoku-cell start-board cell-x cell-y) 0)
	   (message "Original value. No other possibilities."))
	  (t
	   (let ((string nil))
	     (dolist (n (cdr (sudoku-cell-possibles current-board cell-x cell-y)))
	       (setq string (concat (int-to-string n) "," string)))
	     (let ((last (int-to-string (car (sudoku-cell-possibles current-board cell-x cell-y)))))
	     (setq string (concat string last)))
	     (message "Possible values: %s" string))))))

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Motion functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sudoku-goto-cell (coords)
  "Moves to a given pair of coordinates."
  (goto-char (sudoku-get-point-from-cell coords)))

(defun sudoku-move-point (direction)
  "Moves in one of four directions: either left, right, up, or
down. Uses sudoku-goto-cell, but doesn't let you go outside the
bounds of the board."
  (let* ((cell (sudoku-get-cell-from-point (point)))
	       (x (car cell))
	       (y (car (cdr cell))))
    (cond ((string= direction "left")
	   (when (> x 0)
	       (setq x (- x 1))))
	  ((string= direction "leftmost")
	   (setq x 0))
	  ((string= direction "right")
	   (when (< x 8)
	       (setq x (+ x 1))))
	  ((string= direction "rightmost")
	   (setq x 8))
	  ((string= direction "up")
	   (when (> y 0)
	       (setq y (- y 1))))
	  ((string= direction "upmost")
	   (setq y 0))
	  ((string= direction "down")
	   (when (< y 8)
	       (setq y (+ y 1))))
	  ((string= direction "downmost")
	   (setq y 8)))
    (sudoku-goto-cell (list x y))))

(defun sudoku-move-point-left ()
  "Moves the point one cell left."
  (interactive)
  (sudoku-move-point "left"))

(defun sudoku-move-point-leftmost ()
  "Moves the point to the leftmost cell."
  (interactive)
  (sudoku-move-point "leftmost"))

(defun sudoku-move-point-right ()
  "Moves the point one cell right."
  (interactive)
  (sudoku-move-point "right"))

(defun sudoku-move-point-rightmost ()
  "Moves the point to the rightmost cell."
  (interactive)
  (sudoku-move-point "rightmost"))

(defun sudoku-move-point-up ()
  "Moves the point one cell up."
  (interactive)
  (sudoku-move-point "up"))

(defun sudoku-move-point-upmost ()
  "Moves the point to the upmost cell."
  (interactive)
  (sudoku-move-point "upmost"))

(defun sudoku-move-point-down ()
  "Moves the point one cell down."
  (interactive)
  (sudoku-move-point "down"))

(defun sudoku-move-point-downmost ()
  "Moves the point to the downmost cell."
  (interactive)
  (sudoku-move-point "downmost"))

(defun sudoku-disabled-key ()
  (interactive)
  (message "Disabled in Sudoku mode"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;For downloading new puzzles (requires lynx)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sudoku-download-new-puzzle (level)
  "Uses sudoku-get-board and the parsing functions to return a
  new board from the web. The levels can be either \"easy\",
  \"medium\", \"hard\", or \"evil\"."
  (save-excursion
    (let ((new-board nil)
	  (source (concat "http://play.websudoku.com/?level=" (int-to-string level))))
      (cond ((string= sudoku-download-method "native-url-lib")
	     (get-board-native source))
	    ((string= sudoku-download-method "w3")
	     (get-board-w3 source))
	    ((string= sudoku-download-method "lynx")
	     (get-board-lynx source))
	    ((string= sudoku-download-method "wget")
	     (get-board-wget source))))))
	     
(defun sudoku-html-to-list ()
  "Assumes you are in another buffer, into which the websudoku
html has been downloaded. Split out because some of the routines
can use `with-temp-buffer' and others seem to require a
`set-buffer'. Used by the different get-board-* functions."
      (cut-to-table)
      (cut-to-input-entries)
      (goto-char (point-min))
      (dotimes (i 9)
	(let ((row nil))
	  (dotimes (j 9)
	    (beginning-of-line)
	    (setq row (cons (parse-input-entry) row))
	    (next-line 1))
	  (setq new-board (cons (reverse row) new-board))))
      (reverse new-board))

(defun get-board-lynx (source)
  "Downloads a websudoku html file into a temp buffer using lynx
and turns it into a list. Used by sudoku-download-new-puzzle."
  (with-temp-buffer
    (call-process "lynx"
		  nil t nil
		  "--source" source)
    (sudoku-html-to-list)))

(defun get-board-wget (source)
  "Downloads a websudoku html file into a temp buffer using wget
and turns it into a list. Used by sudoku-download-new-puzzle."
  (with-temp-buffer
    (call-process "wget"
		  nil t nil
		  "-q" "-O" "-" source)
    (sudoku-html-to-list)))

(defun get-board-native (source)
  "Downloads a websudoku html file into a temp buffer using the
native emacs url library (emacs >= 22), or downloaded from cvs
and turns it into a list. Used by sudoku-download-new-puzzle."
  (unless (featurep 'url)
    (require 'url))
  (save-excursion
    (set-buffer (url-retrieve-synchronously source))
    (sudoku-html-to-list)))

;; Adapted from code submitted by Wojciech Komornicki. THANKS!
(defun get-board-w3 (source)
  "Downloads a websudoku html file into a temp buffer using the
url retrieval library from w3/emacs (seems to be obsolete now),
and turns it into a list. Used by sudoku-download-new-puzzle."
    (unless (featurep 'w3)
     (require 'w3))
    (with-temp-buffer
      (url-retrieve source)
      (sudoku-html-to-list)))

(defun cut-to-table ()
  "Cuts everything out but the html table containing the puzzle. Used by
sudoku-download-new-puzzle."
  (save-excursion
    (goto-char (point-min))
    (delete-region (point-min)
		   (search-forward "<FORM NAME=\"board\" METHOD=POST ACTION=\"./\">"))
    (kill-line 1)
    (delete-region (search-forward "</TABLE>") (point-max))))

(defun cut-to-input-entries ()
  "Cuts the html table down to \"<INPUT>\" entries which contain
  the cell values. Used by sudoku-download-new-puzzle."
  (save-excursion
    (goto-char (point-min))
    (while (not (looking-at "</TABLE>"))
      (search-forward ">")
      (newline)
      (previous-line 1)
      (beginning-of-line)
      (cond ((looking-at "<INPUT CLASS")
	     (next-line 1))
	    (t
	     (kill-line 1))))
    (kill-line 1)))

(defun parse-input-entry ()
  "Makes a board list out of the input entries. Used by
sudoku-download-new-puzzle."
  (save-excursion
    (let ((value 0))
      (cond ((looking-at ".*VALUE.*")
	     (search-forward "VALUE=\"")
	     (setq value (string-to-int (char-to-string (following-char))))))
      value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Puzzles. About 200.
;;50 each of easy, medium, hard, and evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq evil-puzzles '(
((0 0 0 0 0 6 0 8 0) (0 1 0 0 2 0 6 0 0) (8 0 0 0 0 1 0 0 2) (3 0
0 0 0 0 0 1 7) (0 4 0 3 0 5 0 2 0) (7 6 0 0 0 0 0 0 8) (4 0 0 7 0
0 0 0 5) (0 0 2 0 4 0 0 6 0) (0 3 0 5 0 0 0 0 0))

((0 1 0 0 4 0 3 0 2) (0 0 9 0 3 0 0 0 0) (7 0 0 0 0 0 0 8 0) (0 0 7 0
2 1 0 0 0) (9 0 0 0 8 0 0 0 6) (0 0 0 3 5 0 1 0 0) (0 5 0 0 0 0 0 0 7)
(0 0 0 0 7 0 6 0 0) (8 0 4 0 6 0 0 3 0))

((0 0 0 7 0 0 3 0 0) (0 5 0 0 0 4 2 0 0) (8 0 0 2 0 5 0 0 7) (0 0 4 0
0 0 7 0 8) (3 0 0 0 0 0 0 0 5) (6 0 8 0 0 0 4 0 0) (4 0 0 6 0 7 0 0 1)
(0 0 2 8 0 0 0 7 0) (0 0 1 0 0 2 0 0 0))

((0 0 4 0 0 0 0 5 8) (0 0 0 5 0 0 0 0 0) (5 0 0 8 0 3 9 6 0) (0 0 0 0
7 0 0 2 0) (0 8 2 0 0 0 5 1 0) (0 9 0 0 6 0 0 0 0) (0 5 9 7 0 6 0 0 4)
(0 0 0 0 0 1 0 0 0) (8 2 0 0 0 0 7 0 0))

((5 0 0 0 0 0 0 0 0) (0 0 0 8 0 0 7 0 5) (9 0 0 1 0 0 3 8 4) (0 0 0 9
1 0 0 7 0) (0 0 3 0 0 0 4 0 0) (0 7 0 0 6 2 0 0 0) (3 5 6 0 0 1 0 0 2)
(1 0 2 0 0 6 0 0 0) (0 0 0 0 0 0 0 0 1))

((0 2 3 1 9 0 0 4 0) (0 0 0 4 0 0 0 0 3) (0 0 0 6 0 0 0 1 0) (8 0 0 0
0 0 7 0 0) (0 0 6 0 0 0 1 0 0) (0 0 2 0 0 0 0 0 5) (0 5 0 0 0 8 0 0 0)
(6 0 0 0 0 4 0 0 0) (0 7 0 0 3 5 6 9 0))

((9 0 0 0 0 0 0 0 0) (8 0 0 7 0 0 1 5 2) (0 0 0 1 0 0 0 4 9) (0 0 0 8
7 0 4 0 0) (0 5 0 0 0 0 0 2 0) (0 0 4 0 6 3 0 0 0) (7 3 0 0 0 6 0 0 0)
(5 6 9 0 0 7 0 0 3) (0 0 0 0 0 0 0 0 7))

((4 0 0 0 0 6 0 9 0) (0 0 0 3 0 0 1 0 0) (1 6 0 0 8 0 2 0 0) (0 0 8 0
0 1 0 0 0) (0 2 0 8 0 5 0 4 0) (0 0 0 4 0 0 8 0 0) (0 0 3 0 4 0 0 2 5)
(0 0 7 0 0 9 0 0 0) (0 5 0 6 0 0 0 0 3))

((0 6 0 1 7 0 0 0 0) (0 9 0 0 0 0 7 0 0) (2 0 0 0 6 0 0 0 1) (0 8 0 0
0 0 0 0 0) (7 5 3 0 1 0 6 8 4) (0 0 0 0 0 0 0 9 0) (9 0 0 0 8 0 0 0 3)
(0 0 4 0 0 0 0 2 0) (0 0 0 0 9 6 0 4 0))

((0 8 5 0 6 0 0 0 0) (0 6 0 0 9 2 0 0 1) (9 0 0 8 0 0 0 0 0) (3 0 0 0
0 0 9 1 0) (0 0 8 0 0 0 7 0 0) (0 7 6 0 0 0 0 0 3) (0 0 0 0 0 8 0 0 5)
(5 0 0 2 3 0 0 6 0) (0 0 0 0 7 0 2 3 0))

((0 0 0 0 7 0 5 0 0) (5 0 0 9 0 0 0 3 0) (0 0 8 0 0 4 0 0 9) (0 1 2 0
0 0 7 0 0) (0 7 0 0 2 0 0 1 0) (0 0 5 0 0 0 4 2 0) (8 0 0 6 0 0 1 0 0)
(0 4 0 0 0 2 0 0 3) (0 0 3 0 1 0 0 0 0))

((6 0 0 0 0 0 0 0 0) (0 9 5 8 6 2 0 0 0) (0 3 7 4 0 0 0 0 0) (0 6 0 0
0 0 4 0 3) (0 0 0 7 0 4 0 0 0) (2 0 1 0 0 0 0 8 0) (0 0 0 0 0 7 6 2 0)
(0 0 0 2 8 5 9 7 0) (0 0 0 0 0 0 0 0 1))

((0 0 3 0 8 1 0 0 0) (0 5 6 0 0 9 0 0 0) (2 0 0 6 0 0 0 9 0) (7 0 0 0
0 0 0 3 0) (0 6 8 0 0 0 5 1 0) (0 3 0 0 0 0 0 0 7) (0 2 0 0 0 6 0 0 1)
(0 0 0 8 0 0 7 5 0) (0 0 0 5 7 0 2 0 0))

((0 0 8 0 0 6 0 0 2) (0 9 0 0 0 4 0 0 0) (0 4 0 0 5 0 0 0 0) (0 0 3 0
0 5 0 2 6) (5 0 0 0 2 0 0 0 3) (7 6 0 1 0 0 4 0 0) (0 0 0 0 3 0 0 8 0)
(0 0 0 4 0 0 0 3 0) (8 0 0 5 0 0 6 0 0))

((8 0 0 0 0 0 1 0 0) (9 0 4 0 0 1 0 0 0) (0 6 0 0 0 4 0 0 0) (0 0 0 0
9 5 0 8 0) (6 0 9 0 7 0 2 0 3) (0 2 0 6 4 0 0 0 0) (0 0 0 5 0 0 0 7 0)
(0 0 0 8 0 0 3 0 6) (0 0 1 0 0 0 0 0 2))

((9 2 0 0 5 7 0 0 0) (0 0 7 2 0 0 0 0 0) (4 0 3 0 0 0 0 0 0) (2 0 0 0
0 9 0 1 6) (0 0 4 0 0 0 5 0 0) (7 1 0 6 0 0 0 0 2) (0 0 0 0 0 0 3 0 9)
(0 0 0 0 0 2 7 0 0) (0 0 0 8 4 0 0 5 1))

((2 0 4 0 5 0 0 7 0) (0 0 0 3 0 0 0 2 0) (1 0 0 0 0 4 9 0 0) (0 5 0 0
0 2 0 0 0) (0 0 7 5 0 8 1 0 0) (0 0 0 1 0 0 0 5 0) (0 0 8 4 0 0 0 0 3)
(0 6 0 0 0 9 0 0 0) (0 3 0 0 1 0 7 0 8))

((0 9 0 0 7 0 0 1 0) (0 0 4 2 0 5 0 0 0) (8 0 0 1 0 0 0 0 6) (7 0 0 0
0 4 0 5 0) (0 0 5 0 0 0 4 0 0) (0 4 0 7 0 0 0 0 9) (9 0 0 0 0 1 0 0 7)
(0 0 0 5 0 9 3 0 0) (0 1 0 0 2 0 0 9 0))

((0 4 0 0 8 5 0 9 0) (0 0 0 7 0 0 0 0 5) (0 0 0 0 2 0 0 0 8) (6 0 0 0
0 4 3 0 0) (0 5 3 0 0 0 2 7 0) (0 0 8 6 0 0 0 0 1) (7 0 0 0 6 0 0 0 0)
(1 0 0 0 0 7 0 0 0) (0 3 0 5 4 0 0 2 0))

((1 0 0 4 0 0 0 0 0) (0 5 0 0 7 0 0 0 9) (0 0 7 2 0 0 4 0 0) (9 0 2 0
0 0 3 0 0) (7 0 0 1 0 4 0 0 5) (0 0 1 0 0 0 2 0 6) (0 0 3 0 0 6 5 0 0)
(6 0 0 0 5 0 0 9 0) (0 0 0 0 0 9 0 0 3))

((6 0 0 0 0 4 1 9 0) (0 0 0 0 0 0 0 0 0) (1 4 0 3 8 0 2 0 0) (2 0 0 0
0 0 8 0 0) (9 0 0 4 0 6 0 0 5) (0 0 4 0 0 0 0 0 6) (0 0 1 0 5 8 0 7 2)
(0 0 0 0 0 0 0 0 0) (0 7 5 2 0 0 0 0 3))

((0 0 0 7 0 3 9 6 0) (0 2 0 0 0 0 0 0 0) (3 0 0 0 4 5 0 0 7) (6 0 8 0
0 0 0 3 5) (0 0 0 0 0 0 0 0 0) (2 7 0 0 0 0 6 0 8) (8 0 0 3 2 0 0 0 9)
(0 0 0 0 0 0 0 8 0) (0 6 4 8 0 7 0 0 0))

((0 0 0 0 0 0 9 6 0) (0 0 0 8 3 0 1 0 4) (0 0 0 0 0 7 0 2 0) (1 0 2 5
0 0 7 0 0) (0 3 0 0 0 0 0 4 0) (0 0 7 0 0 9 5 0 1) (0 2 0 7 0 0 0 0 0)
(7 0 9 0 4 2 0 0 0) (0 6 3 0 0 0 0 0 0))

((0 3 0 0 0 5 7 0 0) (8 0 0 3 0 6 0 0 9) (0 0 0 7 0 0 8 0 0) (0 0 9 0
0 0 5 0 6) (4 0 0 0 0 0 0 0 2) (5 0 3 0 0 0 9 0 0) (0 0 2 0 0 3 0 0 0)
(3 0 0 4 0 7 0 0 5) (0 0 7 9 0 0 0 4 0))

((0 0 0 0 3 7 4 5 0) (0 6 0 0 0 0 8 0 0) (0 0 1 0 4 0 0 9 0) (3 5 0 0
0 0 0 2 0) (0 0 0 5 0 1 0 0 0) (0 1 0 0 0 0 0 8 4) (0 9 0 0 7 0 6 0 0)
(0 0 8 0 0 0 0 4 0) (0 4 7 9 8 0 0 0 0))

((0 0 0 0 0 9 3 0 0) (0 0 1 4 3 0 0 7 0) (0 0 0 0 7 0 0 9 2) (3 1 0 0
0 0 5 0 0) (6 0 0 0 0 0 0 0 9) (0 0 5 0 0 0 0 6 7) (4 5 0 0 6 0 0 0 0)
(0 7 0 0 5 4 2 0 0) (0 0 2 9 0 0 0 0 0))

((6 0 3 0 0 0 0 0 0) (0 0 0 0 0 3 4 6 1) (4 0 0 0 0 0 9 0 3) (0 0 8 5
9 0 0 7 0) (0 0 0 0 0 0 0 0 0) (0 4 0 0 2 8 1 0 0) (1 0 7 0 0 0 0 0 5)
(2 3 5 9 0 0 0 0 0) (0 0 0 0 0 0 3 0 8))

((0 0 0 0 0 0 7 3 0) (0 2 0 0 8 0 4 0 0) (6 0 5 0 0 1 0 0 0) (0 4 0 3
1 0 0 0 5) (0 0 3 0 0 0 1 0 0) (9 0 0 0 2 7 0 4 0) (0 0 0 1 0 0 8 0 7)
(0 0 8 0 3 0 0 9 0) (0 5 2 0 0 0 0 0 0))

((0 4 0 0 8 0 0 0 5) (8 0 0 9 4 0 0 0 0) (9 1 0 6 0 0 0 0 0) (0 0 4 0
0 0 1 0 0) (5 3 0 0 0 0 0 6 7) (0 0 8 0 0 0 3 0 0) (0 0 0 0 0 3 0 7 1)
(0 0 0 0 2 8 0 0 9) (6 0 0 0 1 0 0 2 0))

((0 0 0 1 0 0 6 0 0) (0 4 1 0 0 0 0 0 0) (5 7 0 9 0 0 0 3 0) (0 0 0 5
0 6 3 0 9) (9 0 0 0 0 0 0 0 8) (2 0 4 7 0 9 0 0 0) (0 2 0 0 0 1 0 5 6)
(0 0 0 0 0 0 2 7 0) (0 0 9 0 0 5 0 0 0))

((0 0 6 4 0 0 0 1 0) (2 0 0 0 3 9 0 0 0) (4 7 0 0 0 1 0 0 0) (0 0 5 0
0 0 0 2 0) (3 4 0 0 0 0 0 9 7) (0 2 0 0 0 0 5 0 0) (0 0 0 3 0 0 0 7 5)
(0 0 0 7 5 0 0 0 6) (0 6 0 0 0 4 9 0 0))

((0 0 0 0 0 1 7 3 0) (0 0 0 9 6 4 8 2 0) (0 0 0 0 0 0 0 0 6) (7 1 0 0
0 0 6 0 0) (0 0 0 1 0 3 0 0 0) (0 0 4 0 0 0 0 5 9) (5 0 0 0 0 0 0 0 0)
(0 8 3 2 4 9 0 0 0) (0 6 9 3 0 0 0 0 0))

((0 5 0 0 0 1 6 0 0) (0 8 0 0 2 0 0 5 0) (4 0 0 0 0 0 0 0 0) (0 6 1 5
0 0 0 0 9) (0 0 0 1 0 8 0 0 0) (3 0 0 0 0 6 5 7 0) (0 0 0 0 0 0 0 0 7)
(0 3 0 0 4 0 0 8 0) (0 0 9 7 0 0 0 2 0))

((4 0 0 0 0 0 0 0 0) (0 0 9 7 0 6 8 0 0) (0 6 0 3 9 0 0 0 4) (0 4 3 0
8 0 0 0 0) (9 0 0 0 0 0 0 0 1) (0 0 0 0 5 0 7 8 0) (3 0 0 0 1 4 0 5 0)
(0 0 6 5 0 7 1 0 0) (0 0 0 0 0 0 0 0 9))

((7 0 0 0 3 0 0 0 0) (0 0 0 0 8 5 0 6 2) (0 0 2 0 4 0 1 0 0) (0 0 0 0
0 0 0 9 0) (0 2 5 0 9 0 6 4 0) (0 6 0 0 0 0 0 0 0) (0 0 3 0 7 0 9 0 0)
(1 7 0 2 5 0 0 0 0) (0 0 0 0 6 0 0 0 4))

((0 3 0 0 7 1 0 0 0) (8 0 0 0 0 0 0 3 0) (0 0 5 9 0 2 0 0 7) (2 0 0 0
0 0 0 1 0) (0 0 9 0 1 0 7 0 0) (0 8 0 0 0 0 0 0 4) (6 0 0 1 0 5 3 0 0)
(0 5 0 0 0 0 0 0 6) (0 0 0 7 9 0 0 2 0))

((5 0 1 0 7 0 0 0 0) (0 4 0 0 0 8 0 0 0) (7 3 0 4 0 1 0 0 0) (0 0 4 0
0 9 0 2 0) (0 6 0 0 0 0 0 7 0) (0 1 0 3 0 0 4 0 0) (0 0 0 9 0 4 0 8 3)
(0 0 0 8 0 0 0 6 0) (0 0 0 0 6 0 1 0 5))

((0 0 4 0 0 0 8 0 7) (0 3 0 6 0 0 0 0 0) (1 0 0 2 0 0 0 0 0) (0 7 0 4
0 0 0 0 2) (8 2 0 0 7 0 0 9 6) (6 0 0 0 0 2 0 3 0) (0 0 0 0 0 9 0 0 8)
(0 0 0 0 0 5 0 7 0) (5 0 2 0 0 0 1 0 0))

((0 1 0 0 0 6 0 7 0) (0 0 6 0 5 0 1 0 0) (0 0 0 9 0 1 0 0 4) (0 0 2 7
0 0 0 1 0) (9 0 0 0 0 0 0 0 2) (0 7 0 0 0 2 9 0 0) (2 0 0 5 0 9 0 0 0)
(0 0 1 0 7 0 6 0 0) (0 3 0 6 0 0 0 8 0))

((0 0 0 0 0 0 8 0 6) (0 0 0 7 0 4 0 0 0) (0 0 0 0 1 0 0 2 7) (3 0 0 0
5 0 2 0 0) (0 4 0 1 3 8 0 7 0) (0 0 9 0 6 0 0 0 5) (6 9 0 0 7 0 0 0 0)
(0 0 0 9 0 3 0 0 0) (8 0 2 0 0 0 0 0 0))

((0 0 0 1 0 0 9 8 0) (0 0 0 0 0 6 0 0 5) (1 0 0 5 8 0 0 0 0) (5 6 0 7
0 0 0 0 0) (0 0 7 0 1 0 8 0 0) (0 0 0 0 0 4 0 6 7) (0 0 0 0 7 3 0 0 9)
(9 0 0 8 0 0 0 0 0) (0 2 6 0 0 5 0 0 0))

((0 0 0 0 6 1 5 0 0) (5 0 0 0 0 0 7 0 0) (0 6 0 0 9 0 0 4 0) (0 0 0 0
0 0 6 0 0) (4 3 8 0 2 0 9 5 1) (0 0 9 0 0 0 0 0 0) (0 7 0 0 1 0 0 2 0)
(0 0 6 0 0 0 0 0 3) (0 0 1 2 3 0 0 0 0))

((8 0 0 0 5 0 0 0 9) (0 0 3 0 6 0 0 0 0) (0 0 0 0 2 1 8 4 0) (0 0 0 0
0 0 0 7 0) (1 8 0 0 7 0 0 5 4) (0 4 0 0 0 0 0 0 0) (0 3 9 8 1 0 0 0 0)
(0 0 0 0 4 0 5 0 0) (6 0 0 0 3 0 0 0 7))

((0 1 6 0 0 8 0 0 0) (3 0 0 0 0 6 0 0 0) (0 7 0 0 0 0 8 0 0) (0 0 0 0
1 5 0 0 7) (0 3 1 0 4 0 9 2 0) (9 0 0 3 6 0 0 0 0) (0 0 8 0 0 0 0 9 0)
(0 0 0 5 0 0 0 0 4) (0 0 0 7 0 0 2 3 0))

((7 8 2 3 0 0 0 0 0) (3 0 5 0 0 0 0 0 2) (0 0 0 0 0 0 3 0 8) (0 4 0 0
5 6 1 0 0) (0 0 0 0 0 0 0 0 0) (0 0 7 1 9 0 0 2 0) (1 0 3 0 0 0 0 0 0)
(6 0 0 0 0 0 4 0 7) (0 0 0 0 0 5 6 3 9))

((1 0 0 0 0 0 0 0 4) (0 0 0 3 0 0 8 0 0) (4 9 0 0 0 8 0 1 5) (0 0 0 7
0 0 0 4 2) (0 0 0 8 0 2 0 0 0) (2 4 0 0 0 3 0 0 0) (5 8 0 2 0 0 0 9 6)
(0 0 2 0 0 5 0 0 0) (9 0 0 0 0 0 0 0 1))

((0 0 0 0 0 9 4 1 0) (0 5 0 0 0 0 3 0 0) (0 1 2 8 6 0 0 0 0) (0 9 0 0
8 0 7 0 0) (0 6 0 0 0 0 0 9 0) (0 0 1 0 7 0 0 2 0) (0 0 0 0 3 1 8 4 0)
(0 0 5 0 0 0 0 7 0) (0 4 6 7 0 0 0 0 0))

((6 0 4 0 0 0 0 0 0) (5 0 0 3 0 0 0 0 0) (0 3 1 0 2 5 0 0 0) (0 0 3 0
0 1 9 7 0) (4 0 0 0 0 0 0 0 2) (0 7 5 9 0 0 3 0 0) (0 0 0 8 4 0 7 2 0)
(0 0 0 0 0 3 0 0 5) (0 0 0 0 0 0 1 0 6))

((0 0 1 0 0 0 3 6 0) (0 0 8 0 7 1 0 0 0) (0 0 0 4 0 0 0 0 7) (5 7 9 0
0 0 0 0 0) (0 0 2 0 0 0 8 0 0) (0 0 0 0 0 0 6 4 5) (8 0 0 0 0 6 0 0 0)
(0 0 0 3 9 0 7 0 0) (0 9 5 0 0 0 2 0 0))

((6 5 0 0 4 0 1 0 0) (0 0 0 0 3 0 0 4 0) (0 0 8 0 0 0 0 0 3) (0 0 0 1
8 0 0 7 0) (9 0 0 0 6 0 0 0 4) (0 3 0 0 2 7 0 0 0) (3 0 0 0 0 0 6 0 0)
(0 9 0 0 1 0 0 0 0) (0 0 7 0 5 0 0 1 2))

((0 0 0 0 3 0 0 9 4) (0 0 0 0 0 0 8 7 0) (0 0 0 2 0 4 0 0 0) (0 1 0 0
9 0 4 0 0) (3 0 0 7 2 5 0 0 6) (0 0 8 0 1 0 0 2 0) (0 0 0 6 0 3 0 0 0)
(0 9 7 0 0 0 0 0 0) (8 3 0 0 5 0 0 0 0))

((0 6 0 0 0 0 9 0 0) (0 0 0 5 2 9 1 0 0) (0 0 0 0 0 4 0 0 3) (0 0 0 7
0 0 8 9 0) (0 8 0 0 0 0 0 5 0) (0 2 1 0 0 6 0 0 0) (3 0 0 9 0 0 0 0 0)
(0 0 9 1 4 8 0 0 0) (0 0 7 0 0 0 0 8 0))

((0 0 6 0 7 0 5 0 0) (0 0 3 0 0 1 0 4 0) (1 0 0 0 0 0 0 0 0) (0 2 1 9
0 0 0 0 5) (0 0 0 6 0 8 0 0 0) (4 0 0 0 0 2 9 8 0) (0 0 0 0 0 0 0 0 7)
(0 9 0 8 0 0 2 0 0) (0 0 2 0 3 0 6 0 0))

((8 1 0 0 0 0 3 0 0) (0 3 0 0 9 4 0 0 0) (0 0 4 0 7 0 0 0 8) (0 0 0 0
0 5 0 0 6) (0 6 8 0 0 0 7 4 0) (4 0 0 7 0 0 0 0 0) (6 0 0 0 5 0 1 0 0)
(0 0 0 6 1 0 0 9 0) (0 0 3 0 0 0 0 5 2))))

(setq hard-puzzles '( ((3 0 0 0 9 0 4 0 0) (0 9 2 0 0 3 0 0 1) (0 5 0
8 0 0 0 0 0) (0 0 0 0 8 0 0 3 0) (9 0 4 7 0 1 6 0 8) (0 6 0 0 3 0 0 0
0) (0 0 0 0 0 8 0 6 0) (5 0 0 1 0 0 2 7 0) (0 0 9 0 2 0 0 0 5))

((0 0 0 3 1 6 0 0 8) (9 0 0 2 0 0 0 6 0) (8 0 6 0 0 0 0 3 0) (3 0 0 1
0 0 0 0 0) (6 1 0 0 0 0 0 2 9) (0 0 0 0 0 4 0 0 7) (0 6 0 0 0 0 8 0 4)
(0 2 0 0 0 5 0 0 3) (1 0 0 7 8 3 0 0 0))

((0 7 2 8 0 0 0 0 0) (0 0 8 5 0 9 0 6 0) (5 0 0 0 1 0 0 0 3) (0 4 0 6
9 0 0 0 0) (0 6 0 0 0 0 0 4 0) (0 0 0 0 4 7 0 3 0) (6 0 0 0 3 0 0 0 2)
(0 3 0 4 0 6 1 0 0) (0 0 0 0 0 8 3 7 0))

((0 0 9 2 0 0 0 0 0) (6 0 0 1 0 0 0 0 2) (0 2 1 0 3 0 0 0 0) (0 3 7 0
0 0 0 1 0) (0 5 0 9 0 3 0 8 0) (0 9 0 0 0 0 4 3 0) (0 0 0 0 2 0 3 5 0)
(9 0 0 0 0 7 0 0 1) (0 0 0 0 0 8 9 0 0))

((0 0 5 0 0 4 0 0 3) (0 0 1 9 0 6 0 0 0) (8 4 0 0 0 0 0 0 9) (0 3 0 6
0 2 0 0 0) (0 6 0 7 0 1 0 8 0) (0 0 0 4 0 3 0 7 0) (4 0 0 0 0 0 0 5 1)
(0 0 0 5 0 7 2 0 0) (5 0 0 3 0 0 4 0 0))

((0 0 0 0 0 3 0 0 0) (0 0 1 0 7 0 5 8 0) (9 0 0 0 6 0 0 2 0) (5 0 0 0
0 0 4 0 7) (0 2 0 0 0 0 0 5 0) (6 0 8 0 0 0 0 0 9) (0 5 0 0 3 0 0 0 1)
(0 7 9 0 4 0 6 0 0) (0 0 0 8 0 0 0 0 0))

((0 0 7 9 0 0 4 6 0) (0 8 0 0 7 0 3 0 0) (9 0 0 2 0 1 0 0 0) (7 0 0 0
0 0 9 1 0) (0 0 0 0 0 0 0 0 0) (0 9 6 0 0 0 0 0 8) (0 0 0 1 0 8 0 0 7)
(0 0 8 0 4 0 0 5 0) (0 1 2 0 0 5 6 0 0))

((0 0 1 0 0 9 0 0 0) (0 0 8 0 0 4 0 3 7) (6 0 0 0 7 0 0 8 0) (0 5 0 0
0 3 0 0 2) (8 0 9 0 0 0 3 0 6) (7 0 0 9 0 0 0 1 0) (0 3 0 0 2 0 0 0 8)
(5 6 0 4 0 0 7 0 0) (0 0 0 6 0 0 2 0 0))

((7 0 0 0 1 2 8 0 0) (0 0 2 6 5 0 0 9 0) (0 0 0 0 0 9 0 0 1) (0 2 0 3
0 0 0 0 0) (0 6 3 0 0 0 1 7 0) (0 0 0 0 0 1 0 2 0) (4 0 0 5 0 0 0 0 0)
(0 9 0 0 2 6 4 0 0) (0 0 8 1 4 0 0 0 9))

((1 0 0 0 0 2 0 9 4) (0 9 0 0 3 1 0 5 0) (0 0 6 0 0 0 1 0 0) (0 0 0 3
9 0 0 0 7) (0 0 0 0 5 0 0 0 0) (7 0 0 0 8 4 0 0 0) (0 0 9 0 0 0 8 0 0)
(0 5 0 6 2 0 0 4 0) (6 7 0 9 0 0 0 0 5))

((0 0 2 0 0 0 0 0 1) (0 1 0 0 8 0 0 0 6) (8 0 7 0 0 9 0 0 0) (0 0 0 0
9 0 0 3 4) (0 0 8 4 7 2 6 0 0) (4 5 0 0 3 0 0 0 0) (0 0 0 1 0 0 9 0 5)
(5 0 0 0 4 0 0 2 0) (7 0 0 0 0 0 4 0 0))

((9 0 3 0 0 6 0 0 0) (0 2 0 0 4 0 0 1 0) (5 8 0 0 0 0 0 0 0) (0 9 0 7
0 2 1 0 0) (1 0 0 0 0 0 0 0 2) (0 0 7 8 0 5 0 4 0) (0 0 0 0 0 0 0 7 6)
(0 6 0 0 7 0 0 8 0) (0 0 0 6 0 0 4 0 5))

((0 0 0 7 0 0 0 5 0) (0 0 0 0 3 0 0 2 8) (0 0 1 0 0 2 9 0 6) (0 1 0 6
0 7 0 0 0) (0 0 7 0 0 0 4 0 0) (0 0 0 8 0 9 0 1 0) (7 0 6 4 0 0 3 0 0)
(1 8 0 0 9 0 0 0 0) (0 4 0 0 0 3 0 0 0))

((6 0 0 4 7 2 0 0 0) (9 2 0 0 0 8 0 0 0) (0 1 0 0 0 0 0 0 0) (2 4 0 0
0 0 0 0 6) (0 6 0 0 3 0 0 7 0) (1 0 0 0 0 0 0 5 3) (0 0 0 0 0 0 0 1 0)
(0 0 0 2 0 0 0 9 4) (0 0 0 6 8 9 0 0 5))

((3 0 0 5 0 0 8 0 4) (0 2 0 7 0 8 0 0 0) (0 0 5 0 9 0 0 0 7) (0 7 0 0
0 0 6 0 3) (0 0 0 0 0 0 0 0 0) (6 0 8 0 0 0 0 2 0) (1 0 0 0 2 0 7 0 0)
(0 0 0 8 0 4 0 6 0) (9 0 3 0 0 6 0 0 2))

((5 0 0 0 0 0 0 1 6) (6 0 0 8 4 0 0 0 0) (0 1 7 0 0 0 0 0 0) (0 0 2 4
3 6 1 8 0) (0 0 0 0 0 0 0 0 0) (0 6 4 9 7 8 3 0 0) (0 0 0 0 0 0 9 6 0)
(0 0 0 0 5 3 0 0 1) (2 8 0 0 0 0 0 0 7))

((0 0 7 0 0 0 0 0 3) (0 0 9 0 0 2 0 1 0) (0 0 4 0 1 0 7 0 5) (8 0 2 3
0 0 0 0 0) (0 0 0 2 8 1 0 0 0) (0 0 0 0 0 6 9 0 2) (6 0 5 0 4 0 2 0 0)
(0 7 0 9 0 0 3 0 0) (4 0 0 0 0 0 8 0 0))

((0 5 0 0 8 0 0 0 9) (9 0 6 0 0 0 5 0 0) (0 0 8 0 1 0 0 0 0) (0 0 0 5
0 0 0 0 4) (3 9 0 0 6 0 0 7 2) (4 0 0 0 0 2 0 0 0) (0 0 0 0 3 0 4 0 0)
(0 0 1 0 0 0 8 0 3) (7 0 0 0 2 0 0 1 0))

((5 9 7 0 0 0 2 0 0) (0 0 6 0 5 2 0 0 0) (0 0 0 4 0 0 8 5 0) (0 0 0 0
3 0 6 0 9) (0 3 0 0 0 0 0 1 0) (4 0 2 0 6 0 0 0 0) (0 6 4 0 0 9 0 0 0)
(0 0 0 8 1 0 4 0 0) (0 0 8 0 0 0 1 9 3))

((3 6 0 0 7 0 0 0 0) (2 0 4 6 0 0 8 0 0) (0 9 0 0 0 1 0 0 0) (0 0 0 1
0 2 0 8 0) (0 0 5 0 0 0 1 0 0) (0 8 0 4 0 3 0 0 0) (0 0 0 7 0 0 0 5 0)
(0 0 7 0 0 5 2 0 1) (0 0 0 0 4 0 0 3 8))

((0 3 4 0 0 2 0 0 0) (0 0 1 3 7 5 0 0 0) (0 9 0 0 0 0 0 0 0) (0 1 6 0
0 0 9 0 0) (0 8 0 0 6 0 0 5 0) (0 0 5 0 0 0 2 4 0) (0 0 0 0 0 0 0 9 0)
(0 0 0 2 8 4 5 0 0) (0 0 0 7 0 0 3 2 0))

((0 0 0 0 4 2 5 0 0) (8 0 0 0 0 6 0 0 3) (4 0 0 8 0 0 0 0 7) (0 0 0 0
0 0 3 0 0) (0 0 9 6 5 4 2 0 0) (0 0 5 0 0 0 0 0 0) (3 0 0 0 0 1 0 0 4)
(7 0 0 4 0 0 0 0 2) (0 0 2 7 9 0 0 0 0))

((0 3 0 0 0 0 0 5 0) (1 0 0 2 9 0 0 0 7) (4 0 2 3 0 0 1 0 0) (0 0 4 0
5 7 0 0 0) (0 0 0 0 1 0 0 0 0) (0 0 0 6 3 0 4 0 0) (0 0 8 0 0 9 7 0 3)
(3 0 0 0 6 8 0 0 1) (0 2 0 0 0 0 0 8 0))

((0 0 7 0 0 0 0 6 5) (0 6 0 5 4 7 0 0 0) (0 0 5 0 0 3 0 2 0) (0 0 0 0
0 4 0 7 0) (0 2 3 0 0 0 4 5 0) (0 1 0 8 0 0 0 0 0) (0 7 0 9 0 0 3 0 0)
(0 0 0 7 6 1 0 4 0) (6 8 0 0 0 0 5 0 0))

((0 0 0 0 0 0 0 0 0) (1 0 0 5 2 0 0 0 0) (0 4 8 0 0 0 9 3 0) (8 0 3 0
6 0 0 9 0) (2 0 0 0 5 0 0 0 3) (0 7 0 0 3 0 6 0 1) (0 3 1 0 0 0 4 8 0)
(0 0 0 0 1 4 0 0 5) (0 0 0 0 0 0 0 0 0))

((1 3 0 0 6 0 0 0 0) (0 0 0 0 0 9 0 2 0) (0 0 6 0 4 0 8 1 0) (0 5 2 0
0 4 0 0 7) (0 0 0 0 5 0 0 0 0) (7 0 0 8 0 0 1 4 0) (0 7 5 0 8 0 4 0 0)
(0 8 0 9 0 0 0 0 0) (0 0 0 0 2 0 0 5 8))

((7 0 0 0 0 3 0 0 2) (0 0 0 4 0 0 3 0 0) (4 1 0 0 6 9 0 0 0) (0 9 4 0
0 0 0 0 0) (6 0 2 0 7 0 4 0 3) (0 0 0 0 0 0 1 2 0) (0 0 0 7 1 0 0 5 8)
(0 0 8 0 0 2 0 0 0) (5 0 0 8 0 0 0 0 4))

((5 0 0 0 7 4 0 2 0) (0 0 0 2 0 0 0 0 5) (0 0 0 0 8 0 4 3 0) (0 4 7 0
2 0 0 9 0) (0 0 0 0 0 0 0 0 0) (0 6 0 0 4 0 2 5 0) (0 8 4 0 6 0 0 0 0)
(6 0 0 0 0 3 0 0 0) (0 3 0 4 9 0 0 0 1))

((0 0 0 0 6 0 8 0 7) (8 5 0 0 1 0 0 6 0) (9 0 0 4 0 0 0 0 0) (0 0 2 1
0 0 0 9 3) (0 0 0 0 3 0 0 0 0) (1 8 0 0 0 5 2 0 0) (0 0 0 0 0 4 0 0 5)
(0 1 0 0 5 0 0 3 2) (3 0 5 0 9 0 0 0 0))

((0 0 0 0 2 6 0 5 0) (0 0 0 3 0 0 0 4 0) (9 0 0 0 5 0 6 0 0) (6 0 0 0
0 0 4 3 0) (4 0 2 0 3 0 7 0 6) (0 1 3 0 0 0 0 0 2) (0 0 8 0 4 0 0 0 3)
(0 6 0 0 0 5 0 0 0) (0 2 0 9 8 0 0 0 0))

((1 0 0 0 0 0 0 0 0) (2 0 5 0 0 9 0 0 0) (0 0 6 4 8 2 0 0 0) (4 0 2 0
0 0 6 0 0) (6 0 0 0 3 0 0 0 8) (0 0 1 0 0 0 3 0 7) (0 0 0 6 9 5 7 0 0)
(0 0 0 2 0 0 4 0 5) (0 0 0 0 0 0 0 0 1))

((0 4 0 7 0 0 0 0 0) (6 0 0 5 4 0 0 1 0) (0 0 7 0 9 8 0 0 5) (0 0 0 0
0 2 5 0 0) (4 0 1 0 0 0 8 0 2) (0 0 5 4 0 0 0 0 0) (3 0 0 8 5 0 7 0 0)
(0 7 0 0 3 4 0 0 6) (0 0 0 0 0 9 0 3 0))

((4 0 0 0 0 0 0 0 7) (0 7 0 0 0 9 1 2 0) (0 0 1 0 6 7 3 0 0) (0 0 0 6
1 0 0 5 0) (0 0 0 0 3 0 0 0 0) (0 5 0 0 8 2 0 0 0) (0 0 3 4 9 0 2 0 0)
(0 4 5 1 0 0 0 3 0) (1 0 0 0 0 0 0 0 8))

((0 3 9 1 6 0 0 0 0) (1 0 4 7 0 5 0 0 0) (0 0 0 0 0 0 0 0 0) (9 0 0 0
0 0 8 0 2) (8 0 0 0 7 0 0 0 6) (5 0 2 0 0 0 0 0 4) (0 0 0 0 0 0 0 0 0)
(0 0 0 3 0 2 9 0 5) (0 0 0 0 5 4 2 3 0))

((0 8 0 0 9 0 2 0 0) (0 0 0 8 3 0 0 1 0) (0 0 7 0 0 0 0 0 5) (0 3 0 9
0 0 4 0 6) (0 6 4 0 0 0 5 8 0) (1 0 8 0 0 4 0 3 0) (4 0 0 0 0 0 1 0 0)
(0 1 0 0 7 8 0 0 0) (0 0 2 0 4 0 0 5 0))

((0 0 0 9 0 0 2 7 0) (0 3 0 7 2 0 0 0 0) (0 6 0 0 5 0 3 0 0) (3 0 0 0
0 5 0 0 9) (0 2 0 0 3 0 0 8 0) (1 0 0 8 0 0 0 0 3) (0 0 4 0 9 0 0 6 0)
(0 0 0 0 1 4 0 5 0) (0 9 1 0 0 8 0 0 0))

((8 7 0 0 0 9 5 1 0) (0 5 3 0 8 0 0 4 0) (0 0 0 5 0 0 0 0 0) (2 0 0 0
0 0 8 0 0) (6 0 0 0 2 0 0 0 1) (0 0 5 0 0 0 0 0 3) (0 0 0 0 0 3 0 0 0)
(0 9 0 0 7 0 3 5 0) (0 8 6 9 0 0 0 7 4))

((9 4 5 1 0 0 0 0 3) (0 0 0 9 0 0 0 5 2) (0 0 0 0 0 0 8 0 0) (0 0 0 6
8 0 9 0 0) (7 0 0 0 4 0 0 0 8) (0 0 1 0 9 5 0 0 0) (0 0 3 0 0 0 0 0 0)
(6 7 0 0 0 4 0 0 0) (2 0 0 0 0 9 1 3 7))

((0 0 0 3 6 2 5 0 0) (0 0 4 7 0 0 0 0 2) (0 2 5 0 0 0 0 0 3) (0 0 3 6
0 0 0 0 0) (6 0 2 0 0 0 4 0 7) (0 0 0 0 0 9 8 0 0) (2 0 0 0 0 0 9 5 0)
(7 0 0 0 0 1 3 0 0) (0 0 6 8 5 3 0 0 0))

((0 0 0 0 2 0 5 0 8) (6 0 0 0 0 1 0 0 2) (0 0 7 4 0 9 0 0 0) (0 0 0 0
0 0 0 3 0) (1 0 3 0 0 0 6 0 9) (0 8 0 0 0 0 0 0 0) (0 0 0 6 0 2 4 0 0)
(4 0 0 8 0 0 0 0 7) (9 0 2 0 5 0 0 0 0))

((6 0 3 8 0 0 0 5 0) (9 0 0 0 2 0 7 0 0) (0 0 0 6 0 0 0 2 0) (0 0 5 4
0 0 0 0 1) (0 4 7 0 0 0 6 9 0) (3 0 0 0 0 9 2 0 0) (0 1 0 0 0 4 0 0 0)
(0 0 6 0 5 0 0 0 7) (0 7 0 0 0 8 5 0 9))

((0 0 0 0 0 0 0 0 0) (0 1 7 9 0 3 0 0 0) (3 9 0 5 7 0 0 0 0) (0 0 5 0
0 0 7 9 0) (0 0 4 0 2 0 6 0 0) (0 6 9 0 0 0 1 0 0) (0 0 0 0 4 8 0 1 3)
(0 0 0 7 0 2 8 5 0) (0 0 0 0 0 0 0 0 0))

((0 0 8 2 0 0 0 0 0) (1 0 2 0 5 0 0 0 3) (0 0 0 6 4 0 0 1 0) (0 0 0 3
0 0 5 0 0) (3 9 0 0 8 0 0 6 4) (0 0 7 0 0 6 0 0 0) (0 1 0 0 7 3 0 0 0)
(5 0 0 0 6 0 4 0 8) (0 0 0 0 0 5 9 0 0))

((0 0 7 2 0 0 0 5 0) (0 4 5 1 0 3 0 0 0) (0 0 0 0 9 0 3 0 0) (0 5 0 0
2 0 4 0 0) (0 0 1 0 8 0 6 0 0) (0 0 8 0 6 0 0 9 0) (0 0 6 0 5 0 0 0 0)
(0 0 0 4 0 2 5 6 0) (0 3 0 0 0 7 2 0 0))

((0 0 3 0 1 0 0 4 2) (0 0 0 8 3 0 0 0 0) (0 0 0 0 0 0 9 3 1) (3 0 0 0
7 2 0 0 0) (0 7 0 5 0 6 0 8 0) (0 0 0 3 9 0 0 0 5) (7 5 2 0 0 0 0 0 0)
(0 0 0 0 2 3 0 0 0) (9 3 0 0 5 0 4 0 0))

((0 0 0 1 3 0 6 0 0) (4 0 6 0 0 7 0 0 0) (0 0 1 0 0 0 3 8 7) (0 6 2 0
4 0 0 0 0) (8 0 0 0 0 0 0 0 3) (0 0 0 0 8 0 4 7 0) (7 9 5 0 0 0 2 0 0)
(0 0 0 6 0 0 1 0 9) (0 0 4 0 9 2 0 0 0))

((0 9 7 0 0 6 0 0 0) (0 4 0 0 7 0 0 0 8) (5 0 0 0 1 0 0 2 0) (0 0 1 0
2 0 0 6 0) (4 0 0 6 0 1 0 0 3) (0 2 0 0 9 0 1 0 0) (0 5 0 0 3 0 0 0 6)
(3 0 0 0 8 0 0 4 0) (0 0 0 7 0 0 5 3 0))

((0 0 4 0 0 2 7 0 0) (0 9 0 3 7 0 0 0 0) (0 0 1 6 0 0 2 0 0) (0 1 0 0
0 0 0 0 0) (0 3 0 7 9 6 0 8 0) (0 0 0 0 0 0 0 9 0) (0 0 3 0 0 7 4 0 0)
(0 0 0 0 8 4 0 3 0) (0 0 7 5 0 0 1 0 0))

((5 0 0 0 0 2 0 0 0) (9 0 0 3 0 0 2 1 0) (0 0 8 6 0 0 3 0 0) (0 0 0 0
0 1 0 4 0) (8 0 7 0 3 0 1 0 6) (0 4 0 7 0 0 0 0 0) (0 0 1 0 0 5 7 0 0)
(0 6 5 0 0 7 0 0 3) (0 0 0 2 0 0 0 0 1))

((0 0 9 0 0 7 0 0 0) (3 8 0 0 5 0 0 0 0) (7 0 0 8 2 0 9 0 0) (1 0 0 0
7 0 0 2 8) (0 0 0 0 0 0 0 0 0) (9 7 0 0 8 0 0 0 6) (0 0 4 0 1 8 0 0 3)
(0 0 0 0 6 0 0 8 5) (0 0 0 3 0 0 6 0 0))

((1 0 4 0 0 0 5 0 0) (0 2 0 8 1 0 0 0 0) (0 0 0 0 0 5 0 0 1) (9 0 3 2
0 0 8 7 0) (0 0 0 0 0 0 0 0 0) (0 5 7 0 0 8 2 0 4) (3 0 0 9 0 0 0 0 0)
(0 0 0 0 6 1 0 8 0) (0 0 8 0 0 0 7 0 6))

((0 0 0 0 3 0 9 0 0) (0 0 1 8 2 0 0 4 0) (0 5 0 0 0 0 0 6 8) (0 0 0 0
0 2 0 0 0) (9 0 6 0 7 0 2 0 4) (0 0 0 5 0 0 0 0 0) (6 7 0 0 0 0 0 9 0)
(0 1 0 0 4 8 3 0 0) (0 0 5 0 1 0 0 0 0))

((7 0 4 0 0 8 0 9 0) (8 0 0 0 0 1 0 0 2) (0 0 0 7 0 0 0 5 0) (0 0 3 4
0 0 0 0 0) (4 1 0 0 8 0 0 2 6) (0 0 0 0 0 6 3 0 0) (0 4 0 0 0 7 0 0 0)
(6 0 0 5 0 0 0 0 4) (0 8 0 6 0 0 1 0 5))))

(setq medium-puzzles '( ((0 0 0 8 7 0 1 0 2) (0 0 1 0 4 0 9 0 6) (0 0
0 0 0 6 0 0 0) (0 0 0 0 8 0 0 5 3) (5 0 6 0 3 0 4 0 9) (8 2 0 0 5 0 0
0 0) (0 0 0 1 0 0 0 0 0) (1 0 5 0 2 0 3 0 0) (7 0 4 0 6 8 0 0 0))

((0 5 0 0 0 8 0 1 0) (0 9 0 0 0 5 0 0 6) (0 0 0 7 4 0 9 0 8) (0 0 0 0
0 0 3 0 5) (0 1 4 0 0 0 8 6 0) (5 0 8 0 0 0 0 0 0) (1 0 9 0 7 6 0 0 0)
(7 0 0 2 0 0 0 8 0) (0 2 0 3 0 0 0 9 0))

((0 0 0 0 2 8 0 0 3) (0 0 0 0 0 7 1 0 2) (0 3 2 0 1 0 0 9 0) (1 0 0 0
0 0 7 2 0) (4 0 0 0 0 0 0 0 5) (0 8 6 0 0 0 0 0 4) (0 6 0 0 9 0 3 7 0)
(9 0 4 7 0 0 0 0 0) (3 0 0 2 5 0 0 0 0))

((9 8 4 0 0 3 5 0 0) (0 7 0 0 4 0 0 1 0) (0 0 3 0 0 0 0 0 4) (5 0 0 0
9 0 0 7 8) (0 0 0 2 0 8 0 0 0) (3 6 0 0 1 0 0 0 2) (4 0 0 0 0 0 8 0 0)
(0 3 0 0 8 0 0 5 0) (0 0 5 4 0 0 7 2 1))

((4 0 0 0 1 0 0 0 2) (0 0 7 2 0 0 4 0 9) (0 0 0 8 0 0 0 6 0) (0 0 0 6
0 8 1 7 0) (0 0 3 0 4 0 2 0 0) (0 6 1 3 0 2 0 0 0) (0 2 0 0 0 9 0 0 0)
(7 0 4 0 0 5 6 0 0) (9 0 0 0 8 0 0 0 7))

((0 0 0 3 7 0 0 0 4) (0 7 1 0 0 0 8 0 5) (0 0 3 4 0 0 0 0 7) (0 9 0 8
2 0 0 0 0) (3 0 7 0 0 0 5 0 2) (0 0 0 0 3 7 0 4 0) (7 0 0 0 0 1 2 0 0)
(2 0 8 0 0 0 3 6 0) (1 0 0 0 8 3 0 0 0))

((4 0 0 5 6 0 0 0 0) (0 6 0 0 7 8 3 0 0) (0 3 0 1 0 0 0 7 0) (6 0 0 0
0 0 0 0 3) (9 0 1 0 0 0 8 0 4) (2 0 0 0 0 0 0 0 6) (0 2 0 0 0 4 0 6 0)
(0 0 9 3 5 0 0 2 0) (0 0 0 0 2 1 0 0 7))

((6 2 1 0 0 9 0 0 4) (8 5 0 0 0 0 0 0 0) (7 0 0 0 0 6 9 1 0) (9 0 0 0
7 0 4 0 2) (0 8 0 0 0 0 0 9 0) (3 0 4 0 1 0 0 0 5) (0 9 2 8 0 0 0 0 3)
(0 0 0 0 0 0 0 4 7) (4 0 0 3 0 0 5 2 9))

((0 0 0 8 3 0 0 6 0) (0 5 0 0 9 0 0 3 7) (0 0 0 0 0 0 5 0 0) (2 0 0 0
8 0 0 0 3) (5 3 8 0 1 0 2 4 9) (1 0 0 0 5 0 0 0 6) (0 0 3 0 0 0 0 0 0)
(4 2 0 0 6 0 0 1 0) (0 7 0 0 2 1 0 0 0))

((0 0 9 5 8 0 6 0 0) (0 3 0 0 0 9 0 4 0) (7 0 0 0 4 0 5 0 0) (0 0 4 3
0 0 0 6 5) (5 0 0 9 0 8 0 0 1) (2 1 0 0 0 7 3 0 0) (0 0 2 0 3 0 0 0 6)
(0 6 0 2 0 0 0 3 0) (0 0 7 0 9 6 1 0 0))

((6 5 0 9 8 0 0 0 2) (0 0 0 0 0 1 9 0 0) (0 0 0 7 6 2 0 0 0) (0 9 7 0
0 0 3 0 0) (8 0 0 0 1 0 0 0 4) (0 0 2 0 0 0 6 5 0) (0 0 0 6 7 8 0 0 0)
(0 0 3 1 0 0 0 0 0) (5 0 0 0 9 3 0 2 8))

((0 0 1 0 8 0 2 0 0) (0 0 0 4 0 3 0 0 5) (0 0 2 9 0 0 0 0 0) (0 1 3 0
0 0 0 2 7) (0 6 5 0 7 0 1 4 0) (7 4 0 0 0 0 5 6 0) (0 0 0 0 0 5 9 0 0)
(3 0 0 2 0 6 0 0 0) (0 0 9 0 1 0 4 0 0))

((8 0 0 0 0 4 0 0 0) (7 6 0 8 5 0 0 4 0) (4 0 3 0 2 0 0 0 7) (0 1 0 0
9 0 0 5 0) (3 0 0 6 0 2 0 0 1) (0 8 0 0 4 0 0 6 0) (5 0 0 0 3 0 6 0 9)
(0 3 0 0 6 5 0 7 8) (0 0 0 7 0 0 0 0 5))

((3 2 0 0 1 0 7 0 0) (1 5 9 6 0 3 0 0 2) (6 0 0 0 0 2 1 0 0) (0 0 0 0
0 0 4 0 1) (0 0 0 7 0 5 0 0 0) (9 0 8 0 0 0 0 0 0) (0 0 3 2 0 0 0 0 7)
(2 0 0 9 0 1 5 8 4) (0 0 1 0 8 0 0 6 3))

((0 0 2 0 5 0 9 4 0) (7 3 0 8 0 1 0 0 0) (0 0 5 0 0 0 0 3 0) (8 9 0 0
0 0 1 2 0) (3 0 0 0 0 0 0 0 6) (0 2 7 0 0 0 0 8 3) (0 7 0 0 0 0 6 0 0)
(0 0 0 5 0 7 0 1 9) (0 1 3 0 9 0 2 0 0))

((0 0 0 1 6 0 0 2 0) (0 0 7 3 0 0 5 0 0) (2 1 0 0 0 8 0 0 9) (0 0 1 0
9 5 0 0 6) (9 5 0 0 0 0 0 8 4) (7 0 0 2 4 0 9 0 0) (3 0 0 5 0 0 0 6 7)
(0 0 5 0 0 7 8 0 0) (0 7 0 0 3 6 0 0 0))

((0 9 8 4 5 0 0 0 2) (0 0 4 8 0 0 6 0 0) (0 0 0 0 0 0 0 8 5) (0 0 5 9
0 0 7 4 0) (1 0 0 0 0 0 0 0 9) (0 4 3 0 0 5 2 0 0) (2 6 0 0 0 0 0 0 0)
(0 0 1 0 0 6 9 0 0) (5 0 0 0 3 2 1 6 0))

((9 0 0 0 2 0 0 5 7) (0 0 0 7 9 3 0 0 4) (0 0 0 0 8 0 2 0 0) (0 8 0 0
0 0 0 0 0) (1 2 0 8 3 9 0 7 6) (0 0 0 0 0 0 0 3 0) (0 0 9 0 7 0 0 0 0)
(5 0 0 2 1 6 0 0 0) (2 6 0 0 4 0 0 0 3))

((0 0 4 7 0 0 0 0 0) (0 0 0 0 2 0 7 3 0) (9 7 8 5 0 0 1 0 0) (0 0 0 0
6 5 3 1 0) (1 0 0 0 7 0 0 0 6) (0 8 5 3 4 0 0 0 0) (0 0 9 0 0 3 2 7 4)
(0 3 2 0 9 0 0 0 0) (0 0 0 0 0 7 6 0 0))

((3 0 0 0 0 0 0 0 6) (0 0 0 4 3 0 1 0 0) (0 0 2 0 0 0 9 0 0) (0 0 9 3
8 0 0 0 5) (6 0 8 7 4 9 3 0 2) (7 0 0 0 5 1 6 0 0) (0 0 4 0 0 0 2 0 0)
(0 0 7 0 2 8 0 0 0) (5 0 0 0 0 0 0 0 9))

((7 0 5 6 1 0 0 0 0) (1 0 2 0 4 0 0 0 7) (0 0 0 8 0 0 0 0 0) (0 7 0 0
0 0 6 0 0) (8 0 4 1 9 6 5 0 2) (0 0 6 0 0 0 0 4 0) (0 0 0 0 0 1 0 0 0)
(9 0 0 0 2 0 7 0 3) (0 0 0 0 3 9 8 0 4))

((0 0 8 0 0 4 5 0 0) (0 1 0 5 9 0 0 6 0) (5 0 0 0 8 0 0 4 0) (0 8 0 6
0 0 1 0 4) (1 0 0 3 0 9 0 0 2) (2 0 5 0 0 8 0 7 0) (0 2 0 0 7 0 0 0 6)
(0 5 0 0 3 2 0 9 0) (0 0 7 9 0 0 8 0 0))

((0 7 1 0 4 0 0 2 0) (0 9 8 2 7 0 0 0 0) (0 0 0 5 0 0 0 0 0) (9 0 0 0
0 0 3 0 0) (0 4 6 3 2 5 9 8 0) (0 0 3 0 0 0 0 0 1) (0 0 0 0 0 8 0 0 0)
(0 0 0 0 5 3 6 1 0) (0 1 0 0 9 0 4 5 0))

((5 0 7 0 4 0 0 0 0) (0 0 0 0 0 1 2 0 0) (0 0 4 0 0 5 7 8 1) (9 0 3 5
8 0 0 0 0) (0 6 0 0 1 0 0 2 0) (0 0 0 0 2 3 5 0 6) (1 4 9 3 0 0 6 0 0)
(0 0 8 1 0 0 0 0 0) (0 0 0 0 7 0 1 0 5))

((0 0 4 8 0 0 0 0 0) (3 6 0 9 0 1 0 0 4) (1 0 0 3 0 0 2 7 0) (0 0 0 1
0 0 9 2 0) (7 8 0 0 0 0 0 4 3) (0 4 9 0 0 3 0 0 0) (0 1 5 0 0 7 0 0 2)
(4 0 0 2 0 8 0 1 6) (0 0 0 0 0 5 7 0 0))

((0 2 5 8 0 0 0 0 0) (7 0 0 0 0 0 8 0 3) (0 0 1 0 7 0 6 0 4) (0 0 0 9
0 3 0 4 0) (0 0 2 0 0 0 5 0 0) (0 7 0 4 0 5 0 0 0) (5 0 4 0 3 0 1 0 0)
(2 0 3 0 0 0 0 0 6) (0 0 0 0 0 6 4 3 0))

((3 0 9 0 6 4 0 0 7) (4 0 0 0 0 0 9 3 0) (5 0 0 3 0 0 0 0 0) (0 4 0 0
7 8 0 6 0) (0 0 0 0 0 0 0 0 0) (0 6 0 4 5 0 0 8 0) (0 0 0 0 0 7 0 0 6)
(0 5 1 0 0 0 0 0 4) (6 0 0 2 1 0 7 0 5))

((0 0 0 0 0 5 4 0 3) (0 0 6 8 7 0 0 0 0) (0 0 0 0 0 0 1 7 0) (0 0 7 4
8 0 2 3 0) (4 6 0 5 0 2 0 9 8) (0 3 2 0 9 1 6 0 0) (0 2 3 0 0 0 0 0 0)
(0 0 0 0 5 7 8 0 0) (5 0 8 6 0 0 0 0 0))

((0 4 0 0 0 0 0 0 7) (3 2 0 0 7 0 0 0 5) (0 0 0 6 0 1 9 4 0) (6 5 0 0
0 0 1 3 0) (0 0 8 0 0 0 4 0 0) (0 1 4 0 0 0 0 5 9) (0 6 3 9 0 7 0 0 0)
(5 0 0 0 3 0 0 6 4) (8 0 0 0 0 0 0 9 0))

((0 0 0 0 0 0 0 0 0) (3 5 7 0 0 9 4 0 0) (0 9 0 8 2 0 7 0 0) (5 0 0 0
0 0 1 7 0) (7 3 0 0 8 0 0 2 9) (0 1 8 0 0 0 0 0 5) (0 0 5 0 7 8 0 6 0)
(0 0 6 2 0 0 8 5 7) (0 0 0 0 0 0 0 0 0))

((9 0 0 8 0 0 0 0 0) (0 0 0 2 0 1 0 7 0) (3 0 0 0 4 0 0 0 9) (1 0 3 0
0 0 9 5 0) (7 0 6 0 5 0 2 0 3) (0 5 2 0 0 0 6 0 7) (8 0 0 0 3 0 0 0 2)
(0 1 0 9 0 6 0 0 0) (0 0 0 0 0 7 0 0 8))

((0 0 2 0 0 5 3 8 0) (0 0 8 1 9 0 0 0 0) (0 6 1 0 7 8 0 0 0) (0 2 0 0
0 0 9 0 0) (0 8 0 0 2 0 0 7 0) (0 0 7 0 0 0 0 2 0) (0 0 0 5 4 0 1 9 0)
(0 0 0 0 8 1 4 0 0) (0 1 6 2 0 0 8 0 0))

((4 0 2 9 0 0 0 0 8) (0 0 0 0 5 0 0 0 4) (1 0 0 0 8 3 2 0 0) (2 0 0 0
0 5 0 9 0) (0 0 5 0 0 0 1 0 0) (0 8 0 3 0 0 0 0 5) (0 0 4 2 9 0 0 0 6)
(3 0 0 0 7 0 0 0 0) (5 0 0 0 0 1 7 0 2))

((0 0 7 8 0 6 3 0 4) (0 5 0 3 0 0 0 0 1) (6 2 0 0 7 0 0 0 0) (0 0 0 0
0 1 4 0 3) (8 0 0 0 0 0 0 0 6) (2 0 5 6 0 0 0 0 0) (0 0 0 0 1 0 0 6 2)
(5 0 0 0 0 3 0 4 0) (7 0 1 2 0 9 5 0 0))

((8 0 0 6 2 0 0 0 1) (0 0 0 0 0 0 0 0 0) (0 1 3 0 5 8 0 2 0) (0 0 0 0
1 3 7 0 0) (0 0 5 7 4 6 2 0 0) (0 0 4 5 9 0 0 0 0) (0 2 0 1 7 0 9 4 0)
(0 0 0 0 0 0 0 0 0) (5 0 0 0 8 9 0 0 6))

((0 6 0 0 0 4 1 9 3) (0 0 0 0 0 1 0 8 0) (0 1 7 0 5 0 0 0 0) (0 7 6 4
2 0 0 0 0) (2 0 0 0 1 0 0 0 6) (0 0 0 0 8 7 9 4 0) (0 0 0 0 3 0 7 5 0)
(0 2 0 1 0 0 0 0 0) (8 5 1 7 0 0 0 3 0))

((0 0 2 0 0 7 0 8 0) (6 0 9 1 0 5 0 0 2) (0 0 0 0 6 0 1 3 0) (2 0 1 3
0 0 0 0 0) (0 0 4 0 0 0 3 0 0) (0 0 0 0 0 6 7 0 8) (0 1 3 0 9 0 0 0 0)
(9 0 0 4 0 3 8 0 7) (0 2 0 7 0 0 6 0 0))

((7 0 0 5 9 0 1 0 0) (2 8 0 0 0 0 9 0 0) (0 9 0 4 2 0 0 0 0) (0 0 0 0
0 0 6 5 7) (0 7 0 0 0 0 0 8 0) (4 5 6 0 0 0 0 0 0) (0 0 0 0 8 7 0 2 0)
(0 0 7 0 0 0 0 1 5) (0 0 9 0 5 2 0 0 3))

((0 0 1 3 8 0 0 7 0) (0 0 0 0 0 0 0 0 0) (2 7 6 0 0 1 0 9 0) (6 0 0 0
0 0 7 5 0) (7 0 2 0 3 0 8 0 1) (0 3 5 0 0 0 0 0 6) (0 4 0 8 0 0 6 3 7)
(0 0 0 0 0 0 0 0 0) (0 6 0 0 7 3 4 0 0))

((0 0 0 7 0 0 0 0 0) (0 8 5 0 0 0 0 0 0) (7 1 2 9 3 0 0 0 0) (8 2 0 3
0 0 0 7 0) (0 7 1 4 5 9 3 8 0) (0 4 0 0 0 7 0 9 1) (0 0 0 0 4 1 7 3 9)
(0 0 0 0 0 0 5 2 0) (0 0 0 0 0 3 0 0 0))

((5 8 0 0 0 9 0 0 7) (0 0 3 8 4 0 0 0 6) (0 0 0 1 0 0 4 0 0) (8 0 0 3
6 0 0 2 0) (0 0 7 0 1 0 9 0 0) (0 1 0 0 7 8 0 0 4) (0 0 9 0 0 1 0 0 0)
(2 0 0 0 5 3 6 0 0) (7 0 0 2 0 0 0 4 5))

((0 0 0 2 7 9 0 4 0) (0 0 0 0 1 0 0 0 6) (0 7 0 0 6 0 3 2 0) (0 0 1 0
0 0 0 0 0) (0 8 6 1 9 7 2 5 0) (0 0 0 0 0 0 9 0 0) (0 6 5 0 4 0 0 9 0)
(7 0 0 0 2 0 0 0 0) (0 3 0 6 8 5 0 0 0))

((0 0 8 4 7 9 0 0 0) (0 1 9 0 6 0 7 0 0) (6 0 0 0 3 0 0 0 0) (0 0 0 0
0 0 0 3 0) (0 9 5 7 4 3 2 6 0) (0 4 0 0 0 0 0 0 0) (0 0 0 0 9 0 0 0 7)
(0 0 4 0 8 0 6 5 0) (0 0 0 5 2 6 1 0 0))

((0 0 2 0 7 5 0 0 1) (0 0 0 0 0 0 0 0 0) (0 0 4 1 0 0 2 9 6) (2 0 3 0
0 0 0 6 0) (7 1 0 0 5 0 0 2 9) (0 6 0 0 0 0 5 0 3) (6 2 5 0 0 7 8 0 0)
(0 0 0 0 0 0 0 0 0) (8 0 0 5 2 0 6 0 0))

((0 0 0 2 0 6 0 5 1) (1 0 9 0 5 0 3 0 0) (6 0 0 0 0 0 8 0 0) (3 0 6 0
0 0 0 9 4) (0 9 0 0 0 0 0 8 0) (5 4 0 0 0 0 1 0 3) (0 0 2 0 0 0 0 0 9)
(0 0 3 0 2 0 5 0 7) (9 6 0 4 0 1 0 0 0))

((4 0 0 0 0 0 9 6 5) (6 7 0 0 0 0 0 0 0) (0 0 3 8 0 0 0 0 1) (0 4 0 2
0 7 0 0 0) (2 3 0 6 0 4 0 7 8) (0 0 0 5 0 3 0 2 0) (1 0 0 0 0 5 8 0 0)
(0 0 0 0 0 0 0 5 2) (7 9 5 0 0 0 0 0 4))

((0 0 0 0 0 0 4 0 0) (0 0 0 7 9 0 0 0 5) (7 0 0 0 6 0 0 1 9) (0 6 0 0
3 0 0 7 0) (1 8 9 0 7 0 2 3 4) (0 4 0 0 2 0 0 9 0) (4 5 0 0 8 0 0 0 3)
(6 0 0 0 4 2 0 0 0) (0 0 3 0 0 0 0 0 0))

((2 0 0 5 0 0 0 0 3) (3 0 0 0 8 1 0 6 0) (0 0 4 9 3 0 0 0 0) (0 0 2 0
0 0 3 0 0) (0 7 5 0 0 0 6 9 0) (0 0 1 0 0 0 2 0 0) (0 0 0 0 2 8 5 0 0)
(0 1 0 7 4 0 0 0 2) (4 0 0 0 0 9 0 0 1))

((7 0 0 2 0 0 0 0 0) (0 0 0 5 0 8 0 6 0) (4 0 0 0 3 0 0 0 7) (2 0 5 0
0 0 4 9 0) (3 0 4 0 9 0 5 0 2) (0 9 8 0 0 0 3 0 6) (8 0 0 0 1 0 0 0 3)
(0 2 0 6 0 4 0 0 0) (0 0 0 0 0 7 0 0 8))

((0 0 0 2 7 0 0 8 4) (2 1 0 3 0 0 0 9 0) (0 0 0 0 5 8 0 2 0) (0 5 0 0
0 0 0 0 9) (7 0 0 0 9 0 0 0 2) (9 0 0 0 0 0 0 7 0) (0 6 0 8 2 0 0 0 0)
(0 2 0 0 0 9 0 4 8) (5 8 0 0 6 3 0 0 0))))

(setq easy-puzzles '( ((8 0 0 5 0 6 9 4 0) (0 6 0 0 9 0 5 3 0) (0 0 5
7 0 4 0 0 6) (0 0 0 0 2 0 1 0 4) (0 0 0 1 0 8 0 0 0) (6 0 2 0 7 0 0 0
0) (9 0 0 2 0 7 6 0 0) (0 2 6 0 5 0 0 7 0) (0 7 8 6 0 3 0 0 1))

((0 7 0 2 1 8 4 0 6) (0 0 0 5 0 4 0 0 0) (2 0 0 0 0 0 0 9 5) (4 0 8 6
5 0 3 0 7) (0 0 7 0 0 0 6 0 0) (6 0 1 0 8 7 2 0 9) (7 6 0 0 0 0 0 0 4)
(0 0 0 4 0 6 0 0 0) (1 0 5 8 2 9 0 6 0))

((0 0 0 0 4 0 0 0 0) (9 0 0 3 0 5 0 0 4) (3 0 7 8 0 2 1 0 6) (0 9 4 5
0 3 0 8 0) (0 0 2 0 8 0 3 0 0) (0 3 0 9 0 4 5 6 0) (7 0 1 2 0 8 4 0 3)
(8 0 0 4 0 7 0 0 2) (0 0 0 0 6 0 0 0 0))

((0 0 0 0 1 2 0 8 0) (0 4 0 7 9 0 0 0 6) (2 5 0 0 4 6 0 3 0) (8 0 0 0
0 0 9 0 0) (0 6 5 9 7 8 2 1 0) (0 0 2 0 0 0 0 0 8) (0 8 0 1 2 0 0 6 4)
(9 0 0 0 5 3 0 7 0) (0 3 0 4 8 0 0 0 0))

((6 0 0 1 9 3 0 0 8) (1 0 0 0 8 0 6 0 0) (0 0 0 6 0 0 0 4 9) (9 0 0 0
4 6 8 0 0) (2 0 6 0 0 0 5 0 4) (0 0 3 7 5 0 0 0 1) (3 5 0 0 0 2 0 0 0)
(0 0 2 0 6 0 0 0 5) (8 0 0 4 3 5 0 0 2))

((0 3 1 0 6 0 0 2 8) (0 0 0 0 3 4 0 0 0) (0 4 0 2 0 0 5 0 0) (0 0 7 0
5 0 0 9 1) (9 1 5 0 7 0 3 4 2) (4 6 0 0 2 0 8 0 0) (0 0 6 0 0 2 0 1 0)
(0 0 0 5 4 0 0 0 0) (2 8 0 0 9 0 7 6 0))

((4 0 0 0 0 0 1 0 0) (6 0 7 3 1 0 4 9 8) (0 9 0 8 0 0 0 0 0) (7 0 8 0
0 1 6 5 0) (0 0 3 4 0 7 8 0 0) (0 6 9 5 0 0 2 0 4) (0 0 0 0 0 5 0 2 0)
(9 1 4 0 2 3 5 0 6) (0 0 5 0 0 0 0 0 7))

((0 0 0 0 0 1 5 0 0) (0 3 0 0 2 7 0 9 0) (2 1 6 0 0 0 0 8 0) (0 0 7 0
0 4 0 3 8) (6 4 0 7 0 8 0 1 5) (9 5 0 3 0 0 6 0 0) (0 6 0 0 0 0 3 5 1)
(0 8 0 1 3 0 0 7 0) (0 0 1 4 0 0 0 0 0))

((0 0 2 9 8 0 0 1 0) (6 0 1 0 0 0 3 0 9) (0 0 9 0 0 6 4 8 0) (0 0 7 0
0 4 0 3 0) (8 3 0 1 0 2 0 9 4) (0 2 0 8 0 0 5 0 0) (0 6 8 5 0 0 9 0 0)
(1 0 3 0 0 0 8 0 2) (0 9 0 0 6 8 1 0 0))

((0 2 0 5 0 0 0 1 0) (8 0 4 0 6 1 0 0 7) (0 1 0 0 7 8 5 0 0) (0 0 9 8
0 0 0 0 5) (1 0 2 0 5 0 9 0 8) (5 0 0 0 0 7 4 0 0) (0 0 8 4 2 0 0 5 0)
(6 0 0 7 3 0 2 0 4) (0 4 0 0 0 9 0 7 0))

((9 0 0 0 4 1 0 3 0) (0 6 5 0 0 0 7 0 0) (3 2 0 0 5 0 9 0 0) (0 3 8 2
6 0 0 0 0) (6 0 9 0 7 0 3 0 4) (0 0 0 0 1 3 6 7 0) (0 0 6 0 9 0 0 1 3)
(0 0 2 0 0 0 4 6 0) (0 4 0 7 8 0 0 0 9))

((0 0 0 0 7 0 8 0 2) (0 0 5 9 0 2 0 3 0) (8 3 0 6 1 0 9 0 0) (5 0 6 0
0 8 0 0 1) (0 0 1 0 5 0 4 0 0) (2 0 0 3 0 0 5 0 9) (0 0 7 0 4 5 0 9 3)
(0 5 0 7 0 9 6 0 0) (9 0 8 0 3 0 0 0 0))

((0 3 0 0 0 8 0 0 0) (0 6 2 0 0 3 7 0 1) (0 9 7 0 1 0 0 8 4) (0 0 0 0
4 2 0 6 9) (0 8 0 1 0 5 0 3 0) (4 7 0 8 3 0 0 0 0) (9 4 0 0 2 0 1 5 0)
(6 0 8 5 0 0 2 7 0) (0 0 0 3 0 0 0 4 0))

((0 3 7 6 0 0 0 0 4) (0 0 5 7 2 9 6 0 0) (0 0 0 0 3 5 0 7 0) (0 0 0 0
1 6 7 0 9) (3 0 9 0 0 0 1 0 8) (6 0 4 8 9 0 0 0 0) (0 9 0 2 5 0 0 0 0)
(0 0 3 1 7 4 9 0 0) (7 0 0 0 0 8 3 1 0))

((0 0 5 8 9 0 0 0 0) (0 0 4 2 7 0 6 0 8) (2 0 0 0 1 3 7 0 0) (0 1 0 0
0 0 0 0 5) (0 8 9 5 3 1 2 6 0) (5 0 0 0 0 0 0 8 0) (0 0 3 4 6 0 0 0 1)
(7 0 2 0 8 9 5 0 0) (0 0 0 0 5 7 4 0 0))

((0 0 1 8 0 0 0 6 0) (0 0 2 0 0 3 7 0 4) (9 0 3 7 2 6 8 0 0) (0 0 0 9
4 0 5 8 0) (0 0 9 0 0 0 6 0 0) (0 2 5 0 3 8 0 0 0) (0 0 4 2 5 1 3 0 8)
(3 0 7 4 0 0 1 0 0) (0 1 0 0 0 7 9 0 0))

((0 9 2 0 4 0 0 0 5) (0 0 0 0 0 7 3 0 0) (0 4 3 0 0 6 0 8 9) (0 0 0 1
2 0 4 5 0) (0 1 0 4 6 5 0 7 0) (0 5 4 0 7 3 0 0 0) (9 2 0 5 0 0 8 4 0)
(0 0 6 7 0 0 0 0 0) (4 0 0 0 8 0 9 1 0))

((0 4 0 3 0 8 1 0 9) (8 0 0 1 0 5 0 7 0) (0 0 1 0 6 0 5 0 8) (0 0 0 0
1 0 0 8 5) (0 0 0 9 0 4 0 0 0) (4 2 0 0 5 0 0 0 0) (6 0 3 0 7 0 8 0 0)
(0 8 0 2 0 1 0 0 6) (7 0 2 8 0 6 0 9 0))

((2 0 4 0 0 0 0 0 6) (0 3 0 0 1 8 9 0 0) (0 9 7 0 2 0 0 0 3) (5 0 9 7
4 0 0 0 0) (3 4 0 0 6 0 0 1 9) (0 0 0 0 8 9 6 0 4) (4 0 0 0 3 0 8 9 0)
(0 0 1 6 5 0 0 3 0) (7 0 0 0 0 0 4 0 1))

((0 0 7 0 9 8 0 4 0) (8 0 0 7 0 0 9 3 6) (0 9 0 1 0 6 2 0 0) (4 2 0 0
0 0 1 0 0) (0 0 6 0 0 0 7 0 0) (0 0 9 0 0 0 0 8 5) (0 0 8 4 0 1 0 5 0)
(1 3 2 0 0 5 0 0 7) (0 5 0 6 7 0 8 0 0))

((0 0 6 5 4 0 0 9 0) (0 4 0 2 0 0 8 0 0) (0 0 5 0 9 0 2 0 4) (1 2 0 6
0 0 0 8 0) (0 0 9 1 0 2 7 0 0) (0 8 0 0 0 5 0 3 2) (4 0 8 0 1 0 9 0 0)
(0 0 1 0 0 7 0 5 0) (0 6 0 0 5 9 4 0 0))

((7 6 8 0 0 9 0 0 0) (0 1 4 0 0 5 0 7 0) (5 0 0 6 0 0 0 0 4) (1 0 3 0
2 0 0 0 9) (0 0 5 0 4 0 3 0 0) (2 0 0 0 9 0 1 0 7) (3 0 0 0 0 4 0 0 6)
(0 8 0 1 0 0 4 9 0) (0 0 0 3 0 0 7 5 1))

((0 3 0 1 0 2 0 6 0) (0 0 0 8 0 4 7 0 1) (1 5 0 0 0 7 4 0 0) (0 2 0 0
0 8 0 0 5) (7 8 0 0 0 0 0 1 3) (3 0 0 6 0 0 0 8 0) (0 0 6 5 0 0 0 9 2)
(5 0 2 4 0 9 0 0 0) (0 7 0 2 0 6 0 5 0))

((0 5 0 0 0 4 8 1 0) (0 0 7 3 8 0 4 0 2) (0 0 0 0 5 6 0 0 3) (0 0 9 4
0 5 2 0 7) (0 0 0 0 2 0 0 0 0) (4 0 6 7 0 3 1 0 0) (5 0 0 8 4 0 0 0 0)
(7 0 4 0 6 9 5 0 0) (0 1 8 5 0 0 0 9 0))

((2 0 0 0 0 9 0 1 3) (7 5 3 2 1 8 6 0 0) (0 1 0 0 5 0 0 0 0) (0 0 0 0
9 0 0 0 5) (0 9 7 1 0 3 4 8 0) (3 0 0 0 8 0 0 0 0) (0 0 0 0 4 0 0 6 0)
(0 0 8 5 6 1 3 7 4) (6 7 0 9 0 0 0 0 8))

((3 0 8 1 4 0 6 9 0) (0 0 0 0 0 0 0 7 0) (0 0 0 0 8 9 0 0 4) (0 8 3 0
0 5 2 0 0) (6 0 1 2 0 7 9 0 8) (0 0 9 8 0 0 7 4 0) (8 0 0 6 7 0 0 0 0)
(0 2 0 0 0 0 0 0 0) (0 1 6 0 2 3 4 0 7))

((6 0 0 4 9 0 2 0 3) (0 9 0 0 0 0 6 0 0) (0 0 2 7 6 0 0 0 0) (5 1 0 8
0 0 7 0 2) (2 3 0 9 0 5 0 6 1) (4 0 8 0 0 7 0 3 9) (0 0 0 0 8 9 4 0 0)
(0 0 5 0 0 0 0 2 0) (9 0 6 0 5 2 0 0 7))

((0 0 0 5 0 0 6 0 0) (5 0 7 1 4 6 0 0 0) (0 6 0 0 9 0 2 0 4) (7 0 2 3
6 0 0 0 8) (6 0 4 0 0 0 7 0 3) (9 0 0 0 2 7 4 0 1) (8 0 6 0 1 0 0 4 0)
(0 0 0 4 3 8 9 0 6) (0 0 3 0 0 5 0 0 0))

((0 0 0 0 0 3 5 0 7) (4 0 1 0 0 0 9 0 0) (7 2 5 0 9 4 0 0 3) (0 0 0 0
8 9 6 2 0) (8 0 0 3 0 2 0 0 1) (0 9 2 1 6 0 0 0 0) (9 0 0 5 2 0 4 3 6)
(0 0 4 0 0 0 1 0 8) (3 0 8 7 0 0 0 0 0))

((0 0 0 0 8 0 0 0 0) (1 0 0 2 0 3 0 0 8) (2 0 5 6 0 4 7 0 9) (0 1 8 3
0 2 0 6 0) (0 0 4 0 6 0 2 0 0) (0 2 0 1 0 8 3 9 0) (5 0 7 4 0 6 8 0 2)
(6 0 0 8 0 5 0 0 4) (0 0 0 0 9 0 0 0 0))

((0 5 3 1 8 0 6 0 0) (0 7 0 0 6 3 0 0 5) (0 0 0 0 0 0 0 4 1) (8 0 7 0
0 4 1 5 2) (0 0 0 0 0 0 0 0 0) (2 6 1 8 0 0 4 0 9) (6 2 0 0 0 0 0 0 0)
(7 0 0 5 3 0 0 1 0) (0 0 4 0 7 8 5 9 0))

((4 0 8 3 0 0 5 0 0) (0 0 3 1 6 5 4 0 2) (0 5 0 0 0 8 9 0 0) (0 1 6 0
4 2 0 0 0) (0 0 9 0 0 0 7 0 0) (0 0 0 9 3 0 6 2 0) (0 0 5 2 0 0 0 7 0)
(9 0 4 8 1 7 2 0 0) (0 0 1 0 0 4 8 0 3))

((3 0 6 0 0 0 7 0 9) (0 0 0 7 0 9 0 0 0) (0 0 8 0 1 2 6 0 0) (2 0 1 9
0 5 3 0 7) (0 4 0 0 3 0 0 2 0) (6 0 3 2 0 7 1 0 5) (0 0 2 5 7 0 8 0 0)
(0 0 0 4 0 3 0 0 0) (1 0 7 0 0 0 4 0 2))

((0 0 3 8 9 0 0 4 5) (0 4 0 0 0 0 0 0 0) (0 0 2 3 4 1 0 7 0) (2 0 6 5
0 0 9 0 0) (0 1 9 4 0 8 5 6 0) (0 0 4 0 0 3 7 0 2) (0 9 0 7 5 6 4 0 0)
(0 0 0 0 0 0 0 9 0) (4 8 0 0 3 9 1 0 0))

((0 1 0 7 0 9 0 0 8) (6 0 0 3 8 0 0 2 0) (4 8 7 0 0 2 3 0 0) (0 9 0 0
0 0 6 0 1) (0 2 0 0 0 0 0 7 0) (3 0 5 0 0 0 0 8 0) (0 0 2 5 0 0 9 1 4)
(0 3 0 0 2 7 0 0 5) (5 0 0 9 0 6 0 3 0))

((0 5 7 0 0 0 1 0 0) (0 3 0 5 1 4 7 0 0) (2 0 6 0 0 3 0 0 4) (0 0 3 0
9 0 0 0 5) (8 9 0 0 0 0 0 4 7) (5 0 0 0 4 0 2 0 0) (6 0 0 4 0 0 5 0 1)
(0 0 5 6 8 9 0 2 0) (0 0 4 0 0 0 8 9 0))

((2 0 3 8 5 0 0 0 0) (0 5 0 0 7 0 0 6 0) (7 8 0 0 0 9 3 2 5) (1 0 0 7
6 0 2 0 0) (0 0 2 0 0 0 1 0 0) (0 0 5 0 3 2 0 0 9) (5 3 6 4 0 0 0 8 2)
(0 1 0 0 2 0 0 3 0) (0 0 0 0 8 6 5 0 1))

((0 0 8 1 0 0 0 2 7) (9 0 0 5 0 2 0 0 6) (0 2 1 8 0 3 0 0 0) (0 7 0 3
0 0 0 0 5) (2 6 0 0 0 0 0 1 3) (3 0 0 0 0 9 0 6 0) (0 0 0 4 0 8 5 7 0)
(7 0 0 9 0 5 0 0 1) (4 5 0 0 0 7 9 0 0))

((0 5 0 0 1 0 0 0 0) (3 1 8 7 5 2 9 0 0) (7 0 0 0 0 4 0 5 8) (0 0 0 0
4 0 0 0 1) (0 4 3 5 0 8 6 2 0) (8 0 0 0 2 0 0 0 0) (9 3 0 4 0 0 0 0 2)
(0 0 2 1 9 5 8 3 6) (0 0 0 0 6 0 0 9 0))

((4 0 0 0 1 2 6 0 0) (0 2 1 0 5 9 0 7 0) (9 0 0 8 0 0 0 0 2) (0 0 2 9
0 0 0 4 0) (0 6 8 0 4 0 1 3 0) (0 4 0 0 0 6 8 0 0) (3 0 0 0 0 4 0 0 1)
(0 9 0 3 7 0 2 6 0) (0 0 4 6 9 0 0 0 3))

((7 1 8 0 9 6 0 3 0) (3 9 5 0 0 0 6 0 0) (0 0 6 0 0 1 9 0 0) (0 3 0 8
6 0 0 0 0) (0 0 0 7 0 2 0 0 0) (0 0 0 0 4 5 0 6 0) (0 0 7 9 0 0 2 0 0)
(0 0 9 0 0 0 7 1 4) (0 4 0 1 7 0 5 9 6))

((2 5 0 1 7 0 0 0 0) (7 0 0 0 0 4 9 0 5) (0 3 4 0 0 9 1 0 7) (0 4 0 0
0 2 0 7 1) (6 0 0 0 0 0 0 0 3) (9 2 0 6 0 0 0 4 0) (4 0 3 9 0 0 8 6 0)
(5 0 6 7 0 0 0 0 4) (0 0 0 0 4 6 0 5 9))

((7 0 5 6 0 0 0 4 2) (0 0 0 7 0 0 6 0 0) (0 0 3 0 0 4 0 7 5) (0 3 0 0
0 0 1 5 8) (0 1 6 0 0 0 4 2 0) (4 5 7 0 0 0 0 6 0) (6 2 0 9 0 0 5 0 0)
(0 0 9 0 0 8 0 0 0) (5 8 0 0 0 6 7 0 9))

((7 0 0 0 0 6 0 0 0) (0 2 5 0 0 7 6 0 3) (0 6 3 2 0 0 0 0 9) (1 3 4 0
0 0 0 9 0) (2 5 0 0 0 0 0 1 7) (0 7 0 0 0 0 2 3 6) (3 0 0 0 0 8 7 5 0)
(6 0 8 7 0 0 3 4 0) (0 0 0 4 0 0 0 0 8))

((0 0 2 0 6 7 0 8 0) (0 3 0 9 1 0 0 2 0) (8 0 0 0 0 0 1 0 6) (0 5 8 0
0 2 0 0 9) (0 4 6 0 9 0 5 3 0) (7 0 0 5 0 0 2 1 0) (5 0 9 0 0 0 0 0 1)
(0 8 0 0 2 9 0 5 0) (0 6 0 8 7 0 9 0 0))

((3 0 0 2 5 0 1 0 4) (0 2 0 0 0 9 0 6 0) (0 0 9 4 3 0 0 2 0) (9 0 0 0
0 4 8 0 0) (4 0 8 0 9 0 6 0 2) (0 0 1 3 0 0 0 0 9) (0 9 0 0 6 1 4 0 0)
(0 3 0 8 0 0 0 1 0) (1 0 6 0 7 3 0 0 5))

((0 0 0 0 7 0 6 0 0) (6 0 0 8 0 3 2 1 0) (4 0 0 1 0 2 0 0 0) (2 0 5 3
0 0 0 4 0) (0 4 6 0 8 0 5 3 0) (0 8 0 0 0 5 1 0 6) (0 0 0 9 0 8 0 0 1)
(0 9 3 4 0 1 0 0 7) (0 0 4 0 5 0 0 0 0))

((6 7 0 2 0 0 8 0 5) (0 0 0 0 7 8 3 2 0) (8 3 0 4 0 0 0 7 0) (0 2 9 8
0 0 7 0 0) (0 8 0 0 0 0 0 6 0) (0 0 7 0 0 9 4 1 0) (0 4 0 0 0 7 0 3 2)
(0 9 3 1 4 0 0 0 0) (7 0 6 0 0 2 0 4 1))

((3 7 0 0 9 8 0 0 4) (4 0 9 0 5 7 0 0 1) (0 1 0 3 0 0 7 0 0) (0 0 0 0
2 5 9 8 0) (0 0 0 0 0 0 0 0 0) (0 2 7 9 8 0 0 0 0) (0 0 6 0 0 1 0 4 0)
(8 0 0 2 6 0 1 0 7) (7 0 0 8 3 0 0 5 2))

((6 8 0 7 0 0 9 0 5) (0 0 9 4 0 0 0 0 0) (0 1 0 0 5 0 3 0 8) (1 0 5 0
3 2 0 0 0) (4 0 0 1 7 5 0 0 2) (0 0 0 9 4 0 5 0 1) (2 0 8 0 6 0 0 5 0)
(0 0 0 0 0 4 7 0 0) (5 0 6 0 0 1 0 8 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'sudoku)

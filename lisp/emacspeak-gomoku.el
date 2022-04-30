;;; emacspeak-gomoku.el --- Speech enable the game of Gomoku  -*- lexical-binding: t; -*-
;;
;; $Author: tv.raman.tv $ 
;; Description: Auditory interface to gomoku
;; Keywords: Emacspeak, Speak, Spoken Output, gomoku
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

;; Copyright (c) 1995 -- 2021, T. V. Raman
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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Required modules

(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'gomoku)
;;}}}
;;{{{  Introduction 
;;; Commentary:
;; Auditory interface to gomoku
;;; Code:
;;}}}
;;{{{ helper functions

(defun gomoku-point-x ()
  (gomoku-index-to-x (gomoku-point-square)))
(defun emacspeak-gomoku-cell-value (row column)
  (cl-declare (special gomoku-board))
  (aref  gomoku-board
         (gomoku-xy-to-index column row)))

;;}}}
;;{{{ Communicate state
(defun emacspeak-gomoku-goto-x-y (x y)
  "Prompt for and go to that square."
  (interactive
   (list
    (read-number "Row: ")
    (read-number "Column: ")))
  (gomoku-goto-xy x y)
  (emacspeak-auditory-icon 'large-movement)
  (emacspeak-gomoku-speak-square))

(defun emacspeak-gomoku-speak-square ()
  "Speak coordinates and state of square at point"
  (interactive)
  (dtk-speak
   (format "%s %s %s"
           (gomoku-point-y)
           (gomoku-point-x)
           (cl-case (char-after (point))
             (?X "x")
             (?. "-")
             (?O "0")))))

(defun emacspeak-gomoku-show-current-row ()
  "Aurally display current row"
  (interactive)
  (cl-declare (special gomoku-board-width))
  (let ((row (gomoku-point-y))
        (values nil))
    (setq values
          (cl-loop for i from 1 to gomoku-board-width
                   collect 
                   (cl-case (emacspeak-gomoku-cell-value row i)
                     (0 "-")
                     (1  "x")
                     (6 "0"))))
    (dtk-speak
     (apply 'concat values))))

(defun emacspeak-gomoku-show-current-column ()
  "Aurally display current column"
  (interactive)
  (cl-declare (special gomoku-board-height))
  (let ((column (gomoku-point-x))
        (values nil))
    (setq values
          (cl-loop for i from 1 to gomoku-board-height
                   collect 
                   (cl-case (emacspeak-gomoku-cell-value i column)
                     (0 "-")
                     (1  "x")
                     (6 "0"))))
    (dtk-speak
     (apply 'concat values))))

(defun emacspeak-gomoku-show-current-positive-diagonal ()
  "Aurally display current positively sloped diagonal"
  (interactive)
  (cl-declare (special gomoku-board-height
                       gomoku-board-width))
  (let ((row (gomoku-point-y))
        (column (gomoku-point-x))
        (diag-start-x nil)
        (diag-start-y nil)
        (values nil))
    (cond
     ((= row column)
      (setq  diag-start-x 1
             diag-start-y 1))
     ((< row column)
      (setq diag-start-y  1
            diag-start-x (+ 1 (- column row))))
     ((> row column)
      (setq diag-start-x  1
            diag-start-y (+ 1 (- row column)))))
    (setq values
          (cl-loop for i from diag-start-y  to gomoku-board-height
                   and j from diag-start-x to gomoku-board-width 
                   collect
                   (cl-case (emacspeak-gomoku-cell-value i j)
                     (0 "-")
                     (1  "x")
                     (6 "0"))))
    (dtk-speak
     (apply 'concat values))))

(defun emacspeak-gomoku-show-current-negative-diagonal ()
  "Aurally display current negative sloped diagonal "
  (interactive)
  (cl-declare (special gomoku-board-height
                       gomoku-board-width))
  (let ((row (gomoku-point-y))
        (column (gomoku-point-x))
        (diag-start-x nil)
        (diag-start-y nil)
        (square-size (min gomoku-board-width gomoku-board-height))
        (values nil))
    (cond
     ((=  (+ row  column) (+ 1 square-size)) ;on major diag
      (setq  diag-start-x   square-size
             diag-start-y 1))
     ((<  (+ row  column) (+ 1 square-size)) ; above major diag 
      (setq diag-start-y  1
            diag-start-x (- (+ column row) 1)))
     ((<  (+ 1 square-size) (+ row  column)) ; below major diag 
      (setq diag-start-x   (min gomoku-board-width
                                (- (+ row column) 1)))
      (setq diag-start-y
            (- (+ row column) diag-start-x))))
    (setq values
          (cl-loop for i from diag-start-y  to gomoku-board-height
                   and j downfrom   diag-start-x  to 1 
                   collect
                   (cl-case (emacspeak-gomoku-cell-value i j)
                     (0 "-")
                     (1  "x")
                     (6 "0"))))
    (dtk-speak
     (apply 'concat values))))

(defun emacspeak-gomoku-display-statistics ()
  "Display statistics from previous games"
  (interactive)
  (cl-declare (special gomoku-number-of-human-wins
                       gomoku-number-of-emacs-wins
                       gomoku-number-of-draws))
  (message (format "Wins %d losses %d%s"
                   gomoku-number-of-human-wins
                   gomoku-number-of-emacs-wins
                   (if (zerop gomoku-number-of-draws)
                       ""
                     (format " draws %d" gomoku-number-of-draws)))))

(defun emacspeak-gomoku-speak-emacs-previous-move ()
  "Speak emacs' previous move"
  (interactive)
  (cl-declare (special gomoku-game-history))
  (let ((square (car (cl-first gomoku-game-history))))
    (message "I last played on square %s %s"
             (gomoku-index-to-y square)
             (gomoku-index-to-x square))))

(defun emacspeak-gomoku-speak-humans-previous-move ()
  "Speak human' previous move"
  (interactive)
  (cl-declare (special gomoku-game-history))
  (let ((square (car (cl-second gomoku-game-history))))
    (message "You last played on square %s %s"
             (gomoku-index-to-y square)
             (gomoku-index-to-x square))))

(defun emacspeak-gomoku-speak-number-of-moves ()
  "Speak number of moves so far"
  (interactive)
  (cl-declare (special gomoku-number-of-moves))
  (message "%s moves in this game"
           gomoku-number-of-moves))

;;}}}
;;{{{  additional interactive commands.

(defun gomoku-move-left (&optional arg)
  "Move left on the Gomoku board"
  (interactive "p")
  (backward-char arg))

(defun gomoku-move-right (&optional arg)
  "Move right on the Gomoku board"
  (interactive "p")
  (forward-char arg))

;;}}}
;;{{{ Advice

;;{{{  advice all navigation

(cl-loop
 for f in
 '(gomoku-beginning-of-line gomoku-end-of-line
                            gomoku-move-down gomoku-move-up gomoku-move-left gomoku-move-right 
                            gomoku-move-ne gomoku-move-nw gomoku-move-se gomoku-move-sw)
 do
 (eval
  `(defadvice ,f  (after emacspeak pre act comp)
     "speak"
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'select-object)
       (emacspeak-gomoku-speak-square)))))

;;}}}

(defadvice gomoku-emacs-plays (after emacspeak pre act comp)
  "Tell me where you played"
  (emacspeak-auditory-icon 'mark-object)
  (emacspeak-gomoku-speak-square))

(defadvice gomoku-terminate-game (around emacspeak pre act comp)
  "speak"
  (cl-declare (special emacspeak-last-message
                       gomoku-number-of-moves))
  (let((result (ad-get-arg 0)))
    ad-do-it
    (dtk-speak
     (format "%s in %s moves  %s "
             result
             gomoku-number-of-moves
             emacspeak-last-message))
    (sit-for 2))
  ad-return-value)

(defadvice gomoku (after emacspeak pre act comp)
  "Speech enable gomoku"
  (when (ems-interactive-p)
    (emacspeak-gomoku-setup-keys)))
;;}}}
;;{{{ keybindings

(defun emacspeak-gomoku-setup-keys ()
  "Add additional keybindings"
  (cl-declare (special gomoku-mode-map))
  (cl-loop for key in (where-is-internal 'backward-char (list gomoku-mode-map))
           do
           (define-key gomoku-mode-map key 'gomoku-move-left))
  (cl-loop for key in (where-is-internal 'forward-char (list gomoku-mode-map))
           do
           (define-key gomoku-mode-map key 'gomoku-move-right))
  (define-key gomoku-mode-map "\t"
              'emacspeak-gomoku-speak-emacs-previous-move)
  (define-key gomoku-mode-map "\M-\t"
              'emacspeak-gomoku-speak-humans-previous-move) 
  (define-key  gomoku-mode-map "f" 'emacspeak-gomoku-goto-x-y)
  ;; my navigational preference
  (define-key gomoku-mode-map "q" 'gomoku-move-nw)
  (define-key gomoku-mode-map "e" 'gomoku-move-up)
  (define-key gomoku-mode-map "t" 'gomoku-move-ne)
  (define-key gomoku-mode-map "a" 'gomoku-move-left)
  (define-key gomoku-mode-map "g" 'gomoku-move-right)
  (define-key gomoku-mode-map "z" 'gomoku-move-sw)
  (define-key gomoku-mode-map "d" 'gomoku-move-down)
  (define-key gomoku-mode-map "v" 'gomoku-move-se)
  (define-key gomoku-mode-map "." 'emacspeak-gomoku-speak-square)
  (define-key gomoku-mode-map "," 'emacspeak-gomoku-display-statistics)
  (define-key gomoku-mode-map '[left] 'gomoku-move-left)
  (define-key gomoku-mode-map '[right] 'gomoku-move-right)
  (define-key gomoku-mode-map "r" 'emacspeak-gomoku-show-current-row)
  (define-key gomoku-mode-map "c" 'emacspeak-gomoku-show-current-column)
  (define-key gomoku-mode-map "\\"
              'emacspeak-gomoku-show-current-positive-diagonal)
  (define-key gomoku-mode-map "/"
              'emacspeak-gomoku-show-current-negative-diagonal)
  (define-key gomoku-mode-map "=" 'emacspeak-gomoku-speak-number-of-moves)
  )

;;}}}

(provide 'emacspeak-gomoku)
;;{{{ end of file 

;; local variables:
;; folded-file: t
;; end: 

;;}}}

;;; pickup.el --- Speech-enable PICKUP  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable PICKUP An Emacs Interface to pickup
;;; Keywords: Emacspeak,  Audio Desktop pickup
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2007, 2019, T. V. Raman
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
;;; MERCHANTABILITY or FITNPICKUP FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; Pickup Sticks game in elisp.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'seq)

;;}}}
;;{{{Functions:
(cl-defstruct pickup
  "Game structure."
  sticks current limit
  move fib-base fibs)

(defun pickup-update (game move &optional prompt)
  "Update game state."
  (cl-assert
   (not (zerop move)) t
   "Cannot pick 0 sticks")
  (cl-assert
   (<= move (pickup-limit game)) t
   "Cannot pick more than %s" (pickup-limit game))
  (setf
   (pickup-move game) move
   (pickup-sticks game) (- (pickup-sticks game) move)
   (pickup-current game) (- (pickup-current game) move)
   (pickup-limit game) (* 2 move))
  (when (zerop (pickup-current game))
    (setf (pickup-current game) (pickup-sticks game)))
  (pickup-update-fib-base game)
  (if prompt
      (message prompt)
    (message "Picked %d, %d left"
             move (pickup-sticks game)))
  (sit-for 1))

(defun pickup-update-fib-base (game)
  "Update fib-base in game."
  (let ( (base 1)
         (current (pickup-current game)))
    (cl-loop
     for f across (pickup-fibs game) do
     (when (< f current) (setq base f)))
    (setf (pickup-fib-base game) base)))

(defun pickup-me (game)
  "Make my move."
  (cond
   ((<= (pickup-sticks game) (pickup-limit game))
    (pickup-update game (pickup-sticks game))
    (message "I pick %s and win!"
             (pickup-move game)))
   ((and
     (> (pickup-current game) 0)
     (<= (pickup-current game) (pickup-limit game)))
    (pickup-update game (pickup-current game)))
   (t ;;; push opponent to closest fib within 3k < n rule:
    (let ((next-move (- (pickup-current game) (pickup-fib-base game))))
      (while
          (or
           (> next-move (pickup-limit game))
           (>= (* 3 next-move) (pickup-current game)))
        (setf (pickup-current game) next-move)
        (pickup-update-fib-base game)
        (setq next-move (- (pickup-current game) (pickup-fib-base game))))
      (pickup-update game next-move)))))

(defun  pickup-you (game)
  "Make your move."
  (cond
   ((zerop (pickup-sticks game))
    (message "You win!"))
   (t
    (pickup-update game
                   (read-number
                    (format "You can pick between 1 and %s sticks:"
                            (pickup-limit game)))))))

(defun pickup-fibonacci (max)
  "Return vector  of Fibonacci numbers upto max."
  (let ((result '(2 1)))
    (while (< (cl-first result) max)
      (push  (+ (cl-first result) (cl-second result)) result))
    (apply 'vector (reverse result))))

(defun pickup-fibonacci-p (game n)
  "Predicate to check if n is a Fibonacci number"
  (seq-find #'(lambda (f) (= f n)) (pickup-fibs game)))

(defun pickup-build (sticks)
  "Build and return  a  pickup game."
  (let ((fibonacci (pickup-fibonacci sticks)))
    (make-pickup
     :sticks sticks
     :current sticks
     :limit (- sticks 1)
     :move 0
     :fibs fibonacci
     :fib-base (aref fibonacci (- (length fibonacci) 2)))))

(defun pickup-play (sticks)
  "Play the pickup sticks game with `sticks' sticks."
  (interactive "nSticks: ")
  (let ((game (pickup-build sticks)))
    (unless (pickup-fibonacci-p game sticks) (pickup-me game))
    (while (> (pickup-sticks game) 0)
      (pickup-you game)
      (pickup-me game))))

;;}}}

(provide 'pickup)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

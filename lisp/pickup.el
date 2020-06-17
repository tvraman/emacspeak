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
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

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

(defun pickup-update (game move)
  "Update game state."
  )

(defun pickup-update-fib-base (game)
  "Update fib-base in game state."
  )

(defun pickup-me (game)
  "Make my move."
  )

(defun  pickup-you (game)
  "Make your move."
  )

(defun pickup-fibonacci (max)
  "Return vector  of Fibonacci numbers upto max."
  (let ((result '(2 1)))
    (while (< (cl-first result) max)
      (push  (+ (cl-first result) (cl-second result)) result))
    (apply 'vector (reverse result))))

(defun pickup-fibonacci-p (fibonacci n)
  "Predicate to check if n is a Fibonacci number"
  (seq-find #'(lambda (f) (= f n)) fibonacci))

(defun pickup-play (sticks)
  "Play the pickup sticks game with `sticks' sticks."
  (interactive "nSticks: ")
  (let* ((fibonacci (pickup-fibonacci sticks))
         (fib-base (elt fibonacci (- (length fibonacci) 2)))
         (game
          (make-pickup
           :sticks sticks
           :current sticks
           :limit (- sticks 1)
           :move 0
           :fibs fibonacci
           :fib-base fib-base)))
    game))

;;}}}
 
(provide 'pickup)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

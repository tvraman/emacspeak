;;; emacspeak-mines.el --- Speech-enable MINES  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable MINES An Emacs Interface to mines
;;; Keywords: Emacspeak,  Audio Desktop mines
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
;;;Copyright (C) 1995 -- 2007, 2011, T. V. Raman
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
;;; MERCHANTABILITY or FITNMINES FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; MINES ==  Minesweeper game in emacs.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Interactive Commands:


(defun emacspeak-mines-init ()
  "Setup additional keys for playing minesweeper."
  (cl-declaim (special mines-mode-map))
  (cl-loop
   for b in
   '(
     ("." emacspeak-mines-speak-neighbors)
     ("SPC" emacspeak-mines-speak-cell))
   do
   (define-key mines-mode-map (kbd (cl-first b)) (cl-second b))))
(eval-after-load  "mines"
`(progn (emacspeak-mines-init)))

(defun emacspeak-mines-speak-cell ()
  "Speak current cell."
  (interactive)
  (let ((pos (mines-index-2-matrix (mines-current-pos))))
    (dtk-speak (format "%c in row %s column  %s"
                       (following-char) (cl-first pos) (cl-second pos)))))


(defun emacspeak-mines-speak-neighbors ()
  "Speak neighboring cells in sorted order."
  (interactive )
  (cl-declare (special mines-state mines-grid))
  (let* ((cells (sort (mines-get-neighbours (mines-current-pos)) #'<))
         (values (mapcar #'(lambda (c) (aref mines-state c)) cells))
         (numbers (mapcar #'(lambda (c) (aref mines-grid c)) cells))
         (result nil))
    (cl-loop
     for v in values
     and n in numbers do
     (cond
      ((null v) (push "." result))
      ((and v (numberp n) ) (push (format "%d" n) result))
      ((eq '@ v) (push "@" result))
      (t (message "Should not  get here"))))
    (dtk-speak-list (nreverse result))))
              
;;}}}
;;{{{ Advice Interactive Commands 
(defadvice mines (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak "Welcome to a new game.")))

(cl-loop
 for f in 
 '(mines-go-down
mines-go-left
mines-go-right
mines-go-up)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'item)
       (emacspeak-mines-speak-cell)))))

(defadvice mines-dig (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (unless mines-game-over (emacspeak-mines-speak-cell))))


(defadvice mines-flag-cell (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    
    (if (aref mines-grid (mines-current-pos))
        (emacspeak-auditory-icon 'close-object)
      (emacspeak-auditory-icon 'mark-object))
    (emacspeak-mines-speak-cell)))

(defadvice mines-game-over (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'shutdown)))


;;}}}
(provide 'emacspeak-mines)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

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
(eval-when-compile (require 'mines "mines" 'no-error))
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
    (dtk-speak
     (format "%c in row %s column  %s"
             (following-char) (cl-first pos) (cl-second pos)))))

(defun emacspeak-mines-cell-flagged-p (c)
  "Predicate to check if cell at index c is flagged."
  (save-excursion
    (mines-goto c)
    (get-text-property (point) 'flag)))

(defun emacspeak-mines-speak-neighbors ()
  "Speak neighboring cells in sorted order."
  (interactive )
  (cl-declare (special mines-state mines-grid))
  (let* ((current (mines-current-pos))
         (cells (sort (mines-get-neighbours current) #'<))
         (pos (mines-index-2-matrix current))
         (row (cl-first pos))
         (column (cl-second pos))
         (count (length cells))
         (values (mapcar #'(lambda (c) (aref mines-state c)) cells))
         (numbers (mapcar #'(lambda (c) (aref mines-grid c)) cells))
         (result nil)
         (group nil))
    (cl-loop
     for c in cells
     for v in values
     and n in numbers do
     (cond
      ((and (null v) (emacspeak-mines-cell-flagged-p c))
       (push "m" result))
      ((null v) (push "dot" result))
      ((and v (numberp n) ) (push (format "%d" n) result))
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
      (t '(3 2 3))))
    (dtk-speak-list (nreverse result) group)))

;;}}}
;;{{{ Advice Interactive Commands
(defadvice mines (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
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

    (if (eq t (aref mines-grid (mines-current-pos)))
        (emacspeak-auditory-icon 'close-object)
      (emacspeak-auditory-icon 'mark-object))
    (emacspeak-mines-speak-cell)))

(defadvice mines-game-over (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'shutdown)))

(defadvice mines-game-completed(after emacspeak pre act comp)
  "Provide an auditory icon."
  (emacspeak-auditory-icon 'task-done))

;;}}}

(provide 'emacspeak-mines)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

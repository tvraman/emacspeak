;;; emacspeak-threes.el --- Speech-enable THREES  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable THREES An Emacs Interface to threes
;;; Keywords: Emacspeak,  Audio Desktop threes
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
;;; MERCHANTABILITY or FITNTHREES FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;;
;;; THREES == threes game. This module speech-enable the
;;; game. @url{https://en.wikipedia.org/wiki/Threes} for history of
;;; the game and details of game play. This module adds additional convenience keybindings to
;;; the default arrow-key bindings implemented in threes.el. In
;;; addition, this module  implements commands that speak the board as well as
;;; getting a column-specific view of the board.
;;;
;;; @table @kbd
;;; @item  f
;;; Move right
;;; @item b
;;; Move left
;;; @item n
;;; Move down
;;; @item p
;;; Move up
;;; @item SPC
;;; Speak the board
;;; @item /
;;; Speak board by column.
;;; @item .
;;; Speak current score.
;;; @item s
;;; Save current state
;;; @item u
;;; Pop state from stack
;;; @item ?
;;; Speak next tile
;;; @end table
;;; The updated board is spoken after each turn.
;;;The next upcoming tile is spoken after the  current state of the board.
;;; You can use @kbd{SPC} and @kbd{/} to review the board.
;;;
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'sox-gen)

;;}}}
;;{{{ Variables:

(defvar emacspeak-threes-rows-max '(0 0 0 0)
  "Max for each row.")

(defun emacspeak-threes-get-rows-max ()
  "Return max for each row."
  (declare (special threes-cells))
  (mapcar #'(lambda (r) (apply #'max   r)) threes-cells))

;;}}}
;;{{{ Helpers:

(loop
 for i in'(1 2 3) do
 (eval
  `(defun  ,(intern  (format "emacspeak-threes-%s" i)) ()
     "Set next tile."
     (interactive)
     (declare (special threes-next-number))
     (setq threes-next-number ,i)
     (emacspeak-threes-speak-board))))

(defun emacspeak-threes-sox-gen (number)
  "Generate a tone  that indicates 1, 2 or 3."
  (let ((fade "fade h .1 .1 "))
    (cond
     ((= 1 number) (sox-sin .1 "E3"fade))
     ((= 2 number) (sox-sin .5 "D3" fade))
     ((= 3 number) (sox-sin .5 "C5"fade)))))

;;}}}
h;;{{{ Advice interactive commands:

(defun emacspeak-threes-speak-board ()
  "Speak the board."
  (interactive)
  (declare (special threes-cells threes-next-number
                    emacspeak-threes-rows-max))
  (emacspeak-threes-sox-gen threes-next-number)
  (let ((cells (copy-sequence threes-cells)))
    (nconc
     cells
     (list (propertize (format "%s" threes-next-number) 'personality voice-bolden)))
    (tts-with-punctuations 'some (dtk-speak-list   cells))
    (emacspeak-auditory-icon 'complete)
    (unless  (equal (emacspeak-threes-get-rows-max) emacspeak-threes-rows-max)
      (emacspeak-auditory-icon 'item))))

(defun emacspeak-threes-speak-next ()
  "Speak upcoming tile."
  (interactive)
  (emacspeak-threes-sox-gen threes-next-number)
  (dtk-speak (format "%s" threes-next-number)))

(defun emacspeak-threes-speak-transposed-board ()
  "Speak the board by columns."
  (interactive)
  (declare (special threes-cells))
  (tts-with-punctuations
   'some
   (dtk-speak-list   (threes-cells-transpose threes-cells) 4))
  (emacspeak-auditory-icon 'progress))

(defun emacspeak-threes-setup ()
  "Set up additional key-bindings."
  (declare (special threes-mode-map))
  (define-key threes-mode-map "1" 'emacspeak-threes-1)
  (define-key threes-mode-map "2" 'emacspeak-threes-2)
  (define-key threes-mode-map "3" 'emacspeak-threes-3)
  (define-key threes-mode-map "#" 'emacspeak-threes-prune-stack)
  (define-key threes-mode-map "e" 'emacspeak-threes-export)
  (define-key threes-mode-map "i" 'emacspeak-threes-import)
  (define-key threes-mode-map "s" 'emacspeak-threes-push-state)
  (define-key threes-mode-map "u" 'emacspeak-threes-pop-state)
  (define-key threes-mode-map "g" 'threes)
  (define-key threes-mode-map " " 'emacspeak-threes-speak-board)
  (define-key threes-mode-map "." 'emacspeak-threes-score)
  (define-key threes-mode-map "/" 'emacspeak-threes-speak-transposed-board)
  (define-key threes-mode-map "?" 'emacspeak-threes-speak-next)
  (define-key threes-mode-map "n" 'threes-down)
  (define-key threes-mode-map "p" 'threes-up)
  (define-key threes-mode-map "f" 'threes-right)
  (define-key threes-mode-map "b" 'threes-left))

(defadvice threes (after emacspeak pre act comp)
  "Provide auditory feedback."
  (random t)
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-threes-speak-board)))

(declare-function threes-cells-score "threes" nil)
(declare-function threes-cells-transpose "threes" (cells))

(defun emacspeak-threes-score ()
  "Speak the score."
  (interactive)
  (message (format "Score: %s" (number-to-string (threes-cells-score)))))

(loop
 for f in
 '(threes-up threes-down threes-left threes-right)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback"
     (when (ems-interactive-p)
       (emacspeak-threes-speak-board)))))

(defadvice threes-check-before-move (before emacspeak pre act comp)
  "Cache max"
  (setq emacspeak-threes-rows-max (emacspeak-threes-get-rows-max)))
(when (boundp 'threes-mode-map)
  (emacspeak-threes-setup))

;;}}}
;;{{{ Push And Pop states:

(defstruct emacspeak-threes-game-state
  board)

(defvar emacspeak-threes-game-stack nil
  "Stack of saved states.")

(defun emacspeak-threes-push-state ()
  "Push current game state on stack."
  (interactive)
  (declare (special emacspeak-threes-game-stack threes-cells))
  (push
   (make-emacspeak-threes-game-state
    :board (copy-sequence threes-cells))
   emacspeak-threes-game-stack)
  (emacspeak-auditory-icon 'mark-object)
  (message "Saved state."))
(declare-function threes-print-board "threes.el" nil)
(defun emacspeak-threes-pop-state ()
  "Reset state from stack."
  (interactive)
  (declare (special emacspeak-threes-game-stack threes-cells
                    threes-game-over-p))
  (cond
   ((null emacspeak-threes-game-stack) (error "No saved  states."))
   (t
    (setq threes-game-over-p nil)
    (let ((state (pop emacspeak-threes-game-stack)))
      (setq threes-cells (emacspeak-threes-game-state-board state))
      (threes-print-board)
      (emacspeak-auditory-icon 'yank-object)
      (message "Popped: Score is now %s" (threes-cells-score))))))

(defun emacspeak-threes-prune-stack (drop)
  "Prune game stack to specified length."
  (interactive
   (list
    (cond
     ((null emacspeak-threes-game-stack) (error "No saved  states."))
     (t (read-number
         (format "Stack: %s New? "
                 (length emacspeak-threes-game-stack))
         (/ (length emacspeak-threes-game-stack) 2))))))
  (declare (special emacspeak-threes-game-stack))
  (setq emacspeak-threes-game-stack
        (butlast emacspeak-threes-game-stack
                 (- (length emacspeak-threes-game-stack) drop)))
  (message "Stack is now %s deep"
           (length emacspeak-threes-game-stack))
  (emacspeak-auditory-icon 'delete-object))

;;}}}
;;; emacspeak-2048.el --- Speech-enable 2048
;;; $Id: emacspeak-2048.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable 2048 An Emacs Interface to 2048
;;; Keywords: Emacspeak,  Audio Desktop 2048
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
;;;Copyright (C) 1995 -- 2015, T. V. Raman
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
;;; MERCHANTABILITY or FITN2048 FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction
;;; Commentary:
;;; Speech-enable 2048 Game

;;; Code:

;;; Commentary:
;;; 2048 ==

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require '2048-game "2048-game" 'no-error)
;;}}}
;;{{{ Push And Pop states:

(defstruct emacspeak-2048-game-state
  board score
  rows cols
  )

(defvar emacspeak-2048-game-stack nil
  "Stack of saved states.")
(defun emacspeak-2048-push-state ()
  "Push current game state on stack."
  (interactive)
  (declare (special emacspeak-2048-game-stack
                    *2048-board* *2048-score* *2048-rows* *2048-columns*))
  (push
   (make-emacspeak-2048-game-state
    :board (copy-sequence *2048-board*)
    :score *2048-score*
    :rows *2048-rows*
    :cols *2048-columns*)
   emacspeak-2048-game-stack)
  (emacspeak-auditory-icon 'mark-object)
  (message "Saved state."))

(defun emacspeak-2048-pop-state ()
  "Reset state from stack."
  (interactive)
  (declare (special emacspeak-2048-game-stack
                    *2048-board* *2048-score* *2048-rows* *2048-columns*))
  (cond
   ((null emacspeak-2048-game-stack) (error "No saved  states."))
   (t
    (let ((state (pop emacspeak-2048-game-stack)))
      (setq
       *2048-board* (emacspeak-2048-game-state-board state)
       *2048-score* (emacspeak-2048-game-state-score state)
       *2048-rows* (emacspeak-2048-game-state-rows state)
       *2048-columns* (emacspeak-2048-game-state-cols state))
      (2048-print-board)
      (emacspeak-auditory-icon 'yank-object)
      (message "Popped: Score is now %s" *2048-score*)))))

(defun emacspeak-2048-prune-stack (drop)
  "Prune game stack to specified length."
  (interactive 
   (list
    (cond
     ((null emacspeak-2048-game-stack) (error "No saved  states."))
     (t (read-number
         (format "Stack: %s New? "
                 (length emacspeak-2048-game-stack))
         (/ (length emacspeak-2048-game-stack) 2))))))
  (declare (special emacspeak-2048-game-stack))
  (setq emacspeak-2048-game-stack
        (butlast emacspeak-2048-game-stack
                 (- (length emacspeak-2048-game-stack) drop)))
  (message "Stack is now %s deep"
           (length emacspeak-2048-game-stack))
  (emacspeak-auditory-icon 'delete-object))

;;}}}
;;{{{ Export And Import Games:

(defvar emacspeak-threes-game-file
  (expand-file-name "threes-game-stack"
                    emacspeak-resource-directory)
  "File where we export/import game state.")

(defun emacspeak-threes-export (&optional prompt)
  "Exports game stack to a file.
Optional interactive prefix arg prompts for a file.
Note that the file is overwritten silently."
  (interactive "P")
  (declare (special emacspeak-threes-game-file emacspeak-threes-game-stack))
  (with-temp-buffer
    (let ((file
           (if prompt
               (read-file-name "File to save game to: ")
             emacspeak-threes-game-file))
          (print-length nil)
          (print-level nil))
      (insert "(setq emacspeak-threes-game-stack \n'")
      (pp emacspeak-threes-game-stack (current-buffer))
      (insert ")\n")
      (write-file file)
      (emacspeak-auditory-icon 'save-object)
      (message "Exported game to %s." file))))

(defun emacspeak-threes-import (&optional prompt)
  "Import game.
Optional interactive prefix arg prompts for a filename."
  (interactive "P")
  (let ((file
         (if prompt
             (read-file-name "File to import game from: ")
           emacspeak-threes-game-file)))
    (load-file file)
    (emacspeak-auditory-icon 'task-done)
    (message "Imported game %s." file)))

;;}}}
(provide 'emacspeak-threes)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

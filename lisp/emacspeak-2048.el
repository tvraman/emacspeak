;;; emacspeak-2048.el --- Speech-enable 2048 Game -*- lexical-binding: t; -*-
;; $Id: emacspeak-2048.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;; $Author: tv.raman.tv $
;; Description:  Speech-enable 2048 An Emacs Interface to 2048
;; Keywords: Emacspeak,  Audio Desktop 2048
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
;; Copyright (C) 1995 -- 2021, T. V. Raman
;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
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
;; MERCHANTABILITY or FITN2048 FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction
;;; Commentary:
;; Speech-enable 2048 Game

;;; Code:

;;; Commentary:
;; 2048 ==

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require '2048-game "2048-game" 'no-error)
;;}}}
;;{{{ Push And Pop states:

(cl-defstruct emacspeak-2048-game-state
  board score
  rows cols
  )

(defvar emacspeak-2048-game-stack nil
  "Stack of saved states.")
(defun emacspeak-2048-push-state ()
  "Push current game state on stack."
  (interactive)
  (cl-declare (special emacspeak-2048-game-stack
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
  (cl-declare (special emacspeak-2048-game-stack
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
  (cl-declare (special emacspeak-2048-game-stack))
  (setq emacspeak-2048-game-stack
        (butlast emacspeak-2048-game-stack
                 (- (length emacspeak-2048-game-stack) drop)))
  (message "Stack is now %s deep"
           (length emacspeak-2048-game-stack))
  (emacspeak-auditory-icon 'delete-object))

;;}}}
;;{{{ Export And Import Games:

(defvar emacspeak-2048-game-file
  (expand-file-name "2048-game-stack"
                    emacspeak-user-directory)
  "File where we export/import game state.")

(defun emacspeak-2048-export (&optional prompt)
  "Exports game stack to a file.
Optional interactive prefix arg prompts for a file.
Note that the file is overwritten silently."
  (interactive "P")
  (cl-declare (special emacspeak-2048-game-file emacspeak-2048-game-stack))
  (with-temp-buffer
    (let ((file
           (if prompt
               (read-file-name "File to save game to: ")
             emacspeak-2048-game-file))
          (print-length nil)
          (print-level nil))
      (insert "(setq emacspeak-2048-game-stack \n'")
      (pp emacspeak-2048-game-stack (current-buffer))
      (insert ")\n")
      (write-file file)
      (emacspeak-auditory-icon 'save-object)
      (message "Exported game to %s." file))))

(defun emacspeak-2048-import (&optional prompt)
  "Import game.
Optional interactive prefix arg prompts for a filename."
  (interactive "P")
  (let ((file
         (if prompt
             (read-file-name "File to import game from: ")
           emacspeak-2048-game-file)))
    (load-file file)
    (cl-loop
     for i in
     '(4096 8192 16384 32768 65536 131072) do
     (2048-init-tile i))
    (emacspeak-auditory-icon 'task-done)
    (message "Imported game %s." file)))

;;}}}
;;{{{ Adding rows and columns:

(defun emacspeak-2048-add-row ()
  "Add a row  to the current board."
  (interactive)
  (cl-declare (special *2048-board* *2048-rows*))
  (setq *2048-rows* (cl-incf *2048-rows*))
  (let ((board (copy-sequence *2048-board*)))
    (setq *2048-board* (make-vector (* *2048-columns* *2048-rows*) 0))
    (cl-loop
     for   i from 0 to (1- (length board)) do
     (aset  *2048-board* i  (aref board i))
     (2048-print-board))
    (message "Added row.")))

(defun emacspeak-2048-drop-row ()
  "Drop last  row  from  the current board."
  (interactive)
  (cl-declare (special *2048-board* *2048-rows*))
  (setq *2048-rows* (1- *2048-rows*))
  (let ((board (copy-sequence *2048-board*)))
    (setq *2048-board* (make-vector (* *2048-columns* *2048-rows*) 0))
    (cl-loop
     for   i from 0 to (1- (length *2048-board*)) do
     (aset  *2048-board* i  (aref board i))
     (2048-print-board))
    (emacspeak-auditory-icon 'delete-object)
    (message "Dropped row.")))

(defun emacspeak-2048-add-column ()
  "Add a column  to the current board."
  (interactive)
  (cl-declare (special *2048-board* *2048-columns*))
  (let ((board (copy-sequence *2048-board*))
        (index 0)
        (cols *2048-columns*))
    (setq *2048-columns* (cl-incf *2048-columns*))
    (setq *2048-board* (make-vector (* *2048-columns* *2048-rows*) 0))
    (cl-loop
     for r from 0 to (1- *2048-rows*) do
     (cl-loop
      for c from 0 to (1- cols) do
      (setq index (+ (* r cols) c))     ; old  board
      (aset *2048-board*
            (+ r index)
            (aref board index)))
     (message "Added column."))))

(defun emacspeak-2048-drop-column ()
  "Drop last  row  from  the current board."
  (interactive)
  (cl-declare (special *2048-board* *2048-columns* *2048-columns*))
  (let ((board (copy-sequence *2048-board*))
        (bound 0))
    (setq *2048-columns* (1- *2048-columns*))
    (setq *2048-board* (make-vector (* *2048-columns* *2048-rows*) 0))
    (cl-loop
     for   i from 0 to (1- (length *2048-board*)) do
     (cond
      ((= bound *2048-columns*) (setq bound 0))
      (t
       (cl-incf bound)
       (aset  *2048-board* i  (aref board i)))))
    (2048-print-board))
  (emacspeak-auditory-icon 'delete-object)
  (message "Dropped column."))
(defun emacspeak-2048-board-reset ()
  "Reset board to default size."
  (cl-declare (special *2048-rows* *2048-columns* *2048-board*))
  (setq *2048-rows* 4
        *2048-columns* 4))

;;}}}
;;{{{ Advice commands, bind one review command

(defun emacspeak-2048-speak-board ()
  "Speak board."
  (interactive)
  (cl-declare (special *2048-board* *2048-columns*))
  (dtk-speak-list (append *2048-board* nil) *2048-columns*))

(defun emacspeak-2048-speak-transposed-board ()
  "Speak board column-wise."
  (interactive)
  (cl-declare (special *2048-board*      *2048-columns* *2048-rows*))
  (dtk-speak-list
   (cl-loop for col from 0 to (- *2048-columns*  1)
            collect
            (cl-loop for row from 0 to (- *2048-rows*  1)
                     collect
                     (aref  *2048-board*  (+ col (* 4 row)))))
   *2048-rows*))

(cl-loop
 for f in
 '(2048-left 2048-right 2048-down 2048-up)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak"
     (when (ems-interactive-p)
       (cond
        ((cl-some #'identity *2048-combines-this-move*)
         (emacspeak-auditory-icon 'item))
        (t (emacspeak-auditory-icon 'close-object)))
       (emacspeak-2048-speak-board)
       (cond
        ((2048-game-was-won) (emacspeak-auditory-icon 'task-done))
        ((2048-game-was-lost) (emacspeak-auditory-icon 'alarm)))))))
(defadvice 2048-insert-random-cell (after emacspeak pre act comp)
  "Provide auditory icon"
  (emacspeak-auditory-icon 'item))

(defun emacspeak-2048-score ()
  "Show total on board."
  (interactive)
  (cl-declare (special *2048-score*))
  (message (format "Score: %d" *2048-score*)))

;;}}}
;;{{{ Setup
(declare-function emacspeak-pronounce-define-local-pronunciation "emacspeak-pronounce" (word pron))

(defun emacspeak-2048-setup ()
  "Emacspeak setup for 2048."
  (cl-declaim (special  2048-mode-map))
  (voice-lock-mode -1)
  (define-key 2048-mode-map "#" 'emacspeak-2048-prune-stack)
  (define-key 2048-mode-map "D" 'emacspeak-2048-drop-row)
  (define-key 2048-mode-map "d" 'emacspeak-2048-drop-column)
  (define-key 2048-mode-map "P" 'emacspeak-2048-prune-stack)
  (define-key 2048-mode-map "R" 'emacspeak-2048-add-row)
  (define-key 2048-mode-map "C" 'emacspeak-2048-add-column)
  (define-key 2048-mode-map "e" 'emacspeak-2048-export)
  (define-key 2048-mode-map "i" 'emacspeak-2048-import)
  (define-key 2048-mode-map " " 'emacspeak-2048-speak-board)
  (define-key 2048-mode-map "s" 'emacspeak-2048-push-state)
  (define-key 2048-mode-map "u"  'emacspeak-2048-pop-state)
  (define-key 2048-mode-map [delete]  'emacspeak-2048-pop-state)
  (define-key 2048-mode-map "/" 'emacspeak-2048-speak-transposed-board)
  (define-key 2048-mode-map  "="'emacspeak-2048-score)
  (define-key 2048-mode-map  "r"'emacspeak-2048-randomize-game)
  (define-key 2048-mode-map  (ems-kbd "C-SPC") 'emacspeak-2048-score)
  (define-key 2048-mode-map "g" '2048-game)
  (dtk-set-rate
   (+ dtk-speech-rate-base
      (* dtk-speech-rate-step  3)))
  (dtk-set-punctuations 'some)
  (emacspeak-auditory-icon 'open-object)
  (emacspeak-pronounce-define-local-pronunciation "0" "o")
  (emacspeak-2048-speak-board))
(cl-declaim (special-display-p 2048-mode-hook))
(add-hook '2048-mode-hook 'emacspeak-2048-setup)
;;}}}
;;{{{ Randomize game

(defun emacspeak-2048-randomize-game (&optional count)
  "Puts game in a randomized new state."
  (interactive "nCount: ")
  (cl-declare (special *2048-board*))
  (cl-loop
   for i from 0 to 15 do
   (cond
    ((< i  count)
     (aset *2048-board* i
           (lsh 2 (random (random count)))))
    (t (aset *2048-board* i 0))))
  (emacspeak-2048-speak-board))

;;}}}
(provide 'emacspeak-2048)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}

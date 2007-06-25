;;; emacspeak-solitaire.el --- Speech enable Solitaire game
;;; $Id$
;;; $Author: tv.raman.tv $ 
;;; Description: Auditory interface to solitaire
;;; Keywords: Emacspeak, Speak, Spoken Output, solitaire
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

;;; Copyright (c) 1995 -- 2007, T. V. Raman
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

;;{{{  Required modules

(require 'emacspeak-preamble)
(require 'solitaire)
;;}}}
;;{{{  Introduction 

;;; Auditory interface to solitaire

;;}}}
;;{{{  Communicate state

(defsubst emacspeak-solitaire-current-row ()
  (declare (special solitaire-start-y))
  (+ 1 (/ 
        (- (solitaire-current-line)
           solitaire-start-y)
        2)))

(defsubst emacspeak-solitaire-current-column()
  (declare (special solitaire-start-x))
  (let ((c (current-column)))
    (+ 1
       (/ (- c solitaire-start-x)
          4))))

(defun emacspeak-solitaire-speak-coordinates ()
  "Speak coordinates of current position"
  (interactive)
  (dtk-speak
   (format "%s at %s %s "
           (case(char-after (point))
             (?o "stone")
             (?. "hole"))
           (emacspeak-solitaire-current-row)
           (emacspeak-solitaire-current-column))))

(defsubst emacspeak-solitaire-stone  () (dtk-tone 400 50 ))

(defsubst emacspeak-solitaire-hole () (dtk-tone 800 50 ))

(defun emacspeak-solitaire-show-row ()
  "Display current row auditorallly"
  (interactive)
  (save-excursion
    (beginning-of-line )
    (skip-syntax-forward " ")
    (let ((row (emacspeak-solitaire-current-row))
          (count 1))
      (while (not (eolp))
        (case (char-after (point))
          (?o (emacspeak-solitaire-stone))
          (?. (emacspeak-solitaire-hole)))
        (incf count)
        (when (and (>= row 3)
                   (<= row 5)
                   (= 0 (% count 3)))
          (dtk-silence 1))
        (forward-char 1))
      (skip-syntax-forward " "))
    (dtk-force)))

(defun emacspeak-solitaire-show-column ()
  "Display current row auditorallly"
  (interactive)
  (save-excursion
    (let ((row (emacspeak-solitaire-current-row))
          (column (emacspeak-solitaire-current-column)))
      (loop for i  from 1 to(- row 1)
            do
            (solitaire-up))
      (case (char-after (point))
        (?o (emacspeak-solitaire-stone))
        (?. (emacspeak-solitaire-hole)))
      (cond
       ((and (>= column 3)
             (<= column 5))
        (loop for count from 2 to 7 
              do
              (when  (= count 3) (dtk-silence 10))
              (when (= count 6) (dtk-silence 10))
              (solitaire-down)
              (case (char-after (point))
                (?o (emacspeak-solitaire-stone))
                (?. (emacspeak-solitaire-hole)))))
       (t (loop for count from 2 to 3
                do
                (solitaire-down)
                (case (char-after (point))
                  (?o (emacspeak-solitaire-stone))
                  (?. (emacspeak-solitaire-hole)))))))
    (dtk-force)))

;;}}}
;;{{{ advice commands

;;}}}
;;{{{ advice commands

(defvar emacspeak-solitaire-autoshow nil
  "*T means rows and columns are toned as we move")

(defadvice solitaire-left (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (let ((dtk-stop-immediately nil))
      (emacspeak-auditory-icon 'select-object)
      (and emacspeak-solitaire-autoshow (emacspeak-solitaire-show-column))
      (emacspeak-solitaire-speak-coordinates))))

(defadvice solitaire-right (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (let ((dtk-stop-immediately nil))
      (emacspeak-auditory-icon 'select-object)
      (and emacspeak-solitaire-autoshow  (emacspeak-solitaire-show-column))
      (emacspeak-solitaire-speak-coordinates))))

(defadvice solitaire-up (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (let ((dtk-stop-immediately nil))
      (emacspeak-auditory-icon 'select-object)
      (and emacspeak-solitaire-autoshow (emacspeak-solitaire-show-row))
      (emacspeak-solitaire-speak-coordinates))))

(defadvice solitaire-down (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (let ((dtk-stop-immediately nil))
      (emacspeak-auditory-icon 'select-object)
      (and emacspeak-solitaire-autoshow (emacspeak-solitaire-show-row))
      (emacspeak-solitaire-speak-coordinates))))

(defadvice solitaire-center-point (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-solitaire-speak-coordinates)))

(defadvice solitaire-move (after emacspeak pre act comp)
  "Provide auditory feedback"
  (emacspeak-auditory-icon 'close-object)
  (emacspeak-solitaire-speak-coordinates))

(defadvice solitaire-do-check (after emacspeak pre act comp)
  "Provide enhanced feedback"
  (dtk-speak
   (format "%s stones left: %s"
           solitaire-stones ad-return-value)))

(defadvice solitaire (after emacspeak pre act comp)
  "Emacspeak provides an auditory interface to the solitaire game.
As you move you hear the coordinates and state of the current cell.
Moving a stone produces an auditory icon.
You can examine the state of the board by using
`r' and `c' to listen to the row and column respectively.
Emacspeak produces tones to indicate the state --a higher pitched beep
indicates a hole.
Rows and columns are displayed aurally by
grouping the tones to provide structure.
Emacspeak specific commands:
               \\[emacspeak-solitaire-show-column] emacspeak-solitaire-show-column
\\[emacspeak-solitaire-show-row]                emacspeak-solitaire-show-row
               \\[emacspeak-solitaire-speak-coordinates]  emacspeak-solitaire-speak-coordinates"
  (when (interactive-p)
    (delete-other-windows)
    (emacspeak-auditory-icon 'alarm)
    (emacspeak-solitaire-setup-keymap)
    (emacspeak-solitaire-speak-coordinates)))

(defadvice solitaire-quit (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{  add keybindings

(defun emacspeak-solitaire-setup-keymap ()
  "Setup emacspeak keybindings for solitaire"
  (declare (special solitaire-mode-map))
  (define-key solitaire-mode-map "." 'emacspeak-solitaire-speak-coordinates)
  (define-key solitaire-mode-map "r" 'emacspeak-solitaire-show-row)
  (define-key solitaire-mode-map "c" 'emacspeak-solitaire-show-column)
  (define-key solitaire-mode-map "f" 'solitaire-move-right)
  (define-key solitaire-mode-map "b" 'solitaire-move-left)
  (define-key solitaire-mode-map "p" 'solitaire-move-up)
  (define-key solitaire-mode-map "n" 'solitaire-move-down)
  (define-key solitaire-mode-map "l" 'solitaire-right)
  (define-key solitaire-mode-map "h" 'solitaire-left)
  (define-key solitaire-mode-map "k" 'solitaire-up)
  (define-key solitaire-mode-map "j" 'solitaire-down))

;;}}}

(provide 'emacspeak-solitaire )
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

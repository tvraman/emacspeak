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

;;}}}
;;{{{ Advice commands, bind one review command

(defun emacspeak-2048-speak-board ()
  "Speak board."
  (interactive)
  (declare (special *2048-board*))
  (dtk-speak-list (append *2048-board* nil ) 4))

(defun emacspeak-2048-speak-transposed-board ()
  "Speak board column-wise."
  (interactive)
  (declare (special *2048-board*
                    *2048-columns* *2048-rows*))
  (dtk-speak-list
   (loop for col from 0 to (- *2048-columns*  1)
      collect 
      (loop for row from 0 to (- *2048-rows*  1)
            collect
            (aref  *2048-board*  (+ col (* 4 row)))))
   4))

(loop 
 for f in
 '(2048-left 2048-right 2048-down 2048-up)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide spoken feedback"
     (when (ems-interactive-p)
       (cond
        ((some #'identity *2048-combines-this-move*) (emacspeak-auditory-icon 'item))
       (t (emacspeak-auditory-icon 'close-object)))
       (emacspeak-2048-speak-board)
       (cond
        ((2048-game-was-won) (emacspeak-auditory-icon 'task-done))
        ((2048-game-was-lost) (emacspeak-auditory-icon 'alarm)))))))



(defun emacspeak-2048-score ()
  "Show total on board."
  (interactive)
  (declare (special *2048-score*))
  (message (format "Score: %d" *2048-score*)))

;;}}}
;;{{{ Setup

(defun emacspeak-2048-setup ()
  "Emacspeak setup for 2048."
  (declaim (special  2048-mode-map))
  (define-key 2048-mode-map " " 'emacspeak-2048-speak-board)
  (define-key 2048-mode-map "/" 'emacspeak-2048-speak-transposed-board)
  (define-key 2048-mode-map "\C- " 'emacspeak-2048-score)
  (define-key 2048-mode-map "g" '2048-game)
  (dtk-set-rate 
   (+ dtk-speech-rate-base
      (* dtk-speech-rate-step  3 )))
  (dtk-set-punctuations 'some)
  (emacspeak-auditory-icon 'open-object)
  (emacspeak-pronounce-define-local-pronunciation "0" "o")
  (emacspeak-2048-speak-board))
(declaim (special-display-p 2048-mode-hook))
(add-hook '2048-mode-hook 'emacspeak-2048-setup )
;;}}}
(provide 'emacspeak-2048)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

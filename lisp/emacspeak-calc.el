;;; emacspeak-calc.el --- Speech enable the Emacs Calculator -- a powerful symbolic algebra system  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $ 
;;; Description: 
;;; Keywords:
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com 
;;; A speech interface to Emacs |
;;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
;;;  $Revision: 4532 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2021, T. V. Raman 
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}

;;{{{  Introduction:
;;; Commentary:
;;; This module extends the Emacs Calculator.
;;; Extensions are minimal.
;;; We force a calc-load-everything,
;;; And use an after advice on this function
;;; To fix all of calc's interactive functions
;;; Code:
;;}}}
;;{{{ required modules
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{  advice calc interaction 
(defadvice calc-dispatch (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)))

(defadvice calc-quit (after emacspeak pre act comp)
  "Announce the buffer that becomes current when calc is quit."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{  speak output 

(defadvice calc-call-last-kbd-macro (around emacspeak pre act comp)
  "Speak."
  (cond
   ((ems-interactive-p)
    (ems-with-messages-silenced ad-do-it)
    (tts-with-punctuations 'all
                           (emacspeak-read-previous-line))
    (emacspeak-auditory-icon 'task-done))
   (t ad-do-it))
  ad-return-value)

(defadvice  calc-do (around emacspeak pre act comp)
  "Speak previous line of output."
  (ems-with-messages-silenced ad-do-it)
  (tts-with-punctuations
   'all
   (emacspeak-read-previous-line)
   (emacspeak-auditory-icon 'select-object))
  ad-return-value)

(defadvice  calc-trail-here (after emacspeak pre act comp)
  "Speak previous line of output."
  (emacspeak-speak-line)
  (emacspeak-auditory-icon 'select-object))

;;}}}
(provide 'emacspeak-calc)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}

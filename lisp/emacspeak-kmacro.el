;;; emacspeak-kmacro.el --- Speech-enable kbd macro interface  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak front-end for KMACRO 
;;; Keywords: Emacspeak, kmacro 
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
;;;  $Revision: 4241 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1995 -- 2021, T. V. Raman
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;;{{{  Introduction
;;; speech-enables kmacro --- a kbd macro interface
;;}}}
;;{{{ required modules

;;; Code:
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ bind keys 

(global-set-key [f13] 'kmacro-start-macro-or-insert-counter)
(global-set-key [f14] 'kmacro-end-or-call-macro)

;;}}}
;;{{{ Advice interactive commands

(defadvice kmacro-start-macro (before emacspeak pre act comp)
  "Provide auditory icon."
  (when  (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Defining new kbd macro.")))

(defadvice kmacro-start-macro-or-insert-counter (before emacspeak pre act comp)
  "Provide auditory icon if new macro is being defined."
  (when (and (ems-interactive-p)
             (not  defining-kbd-macro)
             (not executing-kbd-macro))
    (emacspeak-auditory-icon 'yank-object)
    (message "Defining new kbd macro.")))

(defadvice kmacro-end-or-call-macro (before emacspeak pre act comp)
  "speak about we are about to do."
  (cond
   ((and (ems-interactive-p)
         defining-kbd-macro)
    (emacspeak-auditory-icon 'close-object)
    (message "Finished defining kbd macro."))
   (t(emacspeak-auditory-icon 'open-object)
     (message "Calling macro."))))

(defadvice kmacro-end-or-call-macro-repeat (before emacspeak pre act comp)
  "speak about we are about to do."
  (cond
   ((and (ems-interactive-p)
         defining-kbd-macro)
    (message "Finished defining kbd macro."))
   (t(emacspeak-auditory-icon 'select-object)
     (message "Calling macro."))))

(defadvice kmacro-edit-macro-repeat (after emacspeak pre act
                                           comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice kmacro-call-ring-2nd-repeat (before emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (message "Calling  second macro from ring.")))

(defadvice kmacro-call-macro (around emacspeak pre act comp)
  "Speech-enabled by emacspeak."
  (let ((emacspeak-speak-messages nil))
    ad-do-it
    ad-return-value))

(defadvice call-last-kbd-macro (around emacspeak pre act comp)
  "Speech-enabled by emacspeak."
  (let ((emacspeak-speak-messages t))
    ad-do-it
    ad-return-value))

;;}}}
(provide 'emacspeak-kmacro)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}


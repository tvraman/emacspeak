;;; emacspeak-kmacro.el --- Speech-enable kbd macro interface
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak front-end for KMACRO 
;;; Keywords: Emacspeak, kmacro 
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision: 24.0 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1999 T. V. Raman <raman@cs.cornell.edu>
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

;;; Commentary:
;;{{{  Introduction
;;; speech-enables kmacro --- a kbd macro interface
;;}}}
;;{{{ required modules

;;; Code:
(require 'emacspeak-preamble)

;;}}}
;;{{{ bind keys 

(global-set-key [f13] 'kmacro-start-macro-or-insert-counter)
(global-set-key [f14] 'kmacro-end-or-call-macro)

;;}}}
;;{{{ Advice interactive commands

(defadvice kmacro-start-macro (before emacspeak pre act comp)
  "Provide auditory icon."
  (when  (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (message "Defining new kbd macro.")))

(defadvice kmacro-start-macro-or-insert-counter (before
                                                 emacspeak pre act comp)
  "Provide auditory icon if new macro is being defined."
  (when (and (interactive-p)
             (not  defining-kbd-macro )
             (not executing-kbd-macro))
    (emacspeak-auditory-icon 'select-object)
    (message "Defining new kbd macro.")))

(defadvice kmacro-end-or-call-macro (before emacspeak pre act comp)
  "Provide auditory feedback about we are about to do."
  (cond
   ((and (interactive-p)
         defining-kbd-macro)
    (message "Finished defining kbd macro."))
   (t(emacspeak-auditory-icon 'select-object)
     (message "Calling macro."))))

(defadvice kmacro-end-or-call-macro-repeat (before emacspeak pre act comp)
  "Provide auditory feedback about we are about to do."
  (cond
   ((and (interactive-p)
         defining-kbd-macro)
    (message "Finished defining kbd macro."))
   (t(emacspeak-auditory-icon 'select-object)
     (message "Calling macro."))))

(defadvice kmacro-edit-macro-repeat (after emacspeak pre act
                                           comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-lin)))
(defadvice kmacro-call-ring-2nd-repeat (before emacspeak pre act
                                               comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (message "Calling  second macro from ring.")))

;;}}}
(provide 'emacspeak-kmacro)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}


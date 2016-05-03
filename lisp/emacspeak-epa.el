;;; emacspeak-epa.el --- Speech-enable EasyPG Assistant
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable EPA An Emacs Interface to epa
;;; Keywords: Emacspeak,  Audio Desktop epa
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
;;; MERCHANTABILITY or FITNEPA FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; EPA == EasyPG Assistant
;;; Integrate GPG functionality into Emacs.
;;; Speech-enable all interactive commands.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map Faces:
(voice-setup-add-map
 '(
   (epa-validity-high voice-animate)
   (epa-validity-medium voice-smoothen)
   (epa-validity-low voice-smoothen-extra)
   (epa-validity-disabled voice-monotone)
   (epa-string voice-lighten)
   (epa-mark voice-bolden)
   (epa-field-name voice-smoothen)
   (epa-field-body voice-animate)))

;;}}}
;;{{{ Advice Interactive Commands:
(loop
 for f in
 '(
   epa-decrypt-region epa-decrypt-file epa-decrypt-armor-in-region
   epa-encrypt-file epa-encrypt-region
   epa-dired-do-verify epa-dired-do-sign
   epa-dired-do-encrypt epa-dired-do-decrypt 
   )
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback. "
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'task-done)))))

(defadvice epa-delete-keys (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)))
;;}}}

(provide 'emacspeak-epa)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

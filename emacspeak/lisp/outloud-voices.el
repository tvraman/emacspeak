;;; outloud-voices.el --- Define various device independent voices in terms of OutLoud tags
;;; $Id$
;;; $Author$
;;; Description:  Module to set up Eloquent voices and personalities
;;; Keywords: Voice, Personality, IBM ViaVoice Outloud
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision$ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2002, T. V. Raman 
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
;;{{{  Introduction:

;;; This module defines the various voices used in voice-lock mode.
;;; This module is IBM ViaVoice Outloud specific.

;;}}}
;; 
;;; Code:
(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(eval-when-compile (load-library "cl-extra"))
;;{{{  voice table

(defvar tts-default-voice 'paul 
  "Default voice used. ")

(defvar outloud-default-voice-string " `v1 "
  "Default Outloud tag for the default voice.")

(defvar outloud-voice-table (make-hash-table)
  "Association between symbols and strings to set Outloud  voices.
The string can set any voice parameter.")

(defsubst outloud-define-voice (name command-string)
  "Define a Outloud  voice named NAME.
This voice will be set   by sending the string
COMMAND-STRING to the TTS engine."
  (declare (special outloud-voice-table ))
  (setf (gethash name outloud-voice-table )
	command-string))

(defsubst outloud-get-voice-command-internal  (name)
  "Retrieve command string for  voice NAME."
  (declare (special outloud-voice-table))
  (or  (gethash name outloud-voice-table) outloud-default-voice-string))

(defsubst outloud-get-voice-command (name)
  "Retrieve command string for  voice NAME."
  (declare (special dtk-speech-rate))
  (concat 
   (outloud-get-voice-command-internal name)
   (format " `vs%s " dtk-speech-rate )))

(defsubst outloud-voice-defined-p (name)
  "Check if there is a voice named NAME defined."
  (declare (special outloud-voice-table ))
  (gethash name outloud-voice-table ))

(defsubst outloud-define-voice-alias (alias voice )
  "Alias  ALIAS to be same as voice VOICE."
  (declare (special outloud-voice-table))
  (outloud-define-voice alias (gethash  voice outloud-voice-table)))

;;}}}
;;{{{ voice definitions

;;; the nine predefined voices:
(outloud-define-voice 'paul  " `v1 ")
(outloud-define-voice 'harry " `v1 `vh65 `vb50 ")
(outloud-define-voice 'dennis " `v1  `vb0 ")
(outloud-define-voice 'frank " `v1 `vr100 ")
(outloud-define-voice 'betty " `v7 ")
(outloud-define-voice 'ursula " `v2 ")
(outloud-define-voice 'rita " `v2 `vr100 ")
(outloud-define-voice 'wendy " `v2 `vy50 ")
(outloud-define-voice 'kit " `v3 ")

;;; Modified voices:
;;; Modifications for paul:
(outloud-define-voice 'paul-bold
                      " `v1 `vr10 `vf75 `vh60 `vb45 `vv100
")

(outloud-define-voice 'paul-italic
		      " `v1 `vh37 `vb70 `vf100  `vv100 ")
(outloud-define-voice 'paul-smooth
		      " `v1 `vr0 `vh40 `vb60 `vf75  `vv88 ")

(outloud-define-voice 'anotation-voice
		      " `v1 `vr0 `vh40 `vb60 `vf75  `vv88  ")
                      
(outloud-define-voice 'indent-voice  " `v1 `vr0 `vv65 `vh45
`vf35 `p2  ")
(outloud-define-voice 'paul-animated
		      " `v1 `vf65 `vh45 `vb70 `vv100 ")

(outloud-define-voice 'paul-monotone " `v1 `vf0`vb50  `vv85 ")

;;}}}
;;{{{  Associate faces to standard voices:

(outloud-define-voice-alias 'bold 'paul-smooth)
(outloud-define-voice-alias 'bold-italic 'betty)
(outloud-define-voice-alias 'underline 'ursula)
(outloud-define-voice-alias 'fixed 'paul-monotone)
(outloud-define-voice-alias 'italic 'paul-italic)
(outloud-define-voice-alias 'excerpt 'annotation-voice )

;;}}}
;;{{{  the inaudible voice

(outloud-define-voice 'inaudible " `vv0 ")

;;}}}
;;{{{  return list of all defined voices

(defun outloud-voice-personality-list ()
  "Return list of all defined voices."
  (declare (special outloud-voice-table))
  (loop for key being the hash-keys of outloud-voice-table
	collect key ))

;;}}}
;;{{{ default speech rate 

;;}}}
(provide 'outloud-voices)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}

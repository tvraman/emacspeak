;;; dtk-voices.el --- Define various device independent voices in terms of Dectalk codes.
;;; $Id$
;;; $Author$
;;; Description:  Module to set up dtk voices and personalities
;;; Keywords: Voice, Personality, Dectalk
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
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Introduction:

;;; Commentary:
;;; This module defines the various voices used in voice-lock mode.
;;; This module is Dectalk specific.

;;}}}
;;{{{ required modules

;;; Code:
(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(eval-when-compile (load-library "cl-extra"))

;;}}}
;;{{{  voice table

(defvar tts-default-voice 'paul 
  "Default voice used. ")

(defvar dtk-default-voice-string "[:np]"
  "dtk string for the default voice.")

(defvar dtk-voice-table (make-hash-table)
  "Association between symbols and strings to set dtk voices.
The string can set any dtk parameter.")

(defsubst dtk-define-voice (name command-string)
  "Define a dtk voice named NAME.
This voice will be set   by sending the string
COMMAND-STRING to the Dectalk."
  (declare (special dtk-voice-table ))
  (setf (gethash name dtk-voice-table ) command-string))

(defsubst dtk-get-voice-command (name)
  "Retrieve command string for  voice NAME."
  (declare (special dtk-voice-table ))
  (or  (gethash name dtk-voice-table) dtk-default-voice-string))

(defsubst dtk-voice-defined-p (name)
  "Check if there is a voice named NAME defined."
  (declare (special dtk-voice-table ))
  (gethash name dtk-voice-table ))

(defsubst dtk-define-voice-alias (alias voice )
  "Alias  ALIAS to be same as voice VOICE."
  (dtk-define-voice alias (dtk-get-voice-command voice )))

;;}}}
;;{{{ voice definitions

;;; the nine predefined voices:
(dtk-define-voice 'paul "[:np ]")
(dtk-define-voice 'harry "[:nh ]")
(dtk-define-voice 'dennis "[:nd]")
(dtk-define-voice 'frank "[:nf]")
(dtk-define-voice 'betty "[:nb]")
(dtk-define-voice 'ursula "[:nu]")
(dtk-define-voice 'rita "[:nr]")
(dtk-define-voice 'wendy "[:nw]")
(dtk-define-voice 'kit "[:nk]")

;;; Modified voices:
;;; Modifications for paul:
(dtk-define-voice 'paul-bold "[:np  :dv sm 50 ri 30 pr 200 ap 132]")
(dtk-define-voice 'paul-italic "[:np :dv ap 132 hs 99 pr 200 hr 20 sr 32 qu 100]")
(dtk-define-voice 'paul-smooth "[:np  :dv sm 15 ri 65 sr 50 as 100 qu 100]")
(dtk-define-voice 'annotation-voice "[:np :dv  sm 30 ri 50  hr 0 sr 0 ]")
(dtk-define-voice 'indent-voice  "[:np :dv  sm 40 ri 40  hr 7  sr 10 ]")
(dtk-define-voice 'paul-animated "[:np  :dv pr 200 hr  30 sr 50 as 100 qu 100]")
(dtk-define-voice 'paul-monotone "[:np  :dv pr 0 hr 1 sr 2 as 0 ]")

;;}}}
;;{{{  Associate faces to standard voices:

(dtk-define-voice-alias 'bold 'harry)
(dtk-define-voice-alias 'bold-italic 'betty)
(dtk-define-voice-alias 'underline 'ursula)
(dtk-define-voice-alias 'fixed 'paul-monotone)
(dtk-define-voice-alias 'italic 'paul-animated)
(dtk-define-voice-alias 'excerpt 'annotation-voice )

;;}}}
;;{{{  the inaudible voice

(dtk-define-voice 'inaudible "")

;;}}}
;;{{{  return list of all defined voices

(defun dtk-voice-personality-list ()
  "Return list of all defined voices."
  (declare (special dtk-voice-table))
  (loop for key being the hash-keys of dtk-voice-table
	collect key ))

;;}}}
(provide 'dtk-voices)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}

;;; bep-voices.el --- Define various device independent voices in terms of BEP codes.
;;; $Id$
;;; $Author$
;;; Description:  Module to set up dtk voices and personalities
;;; Keywords: Voice, Personality, BEP
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


;;; Commentary:
;;{{{  Introduction:

;;; This module defines the various voices used in voice-lock mode.
;;; This module is BEP(Japanese/English bilingual) specific.

;;}}}
;; 
;;; Code:
(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(eval-when-compile (load-library "cl-extra"))
;;{{{  voice table



(defvar tts-default-voice 'paul 
  "Default voice used. ")


(defvar bep-default-voice-string "[:np]"
  "Default BEP command string for the default voice.")


(defvar bep-voice-table (make-hash-table)
  "Association between symbols and strings to set BEP voices.
The string can set any BEP Speech Server command.")

(defsubst bep-define-voice (name command-string)
  "Define a BEP voice named NAME.
This voice will be set   by sending the string
COMMAND-STRING to the BEP Speech Server"
  (declare (special bep-voice-table ))
   (setf (gethash name bep-voice-table )
              command-string))

(defsubst bep-get-voice-command (name)
  "Retrieve command string for  voice NAME."
  (declare (special bep-voice-table ))
  (or  (cl-gethash name bep-voice-table)
        bep-default-voice-string))

(defsubst bep-voice-defined-p (name)
"Check if there is a voice named NAME defined."
  (declare (special bep-voice-table ))
  (cl-gethash name bep-voice-table ))

(defsubst bep-define-voice-alias (alias voice )
"Alias  ALIAS to be same as voice VOICE."
  (bep-define-voice alias (bep-get-voice-command voice )))

;;}}}
;;{{{ voice definitions

;;; the nine predefined voices:
(bep-define-voice 'paul "[:np ]")
(bep-define-voice 'harry "[:nh ]")
(bep-define-voice 'dennis "[:nd]")
(bep-define-voice 'frank "[:nf]")
(bep-define-voice 'betty "[:nb]")
(bep-define-voice 'ursula "[:nu]")
(bep-define-voice 'rita "[:nr]")
(bep-define-voice 'wendy "[:nw]")
(bep-define-voice 'kit "[:nk]")

;;; Modified voices:
;;; Modifications for paul:
(bep-define-voice 'paul-bold "[:np-bold]")
(bep-define-voice 'paul-italic
                  "[:np-italic]")
(bep-define-voice 'paul-smooth
                  "[:np-smooth]")
(bep-define-voice 'annotation-voice "[:np-annotate]")
(bep-define-voice 'indent-voice  "[:np-indent]")
(bep-define-voice 'paul-animated
                  "[:np-animated]")
(bep-define-voice 'paul-monotone "[:np-monotone]")
;(bep-define-voice 'paul-italic "[:np :dv ap 140 hs 99 pr 200  hr 10 sr 20]")

;;}}}
;;{{{  Associate faces to standard voices:

(bep-define-voice-alias 'bold 'paul-smooth)
(bep-define-voice-alias 'bold-italic 'betty)
(bep-define-voice-alias 'underline 'ursula)
(bep-define-voice-alias 'fixed 'paul-monotone)
(bep-define-voice-alias 'italic 'paul-animated)
(bep-define-voice-alias 'excerpt 'annotation-voice )

;;}}}
;;{{{  Settings from Janet Cahn's thesis.

;;; the  following are taken from Janet Cahn's Masters thesis.
;;; I've modified them for the Dectalk Express.
;;; lo is g5 on express.
;;; Also get rid of absolute changes in speech rate.

(bep-define-voice 'paul-angry
                  "[:np :dv as 90 ap 95 bf 29 hr 13 pr 250 sr 90 br 0 la 0 lx 0 qu 58 ri 100 sm 0 gh 73 gf 74 gv 65]")

(bep-define-voice
 'paul-disgusted
 "[:np   :dv as 50 ap 120 bf 18 hr 18 pr 145 sr 26 br 0 la 0 lx 0   qu 0 ri 85 sm 18 gh 74 gf 75 gv 63 b4 261 b5 332 ]")

(bep-define-voice
 'paul-glad
 "[:np   :dv as 39 ap 105 bf 10 hr 5 pr 250 sr 73 br 0 la 0 lx 0   qu 0 ri 56 sm 48 gh 49 gf 67 gv 63 b4 261 b5 332 ]")

(bep-define-voice
 'paul-sad
 "[:np   :dv as 30 ap 120 bf 14 hr 16 pr 50 sr 78 br 72 la 0 lx 100  qu 100 ri 7 sm 94 gh 35 gf 65 gv 62 b4 330 b5 1190 ]")

(bep-define-voice 'paul-scared
                  "[:np   :dv as 20 ap 300 bf 0 hr 100 pr 250 sr 100 br 0 la 0 lx 0   qu 100 ri 100 sm 0 gh 70 gf 70 gv 65 b4 260 b5 330 ]")

(bep-define-voice 'paul-surprised
                  "[:np   :dv as 60 ap 120 bf 9 hr 5 pr 220 sr 66 br 0 la 0 lx 0   qu 70 ri 49 sm 54 gh 70 gf 70 gv 64 b4 260 b5 331 ]")

;;}}}
;;{{{  the inaudible voice

(bep-define-voice 'inaudible "")

;;}}}
;;{{{  return list of all defined voices

(defun voice-personality-list ()
  "Return list of all defined voices."
(declare (special bep-voice-table))
(loop for key being the hash-keys of bep-voice-table
      collect key ))
;;}}}
;;{{{ standard symbols as voices:

(bep-define-voice-alias 'voice-lock-comment-personality 'paul-monotone)
(bep-define-voice-alias 'voice-lock-underline-personality 'paul-animated)
(bep-define-voice-alias 'voice-lock-bold-personality 'harry)
(bep-define-voice-alias 'voice-lock-italic-personality 'paul-italic)
(bep-define-voice-alias 'voice-lock-doc-string-personality 'dennis)
(bep-define-voice-alias 'voice-lock-string-personality 'betty)
(bep-define-voice-alias 'voice-lock-function-name-personality 'harry)
(bep-define-voice-alias 'voice-lock-warning-personality 'paul-angry)
(bep-define-voice-alias 'voice-lock-keyword-personality
                        'ursula)
(bep-define-voice-alias 'voice-lock-builtin-personality
                        'harry)
(bep-define-voice-alias 'voice-lock-variable-name-personality 'paul-animated)
(bep-define-voice-alias 'voice-lock-type-personality 'paul-smooth)
(bep-define-voice-alias 'voice-lock-reference-personality 'paul-animated)

;;}}}
;;{{{  font to voice 

;;; let's define the standard symbols used as fonts as
;;; personalities here.


(bep-define-voice-alias 'font-lock-variable-name-face 'voice-lock-variable-name-personality)
(bep-define-voice-alias 'font-lock-reference-face 'voice-lock-reference-personality)
(bep-define-voice-alias'font-lock-comment-face  'voice-lock-comment-personality)
(bep-define-voice-alias'font-lock-string-face  'voice-lock-string-personality)
(bep-define-voice-alias 'font-lock-keyword-face  'voice-lock-keyword-personality)
(bep-define-voice-alias 'font-lock-builtin-face  'voice-lock-builtin-personality)
(bep-define-voice-alias 'font-lock-function-name-face 'voice-lock-function-name-personality)
(bep-define-voice-alias 'font-lock-type-face  'voice-lock-type-personality)
(bep-define-voice-alias 'font-lock-constant-face  'voice-lock-constant-personality)
(bep-define-voice-alias 'font-lock-warning-face  'voice-lock-warning-personality)
;;}}}
(provide 'bep-voices)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}

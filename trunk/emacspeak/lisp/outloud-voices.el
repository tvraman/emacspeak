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
;;{{{ Required modules

;;; Code:
(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(eval-when-compile (load-library "cl-extra"))
(require 'acss-structure)

;;}}}
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
;;{{{  the inaudible voice

(outloud-define-voice 'inaudible " `vv0 ")

;;}}}
;;{{{  Mapping css parameters to tts codes

;;{{{ voice family codes

(defsubst outloud-get-family-code (name)
  "Get control code for voice family NAME."
  (outloud-get-voice-command-internal name))

;;}}}
;;{{{  hash table for mapping families to their dimensions

(defvar outloud-css-code-tables (make-hash-table)
  "Hash table holding vectors of outloud codes.
Keys are symbols of the form <FamilyName-Dimension>.
Values are vectors holding the control codes for the 10 settings.")

(defsubst outloud-css-set-code-table (family dimension table)
  "Set up voice FAMILY.
Argument DIMENSION is the dimension being set,
and TABLE gives the values along that dimension."
  (declare (special outloud-css-code-tables))
  (let ((key (intern (format "%s-%s" family dimension))))
    (setf  (gethash key outloud-css-code-tables) table )))

(defsubst outloud-css-get-code-table (family dimension)
  "Retrieve table of values for specified FAMILY and DIMENSION."
  (declare (special outloud-css-code-tables))
  (let ((key (intern (format "%s-%s" family dimension))))
    (gethash key outloud-css-code-tables)))

;;}}}
;;{{{ volume

(defvar outloud-gain-table (make-vector  10 "")
  "Maps CSS volume settings to actual synthesizer codes.")

;;}}}
;;{{{  average pitch

;;; Average pitch for standard male voice is 65 --this is mapped to
;;; a setting of 5.
;;; Average pitch varies inversely with speaker head size --a child
;;; has a small head and a higher pitched voice.
;;; We change parameter head-size in conjunction with average pitch to
;;; produce a more natural change on the TTS engine.

;;{{{  paul average pitch

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format " `vb%s `vh%s "
                    (second setting)
                    (third setting)))))
   '(
     (0 0 90)
     (1 13 81 )
     (2 26 72)
     (3 39 63)
     (4 52 54  )
     (5 65 50)
     (6 74 40)
     (7 83 30 )
     (8 87 26)
     (9 92 21)))
  (outloud-css-set-code-table 'paul 'average-pitch table ))

;;}}}
;;{{{  harry average pitch

(let ((table (make-vector 10 "")))

  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format " `vb%s `vh% s"
                    (second setting)
                    (third setting)))))
   '(
     (0 0 90)
     (1 10 85 )
     (2 20 80)
     (3 30 70)
     (4 40 60)
     (5 50 60)
     (6 60 50)
     (7 70 40 )
     (8 80 30)
     (9 90 20)))
  (outloud-css-set-code-table 'harry 'average-pitch table ))

;;}}}
;;{{{  betty average pitch

;;;defalt baseline is average pitch of 81 

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
	    (first setting)
	    (format " `vb%s `vh% s"
		    (second setting)
		    (third setting)))))
   '(
     (0 5 70)
     (1 17 66)
     (2 33 62)
     (3 49 58)
     (4 65 54 )
     (5 81  50)
     (6 85 55)
     (7 89  60)
     (8 93 65)
     (9 97 69)))
  (outloud-css-set-code-table 'betty 'average-pitch table ))

;;}}}

(defsubst outloud-get-average-pitch-code (value family)
  "Get  AVERAGE-PITCH for specified VALUE and  FAMILY."
  (or family (setq family 'paul))
  (aref (outloud-css-get-code-table family 'average-pitch)
	value))

;;}}}
;;{{{  pitch range

;;;  Standard pitch range is 30 and is  mapped to
;;; a setting of 5.
;;; A value of 0 produces a flat monotone voice --maximum value of 100
;;; produces a highly animated voice.

;;{{{  paul pitch range

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
	    (first setting)
	    (format " `vf%s  "
		    (second setting)))))
   '(
     (0 0 )
     (1 5 )
     (2  15)
     (3  20)
     (4  25 )
     (5  30 )
     (6  47)
     (7  64)
     (8  81)
     (9  100)))
  (outloud-css-set-code-table 'paul 'pitch-range table ))

;;}}}
;;{{{  harry pitch range

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
	    (first setting)
	    (format " `vf%s  "
		    (second setting)))))
   '(
     (0 0 )
     (1 5 )
     (2  15)
     (3  20)
     (4  25 )
     (5  30 )
     (6  47)
     (7  64)
     (8  81)
     (9  100)))
  (outloud-css-set-code-table 'harry 'pitch-range table ))

;;}}}
;;{{{  betty pitch range

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format " `vf%s  "
                    (second setting)))))
   '(
     (0 0 )
     (1 5 )
     (2  15)
     (3  20)
     (4  25 )
     (5  30 )
     (6  47)
     (7  64)
     (8  81)
     (9  100)))
  (outloud-css-set-code-table 'betty 'pitch-range table ))

;;}}}
(defsubst outloud-get-pitch-range-code (value family)
  "Get pitch-range code for specified VALUE and FAMILY."
  (or family (setq family 'paul))
  (aref (outloud-css-get-code-table family 'pitch-range)
	value))

;;}}}
;;{{{  stress

;;; On the outloud we map stress to roughness
;;{{{  paul stress

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table (first setting)
            (format " `vr%s " (second setting)))))
   '(
     (0 0 )
     (1 0 )
     (2  0)
     (3  0)
     (4  0 )
     (5  0 )
     (6  5)
     (7  10)
     (8  15)
     (9  20)
     ))
  (outloud-css-set-code-table 'paul 'stress table)
  (outloud-css-set-code-table 'harry 'stress table)
  (outloud-css-set-code-table 'betty  'stress table))

;;}}}
(defsubst outloud-get-stress-code (value family)
  (or family (setq family 'paul ))
  (aref (outloud-css-get-code-table family 'stress)
        value))

;;}}}
;;{{{  richness

;;{{{  paul richness

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format " `vv%s "
                    (second setting)))))
   '(
     (0 60)
     (1 78)
     (2 80)
     (3 84)
     (4 88)
     (5 92)
     (6 93)
     (7 95)
     (8 97 )
     (9 100)))
  (outloud-css-set-code-table 'paul 'richness table)
  (outloud-css-set-code-table 'harry 'richness table)
  (outloud-css-set-code-table 'betty 'richness table))

;;}}}

(defsubst outloud-get-richness-code (value family)
  (or family (setq family 'paul))
  (aref (outloud-css-get-code-table family 'richness)
        value))

;;}}}

;;}}}
;;{{{  outloud-define-voice-from-speech-style

(defun outloud-define-voice-from-speech-style (name style)
  "Define NAME to be a outloud voice as specified by settings in STYLE."
  (let* ((family(acss-family style))
	 (command
	  (concat 
	   (outloud-get-family-code family)
	   (outloud-get-average-pitch-code (acss-average-pitch style) family)
	   (outloud-get-pitch-range-code (acss-pitch-range style) family)
	   (outloud-get-stress-code (acss-stress style ) family)
	   (outloud-get-richness-code (acss-richness style) family))))
    (outloud-define-voice name command)))

;;}}}
;;{{{  outloud-personality-from-speech-style

(defun outloud-personality-from-speech-style (style)
  "First compute a symbol that will be name for this STYLE.
Then see if a voice defined for it.
Finally return the symbol"
  (cond
   ((= 0 (acss-gain style))
    'inaudible)
   (t
    (let ((name (intern
                 (format "%s-a%s-p%s-s%s-r%s"
                         (acss-family style)
                         (acss-average-pitch style)
                         (acss-pitch-range style)
                         (acss-stress style)
                         (acss-richness style)))))
      (unless (outloud-voice-defined-p name)
        (outloud-define-voice-from-speech-style name style))
      name))))

;;}}}
(provide 'outloud-voices)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}

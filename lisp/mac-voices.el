;;; mac-voices.el --- Define various device independent voices in terms of Mac tags
;;; $Id: mac-voices.el 6342 2009-10-20 19:12:40Z tv.raman.tv $
;;; $Author: Dave $
;;; Description:  Module to set up Mac voices and personalities
;;; Keywords: Voice, Personality, Mac
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2008-07-06 10:18:30 -0700 (Sun, 06 Jul 2008) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2009, T. V. Raman 
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
;;; This module is Apple Mac specific.

;;}}}
;;{{{ Required modules

;;; Code:
(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'acss-structure)

;;}}}
;;{{{  voice table

(defvar mac-default-voice-string "[[voice alex]]"
  "Default Mac tag for  default voice.")

(defvar mac-voice-table (make-hash-table)
  "Association between symbols and strings to set Mac  voices.
The string can set any voice parameter.")

(defsubst mac-define-voice (name command-string)
  "Define a Mac  voice named NAME.
This voice will be set   by sending the string
COMMAND-STRING to the TTS engine."
  (declare (special mac-voice-table ))
  (puthash name command-string mac-voice-table))

(defsubst mac-get-voice-command-internal  (name)
  "Retrieve command string for  voice NAME."
  (declare (special mac-voice-table))
  (cond
   ((listp name)
    (mapconcat #'mac-get-voice-command name " "))
   (t (or  (gethash name mac-voice-table)
           mac-default-voice-string))))

(defsubst mac-get-voice-command (name)
  "Retrieve command string for  voice NAME."
  (declare (special dtk-speech-rate))
  (concat 
   (mac-get-voice-command-internal name)))
;   (format " [[rate %s]] " dtk-speech-rate )))

(defsubst mac-voice-defined-p (name)
  "Check if there is a voice named NAME defined."
  (declare (special mac-voice-table ))
  (gethash name mac-voice-table ))

;;}}}
;;{{{ voice definitions

;;; the nine predefined voices: TODO: figure out if embedding is possible (and update voice names).
(mac-define-voice 'paul  " [[voice alex]] ")
(mac-define-voice 'harry " [[voice ralf]] ")
(mac-define-voice 'dennis " [[voice bruce]] ")
(mac-define-voice 'frank " [[voice fred]] ")
(mac-define-voice 'betty " [[voice victoria]] ")
(mac-define-voice 'ursula " [[voice kathy]] ")
(mac-define-voice 'rita " [[voice vicki]] ")
(mac-define-voice 'wendy " [[voice kathy]] ")
(mac-define-voice 'kit " [[voice junior]] ")

;;; Modified voices:

;;}}}
;;{{{  the inaudible voice

(mac-define-voice 'inaudible " ")

;;}}}
;;{{{  Mapping css parameters to tts codes

;;{{{ voice family codes

(defsubst mac-get-family-code (name)
  "Get control code for voice family NAME."
  (mac-get-voice-command-internal name))

;;}}}
;;{{{  hash table for mapping families to their dimensions

(defvar mac-css-code-tables (make-hash-table)
  "Hash table holding vectors of mac codes.
Keys are symbols of the form <FamilyName-Dimension>.
Values are vectors holding the control codes for the 10 settings.")

(defsubst mac-css-set-code-table (family dimension table)
  "Set up voice FAMILY.
Argument DIMENSION is the dimension being set,
and TABLE gives the values along that dimension."
  (declare (special mac-css-code-tables))
  (let ((key (intern (format "%s-%s" family dimension))))
    (puthash key table mac-css-code-tables )))

(defsubst mac-css-get-code-table (family dimension)
  "Retrieve table of values for specified FAMILY and DIMENSION."
  (declare (special mac-css-code-tables))
  (let ((key (intern (format "%s-%s" family dimension))))
    (gethash key mac-css-code-tables)))

;;}}}
;;{{{ volume

(defvar mac-gain-table (make-vector  10 "")
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
            (format " [[pbas %s]] "
                    (second setting)
                    (third setting)))))
   '(
     (0 44 52)
     (1 50 58 )
     (2 56 56)
     (3 58 54)
     (4 62 52  )
     (5 65 50)
     (6 69 48)
     (7 73 46 )
     (8 77 44)
     (9 82 40)))
  (mac-css-set-code-table 'paul 'average-pitch table ))

;;}}}
;;{{{  harry average pitch

(let ((table (make-vector 10 "")))

  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format " pitch %s"
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
  (mac-css-set-code-table 'harry 'average-pitch table ))

;;}}}
;;{{{  betty average pitch

;;;defalt baseline is average pitch of 81 

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format " [[pbas %s]] "
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
  (mac-css-set-code-table 'betty 'average-pitch table ))

;;}}}

(defsubst mac-get-average-pitch-code (value family)
  "Get  AVERAGE-PITCH for specified VALUE and  FAMILY."
  (or family (setq family 'paul))
  (if value 
      (aref (mac-css-get-code-table family 'average-pitch)
            value)
    ""))

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
            (format " [[pbas %s]] "
                    (second setting)))))
   '(
     (0 0 )
     (1 10 )
     (2  18)
     (3  25)
     (4  35 )
     (5  44 )
     (6  48)
     (7  54)
     (8  60)
     (9  67)))
  (mac-css-set-code-table 'paul 'pitch-range table ))

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
  (mac-css-set-code-table 'harry 'pitch-range table ))

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
  (mac-css-set-code-table 'betty 'pitch-range table ))

;;}}}
(defsubst mac-get-pitch-range-code (value family)
  "Get pitch-range code for specified VALUE and FAMILY."
  (or family (setq family 'paul))
  (if value 
      (aref (mac-css-get-code-table family 'pitch-range)
            value)
    ""))

;;}}}
;;{{{  stress

;;; On the mac we map stress to roughness
;;; we also use stress markers `00 .. `4 
;;{{{  paul stress TODO

(let ((table (make-vector 10 "")))
  (mapcar
   #'(lambda (setting)
       (aset table (first setting)
             (format " "
                     (second setting))))
;;; stress markers not used for now.
   '(
     (0 0 "`00")
     (1 5 "`00")
     (2  10 "`0")
     (3  15 "`0")
     (4  20 "`1" )
     (5  25 "`1" )
     (6  30 "`v2")
     (7  35 "`v2")
     (8  40 "`v3")
     (9  45 "`v4")))
  (mac-css-set-code-table 'paul 'stress table)
  (mac-css-set-code-table 'harry 'stress table)
  (mac-css-set-code-table 'betty  'stress table))

;;}}}
(defsubst mac-get-stress-code (value family)
  (or family (setq family 'paul ))
  (if value 
      (aref (mac-css-get-code-table family 'stress)
            value)
    ""))

;;}}}
;;{{{  richness

;;{{{  paul richness TODO

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format "  "
                    (second setting)
                    (third setting)))))
   '(
     (0 0 60)
     (1 4 78)
     (2 8 80)
     (3 12 84)
     (4 16 88)
     (5 20 92)
     (6 24 93)
     (7 28 95)
     (8 32 97 )
     (9 36 100)))
  (mac-css-set-code-table 'paul 'richness table)
  (mac-css-set-code-table 'harry 'richness table)
  (mac-css-set-code-table 'betty 'richness table))

;;}}}

(defsubst mac-get-richness-code (value family)
  (or family (setq family 'paul))
  (if value 
      (aref (mac-css-get-code-table family 'richness)
            value)
    ""))

;;}}}
;;{{{  punctuations

(defsubst mac-get-punctuations-code (value)
  "Return string needed to set specified punctuations mode."
  "")

;;}}}
;;}}}
;;{{{  mac-define-voice-from-speech-style

(defun mac-define-voice-from-speech-style (name style)
  "Define NAME to be a mac voice as specified by settings in STYLE."
  (let* ((family(acss-family style))
         (command
          (concat 
           (mac-get-family-code family)
           (mac-get-punctuations-code (acss-punctuations style))
           (mac-get-average-pitch-code (acss-average-pitch style) family)
           (mac-get-pitch-range-code (acss-pitch-range style) family)
           (mac-get-stress-code (acss-stress style ) family)
           (mac-get-richness-code (acss-richness style) family))))
    (mac-define-voice name command)))

;;}}}
;;{{{ list voices 

(defun mac-list-voices ()
  "List defined voices."
  (declare (special mac-voice-table))
  (loop for k being the hash-keys of mac-voice-table 
        collect   k))

;;}}}
;;{{{ Configurater 

(defun mac-configure-tts ()
  "Configure TTS environment to use mac  family of synthesizers."
  (declare (special tts-default-speech-rate
                    mac-default-speech-rate
                    dtk-speaker-process))
  (fset 'tts-list-voices'mac-list-voices)
  (fset 'tts-voice-defined-p 'mac-voice-defined-p)
  (fset 'tts-get-voice-command 'mac-get-voice-command)
  (fset 'tts-define-voice-from-speech-style 'mac-define-voice-from-speech-style)
  (setq tts-default-speech-rate mac-default-speech-rate)
  (set-default 'tts-default-speech-rate
               mac-default-speech-rate)
  (dtk-unicode-update-untouched-charsets '(ascii latin-iso8859-1 latin-iso8859-15 latin-iso8859-9 eight-bit-graphic)))

;;}}}
(provide 'mac-voices)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

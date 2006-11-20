;;; dectalk-voices.el --- Define various device independent voices in terms of Dectalk codes.
;;; $Id$
;;; $Author$
;;; Description:  Module to set up Dectalk voices and personalities
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
;;;Copyright (C) 1995 -- 2006, T. V. Raman 
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
(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'acss-structure)
;;}}}
;;{{{  voice table

(defvar dectalk-default-voice-string ""
  "Dectalk string for  default voice --set to be a no-op.")

(defvar dectalk-voice-table (make-hash-table)
  "Association between symbols and strings to set Dectalk voices.
The string can set any Dectalk parameter.")

(defsubst dectalk-define-voice (name command-string)
  "Define a Dectalk voice named NAME.
This voice will be set   by sending the string
COMMAND-STRING to the Dectalk."
  (declare (special dectalk-voice-table ))
  (puthash  name command-string  dectalk-voice-table))

(defsubst dectalk-get-voice-command (name)
  "Retrieve command string for  voice NAME."
  (declare (special dectalk-voice-table ))
  (cond
   ((listp name)
    (mapconcat #'dectalk-get-voice-command name " "))
   (t (or  (gethash name dectalk-voice-table)
           dectalk-default-voice-string))))

(defsubst dectalk-voice-defined-p (name)
  "Check if there is a voice named NAME defined."
  (declare (special dectalk-voice-table ))
  (gethash name dectalk-voice-table ))

;;}}}
;;{{{ voice definitions

;;; the nine predefined voices:
(dectalk-define-voice 'paul "[:np ]")
(dectalk-define-voice 'harry "[:nh ]")
(dectalk-define-voice 'dennis "[:nd]")
(dectalk-define-voice 'frank "[:nf]")
(dectalk-define-voice 'betty "[:nb]")
(dectalk-define-voice 'ursula "[:nu]")
(dectalk-define-voice 'rita "[:nr]")
(dectalk-define-voice 'wendy "[:nw]")
(dectalk-define-voice 'kit "[:nk]")

;;}}}
;;{{{  the inaudible voice
;;; no special code needed --handled by Emacspeak engine.

(dectalk-define-voice 'inaudible "")

;;}}}
;;{{{  Mapping css parameters to Dectalk codes

;;{{{ voice family codes

(defvar dectalk-family-table nil
  "Association list of Dectalk voice names and control codes.")

(defsubst dectalk-set-family-code (name code)
  "Set control code for voice family NAME  to CODE."
  (declare (special dectalk-family-table))
  (when (stringp name)
    (setq name (intern name)))
  (setq dectalk-family-table
        (cons (list name code )
              dectalk-family-table)))

(defsubst dectalk-get-family-code (name)
  "Get control code for voice family NAME."
  (declare (special dectalk-family-table ))
  (when (stringp name)
    (setq name (intern name )))
  (or (cadr (assq  name dectalk-family-table))
      ""))

(dectalk-set-family-code 'paul ":np")
(dectalk-set-family-code 'harry ":nh")
(dectalk-set-family-code 'dennis ":nd")
(dectalk-set-family-code 'frank ":nf")
(dectalk-set-family-code 'betty ":nb")
(dectalk-set-family-code 'ursula ":nu")
(dectalk-set-family-code 'wendy ":nw")
(dectalk-set-family-code 'rita ":nr")
(dectalk-set-family-code 'kid ":nk")

;;}}}
;;{{{  hash table for mapping families to their dimensions

(defvar dectalk-css-code-tables (make-hash-table)
  "Hash table holding vectors of Dectalk codes.
Keys are symbols of the form <FamilyName-Dimension>.
Values are vectors holding the control codes for the 10 settings.")

(defsubst dectalk-css-set-code-table (family dimension table)
  "Set up voice FAMILY.
Argument DIMENSION is the dimension being set,
and TABLE gives the values along that dimension."
  (declare (special dectalk-css-code-tables))
  (let ((key (intern (format "%s-%s" family dimension))))
    (puthash  key table dectalk-css-code-tables )))

(defsubst dectalk-css-get-code-table (family dimension)
  "Retrieve table of values for specified FAMILY and DIMENSION."
  (declare (special dectalk-css-code-tables))
  (let ((key (intern (format "%s-%s" family dimension))))
    (gethash key dectalk-css-code-tables)))

;;}}}
;;{{{ volume

;;; Note:volume settings not implemented for Dectalks.
(defvar dectalk-gain-table (make-vector  10 "")
  "Maps CSS volume settings to actual synthesizer codes.")

;;}}}
;;{{{  average pitch

;;; Average pitch for standard male voice is 122hz --this is mapped to
;;; a setting of 5.
;;; Average pitch varies inversely with speaker head size --a child
;;; has a small head and a higher pitched voice.
;;; We change parameter head-size in conjunction with average pitch to
;;; produce a more natural change on the Dectalk.

;;{{{  paul average pitch

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format " ap %s hs % s"
                    (second setting)
                    (third setting)))))
   '(
     (0 96 115)
     (1 101 112)
     (2 108 109)
     (3 112 106)
     (4 118 103 )
     (5 122  100)
     (6 128 98)
     (7 134 96)
     (8 140 94)
     (9 147 91)
     ))
  (dectalk-css-set-code-table 'paul 'average-pitch table ))

;;}}}
;;{{{  harry average pitch
;;; Harry  has a big head --and a lower pitch for the middle setting 

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format " ap %s hs % s"
                    (second setting)
                    (third setting)))))
   '(
     (0 50 125)
     (1 59 123)
     (2 68 121)
     (3 77 120)
     (4 83  118 )
     (5 89 115)
     (6 95 112)
     (7 110 105)
     (8 125 100)
     (9 140 95)
     ))
  (dectalk-css-set-code-table 'harry 'average-pitch table ))

;;}}}
;;{{{  betty average pitch

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format " ap %s hs % s"
                    (second setting)
                    (third setting)))))
   '(
     (0 160 115)
     (1 170 112)
     (2 181 109)
     (3 192 106)
     (4 200 103 )
     (5 208  100)
     (6 219 98)
     (7 225  96)
     (8 240 94)
     (9 260  91)
     ))
  (dectalk-css-set-code-table 'betty 'average-pitch table ))

;;}}}

(defsubst dectalk-get-average-pitch-code (value family)
  "Get  AVERAGE-PITCH for specified VALUE and  FAMILY."
  (or family (setq family 'paul))
  (if value 
      (aref (dectalk-css-get-code-table family 'average-pitch)
            value)
    ""))

;;}}}
;;{{{  pitch range

;;;  Standard pitch range is 100 and is  mapped to
;;; a setting of 5.
;;; A value of 0 produces a flat monotone voice --maximum value of 250
;;; produces a highly animated voice.
;;; Additionally, we also set the assertiveness of the voice so the
;;; voice is less assertive at lower pitch ranges.
;;{{{  paul pitch range

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format " pr %s as %s "
                    (second setting)
                    (third setting)))))
   '(
     (0 0 0)
     (1 20 10)
     (2 40 20)
     (3 60 30)
     (4 80 40 )
     (5 100 50 )
     (6 137 60)
     (7 174 70)
     (8 211 80)
     (9 250 100)
     ))
  (dectalk-css-set-code-table 'paul 'pitch-range table ))

;;}}}
;;{{{  harry pitch range

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format " pr %s as %s "
                    (second setting)
                    (third setting)))))
   '(
     (0 0 0)
     (1 16 20)
     (2 32 40)
     (3 48 60)
     (4 64 80 )
     (5 80 100 )
     (6 137 100)
     (7 174 100)
     (8 211 100)
     (9 250 100)
     ))
  (dectalk-css-set-code-table 'harry 'pitch-range table ))

;;}}}
;;{{{  betty pitch range

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format " pr %s as %s "
                    (second setting)
                    (third setting)))))
   '(
     (0 0 0)
     (1 50 10)
     (2 80 20)
     (3 100 25)
     (4 110 30 )
     (5 140 35)
     (6 165 57)
     (7 190 75)
     (8 220 87)
     (9 250 100)
     ))
  (dectalk-css-set-code-table 'betty 'pitch-range table ))

;;}}}
(defsubst dectalk-get-pitch-range-code (value family)
  "Get pitch-range code for specified VALUE and FAMILY."
  (or family (setq family 'paul))
  (if value 
      (aref (dectalk-css-get-code-table family 'pitch-range)
            value)
    ""))

;;}}}
;;{{{  stress

;;; On the Dectalk we vary four parameters
;;; The hat rise which controls the overall shape of the F0 contour
;;; for sentence level intonation and stress,
;;; The stress rise that controls the level of stress on stressed
;;; syllables,
;;; the baseline fall for paragraph level intonation
;;; and the quickness --a parameter that controls whether the final
;;; frequency targets are completely achieved in the phonetic
;;; transitions.
;;{{{  paul stress

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format " hr %s sr %s qu %s bf %s "
                    (second setting)
                    (third setting)
                    (fourth setting)
                    (fifth setting)))))
   '(
     (0  0 0 0 0)
     (1 3 6  20 3)
     (2 6 12  40 6)
     (3 9 18  60 9 )
     (4 12 24 80 14)
     (5 18 32  100 18)
     (6 34 50 100 20)
     (7 48  65 100 35)
     (8 63 82 100 60)
     (9 80  90 100  40)
     ))
  (dectalk-css-set-code-table 'paul 'stress table))

;;}}}
;;{{{  harry stress

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format " hr %s sr %s qu %s bf %s "
                    (second setting)
                    (third setting)
                    (fourth setting)
                    (fifth setting)))))
   '(
     (0  0 0 0 0)
     (1 4 6 2 2 )
     (2 8 12 4 4 )
     (3 12 18 6 6 )
     (4 16 24 8 8 )
     (5 20 30 10 9)
     (6 40  48 32 16)
     (7 60 66 54 22)
     (8 80 78 77 34)
     (9 100 100 100 40)
     ))
  (dectalk-css-set-code-table 'harry 'stress table))

;;}}}
;;{{{  betty stress

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format " hr %s sr %s qu %s bf %s "
                    (second setting)
                    (third setting)
                    (fourth setting)
                    (fifth setting)))))
   '(
     (0  1 1 0 0)
     (1 3 4 11 0)
     (2 5 8 22 0)
     (3 8 12 33 0 )
     (4 11  16 44 0)
     (5 14 20 55 0)
     (6 35 40 65 10)
     (7 56 80 75 20)
     (8 77 90 85 30)
     (9 100 100 100 40)
     ))
  (dectalk-css-set-code-table 'betty 'stress table))

;;}}}
(defsubst dectalk-get-stress-code (value family)
  (or family (setq family 'paul ))
  (if value 
      (aref (dectalk-css-get-code-table family 'stress)
            value)
    ""))

;;}}}
;;{{{  richness

;;; Smoothness and richness vary inversely.
;;; a  maximally smooth voice produces a quieter effect
;;; a rich voice is "bright" in contrast.
;;{{{  paul richness

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table (first setting)
            (format " ri %s sm %s "
                    (second setting)
                    (third setting)))))
   '(
     (0 0 100)
     (1 14 80)
     (2 28 60)
     (3 42 40 )
     (4 56 20)
     (5 70  3 )
     (6 60 24 )
     (7 70 16)
     (8 80 8 20)
     (9 100  0)
     ))
  (dectalk-css-set-code-table 'paul 'richness table))

;;}}}
;;{{{  harry richness

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table (first setting)
            (format " ri %s sm %s "
                    (second setting)
                    (third setting)))))
   '(
     (0 100 0)
     (1 96 3)
     (2 93 6)
     (3 90 9)
     (4 88 11)
     (5 86 12)
     (6 60 24 )
     (7 40 44)
     (8 20 65)
     (9 0 70)
     ))
  (dectalk-css-set-code-table 'harry 'richness table))

;;}}}
;;{{{  betty richness

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table (first setting)
            (format " ri %s sm %s "
                    (second setting)
                    (third setting)))))
   '(
     (0 0 100)
     (1 8 76)
     (2 16 52)
     (3 24  28)
     (4 32 10)
     (5 40 4)
     (6 50 3 )
     (7 65 3)
     (8 80 8 2)
     (9 100  0)
     ))
  (dectalk-css-set-code-table 'betty 'richness table))

;;}}}

(defsubst dectalk-get-richness-code (value family)
  (or family (setq family 'paul))
  (if value 
      (aref (dectalk-css-get-code-table family 'richness)
            value)
    ""))

;;}}}
;;{{{  punctuations

(defsubst dectalk-get-punctuations-code (value)
  "Return string needed to set specified punctuations mode."
  (if value 
      (format " :pu %s " value)
    ""))

;;}}}

;;}}}
;;{{{  dectalk-define-voice-from-speech-style

(defun dectalk-define-voice-from-speech-style (name style)
  "Define NAME to be a Dectalk voice as specified by settings in STYLE."
  (let* ((family(acss-family style))
         (command
          (concat "["
                  (dectalk-get-family-code family)
                  (dectalk-get-punctuations-code (acss-punctuations style))
                  (when (or (acss-average-pitch style)
                            (acss-pitch-range style)
                            (acss-stress style )
                            (acss-richness style))
                    (concat " :dv "
                            (dectalk-get-average-pitch-code (acss-average-pitch style) family)
                            (dectalk-get-pitch-range-code (acss-pitch-range style) family)
                            (dectalk-get-stress-code (acss-stress style ) family)
                            (dectalk-get-richness-code (acss-richness style) family)))
                  "]")))
    (dectalk-define-voice name command)))

;;}}}
;;{{{ list voices 

(defun dectalk-list-voices ()
  "List defined voices."
  (declare (special dectalk-voice-table))
  (loop for k being the hash-keys of dectalk-voice-table 
        collect   k))

;;}}}
;;{{{ configurater 

(defun dectalk-configure-tts ()
  "Configures TTS environment to use Dectalk family of synthesizers."
  (declare (special  dectalk-default-speech-rate
                     tts-default-speech-rate))
  (fset 'tts-list-voices 'dectalk-list-voices)
  (fset 'tts-voice-defined-p 'dectalk-voice-defined-p)
  (fset 'tts-get-voice-command 'dectalk-get-voice-command)
  (fset 'tts-voice-defined-p 'dectalk-voice-defined-p)
  (fset 'tts-define-voice-from-speech-style 'dectalk-define-voice-from-speech-style)
  (setq tts-default-speech-rate dectalk-default-speech-rate)
  (set-default 'tts-default-speech-rate dectalk-default-speech-rate))

;;}}}
(provide 'dectalk-voices)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

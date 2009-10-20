;;; espeak-voices.el --- Define various device independent voices in terms of Espeak tags
;;; Description:  Module to set up Espeak voices and personalities
;;; Keywords: Voice, Personality, Espeak
;;{{{  LCD Archive entry:

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2009, T. V. Raman 
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
;;; This module is Espeak specific.

;;}}}
;;{{{ Required modules

;;; Code:
(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'acss-structure)

;;}}}
;;{{{  voice table

(defvar tts-default-voice nil
  "Default voice used. ")

(defvar espeak-default-voice-string ""
  "Default Espeak tag for  default voice.")

(defvar espeak-voice-table (make-hash-table)
  "Association between symbols and strings to set Espeak  voices.
The string can set any voice parameter.")

(defsubst espeak-define-voice (name command-string)
  "Define an Espeak  voice named NAME.
This voice will be set   by sending the string
COMMAND-STRING to the TTS engine."
  (declare (special espeak-voice-table ))
  (puthash name command-string espeak-voice-table))

(defsubst espeak-get-voice-command  (name)
  "Retrieve command string for  voice NAME."
  (declare (special espeak-voice-table))
  (cond
   ((listp name)
    (mapconcat #'espeak-get-voice-command name " "))
   (t (or  (gethash name espeak-voice-table)
           espeak-default-voice-string))))

(defsubst espeak-voice-defined-p (name)
  "Check if there is a voice named NAME defined."
  (declare (special espeak-voice-table ))
  (gethash name espeak-voice-table ))

;;}}}
;;{{{ voice definitions

;;; the nine predefined voices:
(espeak-define-voice 'paul "<voice gender=\"male\" variant=\"1\">")
(espeak-define-voice 'harry "<voice gender=\"male\" variant=\"3\">")
(espeak-define-voice 'dennis "<voice gender=\"male\" variant=\"5\">")
(espeak-define-voice 'frank "<voice gender=\"male\" variant=\"9\">")
(espeak-define-voice 'betty "<voice gender=\"female\" variant=\"1\">")
(espeak-define-voice 'ursula "<voice gender=\"female\" variant=\"5\">")
(espeak-define-voice 'rita "<voice gender=\"female\" variant=\"5\">")
(espeak-define-voice 'wendy "<voice gender=\"female\" variant=\"5\">")
(espeak-define-voice 'kit "<voice gender=\"neutral\">")

;;; Modified voices:

;;}}}
;;{{{  the inaudible voice
;;; no special code needed --handled by Emacspeak engine.

(espeak-define-voice 'inaudible "")

;;}}}
;;{{{  Mapping css parameters to tts codes

;;{{{ voice family codes

(defsubst espeak-get-family-code (name)
  "Get control code for voice family NAME."
  "")

;;}}}
;;{{{  hash table for mapping families to their dimensions

(defvar espeak-css-code-tables (make-hash-table)
  "Hash table holding vectors of espeak codes.
Keys are symbols of the form <FamilyName-Dimension>.
Values are vectors holding the control codes for the 10 settings.")

(defsubst espeak-css-set-code-table (family dimension table)
  "Set up voice FAMILY.
Argument DIMENSION is the dimension being set,
and TABLE gives the values along that dimension."
  (declare (special espeak-css-code-tables))
  (let ((key (intern (format "%s-%s" family dimension))))
    (puthash key table espeak-css-code-tables )))

(defsubst espeak-css-get-code-table (family dimension)
  "Retrieve table of values for specified FAMILY and DIMENSION."
  (declare (special espeak-css-code-tables))
  (let ((key (intern (format "%s-%s" family dimension))))
    (gethash key espeak-css-code-tables)))

;;}}}
;;{{{ volume

;;; Note: volume settings not implemented for Espeak.
(defvar espeak-gain-table (make-vector  10 "")
  "Maps CSS volume settings to actual synthesizer codes.")

;;}}}
;;{{{  average pitch

;;; Average pitch of standard text is aurally mapped to 
;;; a setting of 5.

;;{{{  paul average pitch

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format "<prosody pitch=\"%s\">"
                    (second setting)))))
   '(
     (0 0)
     (1 10)
     (2 20)
     (3 30)
     (4 40)
     (5 50)
     (6 60)
     (7 70)
     (8 80)
     (9 90)))
  (espeak-css-set-code-table 'paul 'average-pitch table ))

;;}}}
;;{{{  harry average pitch

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format "<prosody pitch=\"%s\">"
                    (second setting)))))
   '(
     (0 26)
     (1 32.5)
     (2 39)
     (3 45.5)
     (4 52 )
     (5 58.5)
     (6 65)
     (7 71.5)
     (8 78)
     (9 84.5)))
  (espeak-css-set-code-table 'harry 'average-pitch table ))

;;}}}
;;{{{  betty average pitch

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format "<prosody pitch=\"%s\">"
                    (second setting)))))
   '(
     (0 40)
     (1 48)
     (2 56)
     (3 64)
     (4 72)
     (5 80)
     (6 88)
     (7 96)
     (8 104)
     (9 112)))
  (espeak-css-set-code-table 'betty 'average-pitch table ))

;;}}}

(defsubst espeak-get-average-pitch-code (value family)
  "Get  AVERAGE-PITCH for specified VALUE and  FAMILY."
  (or family (setq family 'paul))
  (if value
      (aref (espeak-css-get-code-table family 'average-pitch)
            value)
    ""))

;;}}}
;;{{{  pitch range
;;; Based on the sampler, it seems this setting is a range of 
;;; values from 0 to 100%, 0 being monotone.

;;{{{  paul pitch range

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format "<prosody range=\"%s\">"
                    (second setting)))))
   '(
     (0 0)
     (1 10)
     (2 30)
     (3 40)
     (4 50)
     (5 60)
     (6 70)
     (7 80)
     (8 90)
     (9 100)))
  (espeak-css-set-code-table 'paul 'pitch-range table ))

;;}}}
;;{{{  harry pitch range

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format "<prosody range=\"%s\">"
                    (second setting)))))
   '(
     (0 0)
     (1 10)
     (2 30)
     (3 40)
     (4 50)
     (5 60)
     (6 70)
     (7 80)
     (8 90)
     (9 100)))
  (espeak-css-set-code-table 'harry 'pitch-range table ))

;;}}}
;;{{{  betty pitch range

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format "<prosody range=\"%s\">"
                    (second setting)))))
   '(
     (0 0)
     (1 10)
     (2 30)
     (3 40)
     (4 50)
     (5 60)
     (6 70)
     (7 80)
     (8 90)
     (9 100)))
  (espeak-css-set-code-table 'betty 'pitch-range table ))

;;}}}
(defsubst espeak-get-pitch-range-code (value family)
  "Get pitch-range code for specified VALUE and FAMILY."
  (or family (setq family 'paul))
  (if value
      (aref (espeak-css-get-code-table family 'pitch-range)
            value)
    ""))

;;}}}
;;{{{  stress

;;;  Not implemented fo Espeak now.

(defsubst espeak-get-stress-code (value family)
  "Just a dummy."
  "")

;;}}}
;;{{{  richness

;;; Smoothness and richness vary inversely.
;;; Richness is currently implemented as volume, with a setting of 0
;;; corresponding to mute.  Smoothness is not implemented.

;;{{{  paul richness

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format "<prosody volume=\"%s\">"
                    (second setting)))))
   ;;            (format " ri:%s sm:%s "
   ;;                    (third setting)))))
   '(
     (0 10 100)
     (1 20 80)
     (2 30 60)
     (3 40 40)
     (4 50 20)
     (5 60 3)
     (6 70 24)
     (7 80 16)
     (8 90 20)
     (9 100  0)))
  (espeak-css-set-code-table 'paul 'richness table))

;;}}}
;;{{{  harry richness

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table (first setting)
            (format "<prosody volume=\"%s\">"
                    (second setting)))))
   ;;            (format " ri:%s sm:%s "
   ;;                     (second setting)
   ;;                     (third setting)))))
   '(
     (0 10 100)
     (1 20 80)
     (2 30 60)
     (3 40 40)
     (4 50 20)
     (5 60 3)
     (6 70 24)
     (7 80 16)
     (8 90 20)
     (9 100  0)))
  (espeak-css-set-code-table 'harry 'richness table))

;;}}}
;;{{{  betty richness

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table (first setting)
            (format "<prosody volume=\"%s\">"
                    (second setting)))))
   ;;            (format " ri:%s sm:%s "
   ;;                     (second setting)
   ;;                     (third setting)))))
   '(
     (0 10 100)
     (1 20 80)
     (2 30 60)
     (3 40 40)
     (4 50 20)
     (5 60 3)
     (6 70 24)
     (7 80 16)
     (8 90 20)
     (9 100  0)))
  (espeak-css-set-code-table 'betty 'richness table))

;;}}}

(defsubst espeak-get-richness-code (value family)
  (or family (setq family 'paul))
  (if value 
      (aref (espeak-css-get-code-table family 'richness)
            value)
    ""))

;;}}}
;;{{{  punctuations

(defsubst espeak-get-punctuations-code (value)
  "Return string needed to set specified punctuations mode."
  "")

;;}}}
;;}}}
;;{{{  espeak-define-voice-from-speech-style

(defun espeak-define-voice-from-speech-style (name style)
  "Define NAME to be a espeak voice as specified by settings in STYLE."
  (let* ((family(acss-family style))
         (command
          ;;          (concat "[_:"
          (concat
           (espeak-get-family-code family)
           " "
           (espeak-get-punctuations-code (acss-punctuations style))
           (espeak-get-average-pitch-code (acss-average-pitch style) family)
           (espeak-get-pitch-range-code (acss-pitch-range style) family)
           (espeak-get-stress-code (acss-stress style ) family)
           (espeak-get-richness-code (acss-richness style) family)
           )))
    ;;                  "]")))
    (espeak-define-voice name command)))

;;}}}
;;{{{ list voices 

(defun espeak-list-voices ()
  "List defined voices."
  (declare (special espeak-voice-table))
  (loop for k being the hash-keys of espeak-voice-table 
        collect   k))

;;}}}
;;{{{ Configurater 

(defvar espeak-character-to-speech-table nil
  "Table that records how ISO ascii characters are spoken.")

(defun espeak-setup-character-to-speech-table ()
  (when (and (null espeak-character-to-speech-table)
             (boundp 'dtk-character-to-speech-table)
             (vectorp dtk-character-to-speech-table))
    (setq espeak-character-to-speech-table
          (let ((table (copy-seq dtk-character-to-speech-table)))
            (loop for entry across-ref table 
                  when   (string-match "\\(\\[\\*\\]\\)"  entry) do
                  (setf entry (replace-match " " nil nil  entry 1)))
            table))))

(defun espeak-configure-tts ()
  "Configure TTS environment to use eSpeak."
  (declare (special tts-default-speech-rate
                    espeak-default-speech-rate
                    dtk-speaker-process
                    emacspeak-unspeakable-rule))
  (fset 'tts-list-voices'espeak-list-voices)
  (fset 'tts-voice-defined-p 'espeak-voice-defined-p)
  (fset 'tts-get-voice-command 'espeak-get-voice-command)
  (fset 'tts-define-voice-from-speech-style 'espeak-define-voice-from-speech-style)
  (setq tts-default-voice nil)
  (setq tts-default-speech-rate espeak-default-speech-rate)
  (set-default 'tts-default-speech-rate espeak-default-speech-rate)
  (espeak-setup-character-to-speech-table)
  (dtk-unicode-update-untouched-charsets '(ascii latin-iso8859-1)))

;;}}}
(provide 'espeak-voices)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

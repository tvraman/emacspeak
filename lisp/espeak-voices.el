;;; espeak-voices.el --- Define  Espeak tags  -*- lexical-binding: t; -*-
;;; Description:  Module to set up Espeak voices and personalities
;;; Keywords: Voice, Personality, Espeak
;;{{{  LCD Archive entry:

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2021, T. V. Raman 
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Introduction:

;;; Commentary:

;;; This module defines the various voices used in voice-lock mode by
;;; the ESpeak TTS engine.

;;; Code:
;;}}}
;;{{{ Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
;;}}}
;;{{{ Customizations:

(defcustom espeak-default-speech-rate 175
  "Default speech rate for eSpeak."
  :group 'tts
  :type 'integer
  :set #'(lambda(sym val)
           (set-default sym val)
           (when (string-match "espeak" dtk-program)
             (setq-default dtk-speech-rate val))))

;;}}}
;;{{{ Top-Level TTS Call

;;;###autoload
(defun espeak ()
  "Start ESpeak."
  (interactive)
  (espeak-configure-tts)
  (ems--fastload "voice-defs")
  (dtk-select-server "espeak")
  (dtk-initialize))

;;}}}
;;{{{  voice table

(defvar tts-default-voice
  "<voice xml:lang=\"$la\" gender=\"male\" variant=\"1\">"
  "Default voice used. ")

(defvar espeak-default-voice-string ""
  "Default Espeak tag for  default voice.")

(defvar espeak-voice-table (make-hash-table)
  "Association between symbols and strings to set Espeak  voices.
The string can set any voice parameter.")

(defun espeak-define-voice (name command-string)
  "Define an Espeak  voice named NAME.
This voice will be set   by sending the string
COMMAND-STRING to the TTS engine."
  (cl-declare (special espeak-voice-table))
  (puthash name command-string espeak-voice-table))

(defun espeak-get-voice-command  (name)
  "Retrieve command string for  voice NAME."
  (cl-declare (special espeak-voice-table))
  (cond
   ((listp name)
    (mapconcat #'espeak-get-voice-command name " "))
   (t (or  (gethash name espeak-voice-table)
           espeak-default-voice-string))))

(defun espeak-voice-defined-p (name)
  "Check if there is a voice named NAME defined."
  (cl-declare (special espeak-voice-table))
  (gethash name espeak-voice-table))

;;}}}
;;{{{ voice definitions

;;; the nine predefined voices:
(espeak-define-voice 'paul "<voice gender=\"male\" variant=\"1\">")

;;; Modified voices:

;;}}}
;;;Mapping css parameters to tts codes
;;{{{ voice family codes

(defun espeak-get-family-code (_name)
  "Get control code for voice family NAME."
  "")

;;}}}
;;{{{  hash table for mapping families to their dimensions

(defvar espeak-css-code-tables (make-hash-table)
  "Hash table holding vectors of espeak codes.
Keys are symbols of the form <FamilyName-Dimension>.
Values are vectors holding the control codes for the 10 settings.")

(defun espeak-css-set-code-table (family dimension table)
  "Set up voice FAMILY.
Argument DIMENSION is the dimension being set,
and TABLE gives the values along that dimension."
  (cl-declare (special espeak-css-code-tables))
  (let ((key (intern (format "%s-%s" family dimension))))
    (puthash key table espeak-css-code-tables)))

(defun espeak-css-get-code-table (family dimension)
  "Retrieve table of values for specified FAMILY and DIMENSION."
  (cl-declare (special espeak-css-code-tables))
  (let ((key (intern (format "%s-%s" family dimension))))
    (gethash key espeak-css-code-tables)))

;;}}}
;;{{{  average pitch

;;; Average pitch of standard text is aurally mapped to 
;;; a setting of 5. These percentage changes interpolate
;;; between the x-low, low, medium, high, and x-high
;;; percentages defined by espeak

;;{{{  paul average pitch

(let ((table (make-vector 10 "")))
  (mapc
   #'(lambda (setting)
       (aset table
             (cl-first setting)
             (format "<prosody pitch=\"%s%%\">"
                     (cl-second setting))))
   '(
     (0 0)
     (1 70) ; x-low
     (2 78)
     (3 85) ; low
     (4 93)
     (5 100) ; medium/default
     (6 105)
     (7 110) ; high
     (8 115)
     (9 120))) ; x-high
  (espeak-css-set-code-table 'paul 'average-pitch table))

;;}}}

(defun espeak-get-average-pitch-code (value family)
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
  (mapc
   #'(lambda (setting)
       (aset table
             (cl-first setting)
             (format "<prosody range=\"%s%%\">"
                     (cl-second setting))))
   '(
     (0 0)
     (1 20) ; x-low
     (2 35)
     (3 50) ; low
     (4 75)
     (5 100) ; medium/default
     (6 120)
     (7 140) ; high
     (8 160)
     (9 180))) ; x-high
  (espeak-css-set-code-table 'paul 'pitch-range table))

;;}}}

(defun espeak-get-pitch-range-code (value family)
  "Get pitch-range code for specified VALUE and FAMILY."
  (or family (setq family 'paul))
  (if value
      (aref (espeak-css-get-code-table family 'pitch-range)
            value)
    ""))

;;}}}
;;{{{  stress

;;;  Not implemented for Espeak now.

(defun espeak-get-stress-code (_value _family)
  "Just a dummy."
  "")

;;}}}
;;{{{  richness

;;; Smoothness and richness vary inversely.
;;; Richness is currently implemented as volume, with a setting of 0
;;; corresponding to mute.  Smoothness is not implemented.

;;{{{  paul richness

(let ((table (make-vector 10 "")))
  (mapc
   #'(lambda (setting)
       (aset table
             (cl-first setting)
             (format "<prosody volume=\"%s\">"
                     (cl-second setting))))
   ;;            (format " ri:%s sm:%s "
   ;;                    (cl-third setting)))))
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

(defun espeak-get-richness-code (value family)
  (or family (setq family 'paul))
  (if value 
      (aref (espeak-css-get-code-table family 'richness)
            value)
    ""))

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
           (espeak-get-average-pitch-code (acss-average-pitch style) family)
           (espeak-get-pitch-range-code (acss-pitch-range style) family)
           (espeak-get-stress-code (acss-stress style) family)
           (espeak-get-richness-code (acss-richness style) family)
           )))
    ;;                  "]")))
    (espeak-define-voice name command)))

;;}}}
;;{{{ Configurater 

(defvar espeak-character-to-speech-table nil
  "Table that records how ISO ascii characters are spoken.")

(defun espeak-setup-character-to-speech-table ()
  (when (and (null espeak-character-to-speech-table)
             (boundp 'dtk-character-to-speech-table)
             (vectorp dtk-character-to-speech-table))
    (setq espeak-character-to-speech-table
          (let ((table (cl-copy-seq  dtk-character-to-speech-table)))
            (cl-loop for entry across-ref table 
                     when   (string-match "\\(\\[\\*\\]\\)"  entry) do
                     (setf entry (replace-match " " nil nil  entry 1)))
            table))))
;;;###autoload
(defun espeak-configure-tts ()
  "Configure  to use eSpeak."
  (cl-declare (special tts-default-speech-rate
                       espeak-default-speech-rate
                       dtk-speaker-process))
  (fset 'tts-voice-defined-p 'espeak-voice-defined-p)
  (fset 'tts-get-voice-command 'espeak-get-voice-command)
  (fset
   'tts-define-voice-from-speech-style 'espeak-define-voice-from-speech-style)
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
;;; end:

;;}}}

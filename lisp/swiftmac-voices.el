;;; swiftmac-voices.el --- Define  Swiftmac tags  -*- lexical-binding: t; -*-
;; $Id: swiftmac-voices.el 6342 2024-04-20 19:12:40Z tv.raman.tv $
;; $Author: Robert Melton $
;; Description:  Module to set up Swiftmac voices and personalities
;; Keywords: Voice, Personality, Swiftmac
;;;   LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; 
;;  $Revision: 4532 $ |
;; Location https://github.com/tvraman/emacspeak
;; 

;;;   Copyright:

;; Copyright (C) 1995 -- 2024, T. V. Raman 
;; All Rights Reserved.
;; 
;; This file is not part of GNU Emacs, but the same permissions apply.
;; 
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Commentary:
;; This module defines the various voices used in voice-lock mode by SwiftMac TTS.

;;; Code:

;;  Required modules: 

(eval-when-compile (require 'cl-lib))
(require 'emacspeak-preamble)           ;For `ems--fastload'.
(cl-declaim  (optimize  (safety 0) (speed 3)))

;;; swiftmac:
;;;###autoload
(defun swiftmac ()
  "SwiftMac TTS."
  (interactive)
  (swiftmac-configure-tts)
  (ems--fastload "voice-defs")
  (dtk-select-server "swiftmac")
  (dtk-initialize))

;;;  Customizations:

(defcustom swiftmac-default-speech-rate 0.65
  "Default speech rate for swiftmac."
  :group 'tts
  :type 'float
  :set #'(lambda(sym val)
           (set-default sym val)
           (when (string-match "swiftmac\\'"dtk-program)
             (setq-default dtk-speech-rate val))))

;;;   voice table

(defvar swiftmac-default-voice-string "[{voice en-US:Alex}]"
  "Default swiftmac tag for  default voice.")

(defvar swiftmac-voice-table (make-hash-table)
  "Association between symbols and strings to set SwiftMac  voices.
The string can set any voice parameter.")

(defun swiftmac-define-voice (name command-string)
  "Define a SwiftMac  voice named NAME.
This voice will be set   by sending the string
COMMAND-STRING to the TTS engine."
  (cl-declare (special swiftmac-voice-table))
  (puthash name command-string swiftmac-voice-table))

(defun swiftmac-get-voice-command-internal  (name)
  "Retrieve command string for  voice NAME."
  (cl-declare (special swiftmac-voice-table))
  (cond
   ((listp name)
    (mapconcat #'swiftmac-get-voice-command name " "))
   (t (or  (gethash name swiftmac-voice-table)
           swiftmac-default-voice-string))))

(defun swiftmac-get-voice-command (name)
  "Retrieve command string for  voice NAME."
  (swiftmac-get-voice-command-internal name))

(defun swiftmac-voice-defined-p (name)
  "Check if there is a voice named NAME defined."
  (cl-declare (special swiftmac-voice-table))
  (gethash name swiftmac-voice-table))

;;;  voice definitions

;; the predefined voices:
(swiftmac-define-voice 'paul  " [{voice en-US:Alex}] [[pitch 1]]")

;; Modified voices:

;;;   Mapping css parameters to tts codes

;;;  voice family codes

(defun swiftmac-get-family-code (name)
  "Get control code for voice family NAME."
  (swiftmac-get-voice-command-internal name))

;;;   hash table for mapping families to their dimensions

(defvar swiftmac-css-code-tables (make-hash-table)
  "Hash table holding vectors of swiftmac codes.
Keys are symbols of the form <FamilyName-Dimension>.
Values are vectors holding the control codes for the 10 settings.")

(defun swiftmac-css-set-code-table (family dimension table)
  "Set up voice FAMILY.
Argument DIMENSION is the dimension being set,
and TABLE gives the values along that dimension."
  (cl-declare (special swiftmac-css-code-tables))
  (let ((key (intern (format "%s-%s" family dimension))))
    (puthash key table swiftmac-css-code-tables)))

(defun swiftmac-css-get-code-table (family dimension)
  "Retrieve table of values for specified FAMILY and DIMENSION."
  (cl-declare (special swiftmac-css-code-tables))
  (let ((key (intern (format "%s-%s" family dimension))))
    (gethash key swiftmac-css-code-tables)))

;;;   average pitch

;; Average pitch for standard male voice is 65 --this is mapped to
;; a setting of 5.
;; Average pitch varies inversely with speaker head size --a child
;; has a small head and a higher pitched voice.
;; We change parameter head-size in conjunction with average pitch to
;; produce a more natural change on the TTS engine.

;;;   paul average pitch

(let ((table (make-vector 10 "")))
  (mapc
   #'(lambda (setting)
       (aset table
             (cl-first setting)
             (format " [[average-pitch %s]] "
                     (cl-second setting))))
   '(
     (0 1)
     (1 10)
     (2 20)
     (3 35)
     (4 40)
     (5 45)
     (6 50)
     (7 55)
     (8 58)
     (9 62)))
  (swiftmac-css-set-code-table 'paul 'average-pitch table))

(defun swiftmac-get-average-pitch-code (value family)
  "Get  AVERAGE-PITCH for specified VALUE and  FAMILY."
  (or family (setq family 'paul))
  (if value 
      (aref (swiftmac-css-get-code-table family 'average-pitch)
            value)
    ""))

;;;   pitch range

;;  Standard pitch range is 30 and is  mapped to
;; a setting of 5.
;; A value of 0 produces a flat monotone voice --maximum value of 100
;; produces a highly animated voice.

;;;   paul pitch range

(let ((table (make-vector 10 "")))
  (mapc
   #'(lambda (setting)
       (aset table
             (cl-first setting)
             (format " [[pitch-range %s]] "
                     (cl-second setting))))
   '(
     (0 0)
     (1 14.1)
     (2  28.2)
     (3  42.3)
     (4  56.4)
     (5  70.5)
     (6  84.6)
     (7 98.7)
     (8  112.8)
     (9  127)))
  (swiftmac-css-set-code-table 'paul 'pitch-range table))

(defun swiftmac-get-pitch-range-code (value family)
  "Get pitch-range code for specified VALUE and FAMILY."
  (or family (setq family 'paul))
  (if value 
      (aref (swiftmac-css-get-code-table family 'pitch-range)
            value)
    ""))

;;;   stress

;;;   paul stress TODO

(let ((table (make-vector 10 "")))
  (mapc
   #'(lambda (setting)
       (aset table
             (cl-first setting)
             (format " [[stress %s %s %s %s]] "
                     (cl-second setting)
                     (cl-third setting)
                     (cl-fourth setting)
                     (cl-fifth setting)
                     )))
   '(
     (0 1 1 0.1 0.1)
     (1 1 1 10 .1)
     (2  1 1 20 .2)
     (3  1 1 30 .2)
     (4  1 1 40 .3)
     (5  1 1 50 .3)
     (6  1 1 60 .3)
     (7  1 1 70 .3)
     (8  1 1 80 .3)
     (9  1 1 90 .3)))
  (swiftmac-css-set-code-table 'paul 'stress table))

(defun swiftmac-get-stress-code (value family)
  (or family (setq family 'paul))
  (if value 
      (aref (swiftmac-css-get-code-table family 'stress)
            value)
    ""))

;;;   richness

;;;   paul richness TODO
(let ((table (make-vector 10 "")))
  (swiftmac-css-set-code-table 'paul 'richness table))

(defun swiftmac-get-richness-code (value family)
  (or family (setq family 'paul))
  (if value 
      (aref (swiftmac-css-get-code-table family 'richness)
            value)
    ""))

;;;   swiftmac-define-voice-from-speech-style

(defun swiftmac-define-voice-from-speech-style (name style)
  "Define NAME to be a swiftmac voice as specified by settings in STYLE."
  (let* ((family(acss-family style))
         (command
          (concat 
           (swiftmac-get-family-code family)
           (swiftmac-get-average-pitch-code (acss-average-pitch style) family)
           (swiftmac-get-pitch-range-code (acss-pitch-range style) family)
           (swiftmac-get-stress-code (acss-stress style) family)
           (swiftmac-get-richness-code (acss-richness style) family))))
    (swiftmac-define-voice name command)))

;;;  Configurater 
;;;###autoload
(defun swiftmac-configure-tts ()
  "Configure TTS  to use swiftmac."
  (cl-declare (special tts-default-speech-rate
                       tts-default-voice swiftmac-default-speech-rate))
  (setq tts-default-voice 'paul)
  (fset 'tts-voice-defined-p 'swiftmac-voice-defined-p)
  (fset 'tts-get-voice-command 'swiftmac-get-voice-command)
  (fset 'tts-define-voice-from-speech-style 'swiftmac-define-voice-from-speech-style)
  (setq tts-default-speech-rate swiftmac-default-speech-rate)
  (set-default 'tts-default-speech-rate swiftmac-default-speech-rate)
  (dtk-unicode-update-untouched-charsets
   '(ascii latin-iso8859-1 latin-iso8859-15 latin-iso8859-9
           eight-bit-graphic))
  (setq emacspeak-play-program nil))

;;;  tts-env for Mac:

(provide 'swiftmac-voices)


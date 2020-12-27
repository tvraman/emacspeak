;;; dectalk-voices.el --- Define various device independent voices in terms of Dectalk codes  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Module to set up Dectalk voices and personalities
;;; Keywords: Voice, Personality, Dectalk
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2018, T. V. Raman
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
;;; Code:

;;}}}
;;{{{ required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))

;;}}}
;;{{{ Customizations:

(defcustom dectalk-default-speech-rate 225
  "Default speech rate . "
  :group 'tts
  :type 'integer
  :set #'(lambda(sym val)
           (set-default sym val)
           (when (and (getenv "DTK_PROGRAM")
                      (string-match "dtk" (getenv "DTK_PROGRAM")))
             (setq-default dtk-speech-rate val))))

;;}}}
;;{{{  Top-level TTS  switcher

;;;###autoload
(defun dectalk ()
  "Dectalk TTS."
  (interactive)
  (dectalk-configure-tts)
  (ems--fastload "voice-defs")
  (dtk-select-server "dtk-exp")
  (dtk-initialize))

;;}}}
;;{{{ Forward declarations:

;;; From dtk-speak.el:

(defvar tts-default-speech-rate)

(defvar dtk-speech-rate-step)
(defvar dtk-speech-rate-base)

;;}}}
;;{{{  voice table

(defvar dectalk-default-voice-string ""
  "Dectalk string for  default voice --set to be a no-op.")

(defvar dectalk-voice-table (make-hash-table)
  "Map symbols to strings  that set Dectalk voices. ")

(defun dectalk-define-voice (name command-string)
  "Map voice name to command-string."
  (cl-declare (special dectalk-voice-table))
  (puthash  name command-string  dectalk-voice-table))

(defun dectalk-get-voice-command (name)
  "Retrieve command string for  voice NAME."
  (cl-declare (special dectalk-voice-table))
  (cond
   ((listp name)
    (mapconcat #'dectalk-get-voice-command name " "))
   (t (or  (gethash name dectalk-voice-table)
           dectalk-default-voice-string))))

(defsubst dectalk-voice-defined-p (name)
  "Check if there is a voice named NAME defined."
  (cl-declare (special dectalk-voice-table))
  (gethash name dectalk-voice-table))

;;}}}
;;{{{ voice definitions

;;; the nine predefined voices:
(dectalk-define-voice 'paul "[:np ]")

;;}}}
;;{{{  the inaudible voice

;;; no special code needed --handled by Emacspeak engine.

(dectalk-define-voice 'inaudible "")

;;}}}
;;{{{  Mapping css parameters to Dectalk codes

;;{{{ voice family codes

(defvar dectalk-family-table nil
  "Association list of Dectalk voice names and control codes.")

(defun dectalk-set-family-code (name code)
  "Set control code for voice family NAME  to CODE."
  (cl-declare (special dectalk-family-table))
  (when (stringp name)
    (setq name (intern name)))
  (setq dectalk-family-table
        (cons (list name code)
              dectalk-family-table)))

(defun dectalk-get-family-code (name)
  "Get control code for voice family NAME."
  (cl-declare (special dectalk-family-table))
  (when (stringp name)
    (setq name (intern name)))
  (or (cadr (assq  name dectalk-family-table))
      ""))

(dectalk-set-family-code 'paul ":np")

;;}}}
;;{{{  hash table for mapping families to their dimensions

(defvar dectalk-css-code-tables (make-hash-table)
  "Hash table holding vectors of Dectalk codes. ")

(defun dectalk-css-set-code-table (family dimension table)
  "Set up voice FAMILY.
Argument DIMENSION is the dimension being set,
and TABLE gives the values along that dimension."
  (cl-declare (special dectalk-css-code-tables))
  (let ((key (intern (format "%s-%s" family dimension))))
    (puthash  key table dectalk-css-code-tables)))

(defun dectalk-css-get-code-table (family dimension)
  "Retrieve table of values for  FAMILY and DIMENSION."
  (cl-declare (special dectalk-css-code-tables))
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
  (mapc
   #'(lambda (setting)
       (aset table
             (cl-first setting)
             (format " ap %s hs % s"
                     (cl-second setting)
                     (cl-third setting))))
   '(
     (0 96 115)
     (1 101 112)
     (2 108 109)
     (3 112 106)
     (4 118 103)
     (5 122  100)
     (6 128 98)
     (7 134 96)
     (8 140 94)
     (9 147 91)
     ))
  (dectalk-css-set-code-table 'paul 'average-pitch table))

;;}}}



;;}}}

(defun dectalk-get-average-pitch-code (value family)
  "Get  AVERAGE-PITCH for  VALUE and  FAMILY."
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
  (mapc
   #'(lambda (setting)
       (aset table
             (cl-first setting)
             (format " pr %s as %s "
                     (cl-second setting)
                     (cl-third setting))))
   '(
     (0 0 0)
     (1 20 10)
     (2 40 20)
     (3 60 30)
     (4 80 40)
     (5 100 50)
     (6 137 60)
     (7 174 70)
     (8 211 80)
     (9 250 100)
     ))
  (dectalk-css-set-code-table 'paul 'pitch-range table))

;;}}}

(defun dectalk-get-pitch-range-code (value family)
  "Get pitch-range code for  VALUE and FAMILY."
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
  (mapc
   #'(lambda (setting)
       (aset table
             (cl-first setting)
             (format " hr %s sr %s qu %s bf %s "
                     (cl-second setting)
                     (cl-third setting)
                     (cl-fourth setting)
                     (cl-fifth setting))))
   '(
     (0  0 0 0 0)
     (1 3 6  20 3)
     (2 6 12  40 6)
     (3 9 18  60 9)
     (4 12 24 80 14)
     (5 18 32  100 18)
     (6 34 50 100 20)
     (7 48  65 100 35)
     (8 63 82 100 60)
     (9 80  90 100  40)
     ))
  (dectalk-css-set-code-table 'paul 'stress table))

;;}}}

(defun dectalk-get-stress-code (value family)
  (or family (setq family 'paul))
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
  (mapc
   #'(lambda (setting)
       (aset table (cl-first setting)
             (format " ri %s sm %s "
                     (cl-second setting)
                     (cl-third setting))))
   '(
     (0 0 100)
     (1 14 80)
     (2 28 60)
     (3 42 40)
     (4 56 20)
     (5 70  3)
     (6 60 24)
     (7 70 16)
     (8 80 8 20)
     (9 100  0)
     ))
  (dectalk-css-set-code-table 'paul 'richness table))

;;}}}

(defun dectalk-get-richness-code (value family)
  (or family (setq family 'paul))
  (if value
      (aref (dectalk-css-get-code-table family 'richness)
            value)
    ""))

;;}}}
;;{{{  punctuations

(defun dectalk-get-punctuations-code (value)
  "Return string needed to set  punctuations mode."
  (if value
      (format " :pu %s " value)
    ""))

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
                            (acss-stress style)
                            (acss-richness style))
                    (concat " :dv "
                            (dectalk-get-average-pitch-code (acss-average-pitch style) family)
                            (dectalk-get-pitch-range-code (acss-pitch-range style) family)
                            (dectalk-get-stress-code (acss-stress style) family)
                            (dectalk-get-richness-code (acss-richness style) family)))
                  "]")))
    (dectalk-define-voice name command)))

;;}}}
;;{{{ configurater

;;;###autoload
(defun dectalk-configure-tts ()
  "Configures   to use Dectalk."
  (cl-declare (special  dectalk-default-speech-rate
                        tts-default-speech-rate tts-default-voice))
  (setq tts-default-voice 'paul)
  (fset 'tts-voice-defined-p 'dectalk-voice-defined-p)
  (fset 'tts-get-voice-command 'dectalk-get-voice-command)
  (fset 'tts-voice-defined-p 'dectalk-voice-defined-p)
  (fset 'tts-define-voice-from-speech-style 'dectalk-define-voice-from-speech-style)
  (setq tts-default-speech-rate dectalk-default-speech-rate)
  (set-default 'tts-default-speech-rate dectalk-default-speech-rate)
  (setq dtk-speech-rate-step 50
        dtk-speech-rate-base 150)
  (setq-default dtk-speech-rate-step 50
                dtk-speech-rate-base 150))

;;}}}

 

(provide 'dectalk-voices)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

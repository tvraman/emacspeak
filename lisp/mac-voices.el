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

;;;Copyright (C) 1995 -- 2011, T. V. Raman 
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
;;; This module is Apple Mac specific.
;;; Code:

;;}}}
;;{{{ Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'acss-structure)

;;}}}
;;{{{  voice table

(defvar mac-default-voice-string "[{voice alex}]"
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


(defsubst mac-voice-defined-p (name)
  "Check if there is a voice named NAME defined."
  (declare (special mac-voice-table ))
  (gethash name mac-voice-table ))

;;}}}
;;{{{ voice definitions

;;; the nine predefined voices: TODO: figure out if embedding is possible (and update voice names).
(mac-define-voice 'paul  " [{voice alex}] ")
(mac-define-voice 'harry " [{voice ralf}] ")
(mac-define-voice 'dennis " [{voice bruce}] ")
(mac-define-voice 'frank " [{voice fred}] ")
(mac-define-voice 'betty " [{voice victoria}] ")
(mac-define-voice 'ursula " [{voice kathy}] ")
(mac-define-voice 'rita " [{voice vicki}] ")
(mac-define-voice 'wendy " [{voice kathy}] ")
(mac-define-voice 'kit " [{voice junior}] ")

;;; Modified voices:

;;}}}
;;{{{  the inaudible voice

;;; Achieve this by setting volume to 0?
(mac-define-voice 'inaudible " [[volm 0.1]] ")

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
;; TODO
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
                    (second setting)))))
   '(
     (0 1)
     (1 10)
     (2 20)
     (3 35)
     (4 40)
     (5 45)
     (6 50)
     (7 55 )
     (8 58)
     (9 62)))
  (mac-css-set-code-table 'paul 'average-pitch table ))

;;}}}
;;{{{  harry average pitch

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format " [[pbas %s]]"
                    (second setting)))))
   '(
     (0 0)
     (1 10 )
     (2 20)
     (3 30)
     (4 40)
     (5 50)
     (6 60)
     (7 70 )
     (8 80)
     (9 90)))
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
                    (second setting)))))
   '(
     (0 5)
     (1 17)
     (2 33)
     (3 49)
     (4 65 )
     (5 81 )
     (6 85)
     (7 89 )
     (8 93)
     (9 97)))
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
            (format " [[pmod %s]] "
                    (second setting)))))
   '(
     (0 0 )
     (1 14.1)
     (2  28.2)
     (3  42.3)
     (4  56.4)
     (5  70.5)
     (6  84.6)
     (7 98.7)
     (8  112.8)
     (9  127)))
  (mac-css-set-code-table 'paul 'pitch-range table ))
(let ((table (make-vector 10 "")))
  (mac-css-set-code-table 'harry 'pitch-range table ))
(let ((table (make-vector 10 "")))
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


;;{{{  paul stress TODO

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
            (format " [{echo %s %s %s %s}] "
                    (second setting)
		    (third setting)
		    (fourth setting)
		    (fifth setting)
))))
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
  (mac-css-set-code-table 'paul 'stress table ))

(let ((table (make-vector 10 "")))
  (mac-css-set-code-table 'harry 'stress table )
  (mac-css-set-code-table 'betty 'stress table ))


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

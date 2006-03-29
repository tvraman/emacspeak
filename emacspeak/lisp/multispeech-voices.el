;;; multispeech-voices.el --- Define various device independent voices in terms of Multispeech tags
;;; Description:  Module to set up Multispeech voices and personalities
;;; Keywords: Voice, Personality, Multispeech
;;{{{  LCD Archive entry:

;;}}}
;;{{{  Copyright:

;;; Initial version: Author: Igor B. Poretsky <master@goga.energo.ru>
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
;;; This module is Multispeech specific.

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

(defvar multispeech-default-voice-string ""
  "Default Multispeech tag for  default voice.")

(defvar multispeech-voice-table (make-hash-table)
  "Association between symbols and strings to set Multispeech  voices.
The string can set any voice parameter.")

(defsubst multispeech-define-voice (name command-string)
  "Define a Multispeech  voice named NAME.
This voice will be set   by sending the string
COMMAND-STRING to the TTS engine."
  (declare (special multispeech-voice-table ))
  (puthash name command-string multispeech-voice-table))

(defsubst multispeech-get-voice-command  (name)
  "Retrieve command string for  voice NAME."
  (declare (special multispeech-voice-table))
  (cond
   ((listp name)
    (mapconcat #'multispeech-get-voice-command name " "))
   (t (or  (gethash name multispeech-voice-table)
           multispeech-default-voice-string))))

(defsubst multispeech-voice-defined-p (name)
  "Check if there is a voice named NAME defined."
  (declare (special multispeech-voice-table ))
  (gethash name multispeech-voice-table ))

;;}}}
;;{{{ voice definitions

;;; the nine predefined voices:
(multispeech-define-voice 'paul "[_: pi:1 fr:16000 ]")
(multispeech-define-voice 'harry "[_: pi:0.5 fr:16000 ]")
(multispeech-define-voice 'dennis "[_: pi:0.7 fr:14000 ]")
(multispeech-define-voice 'frank "[_: pi:0.7 fr:12000 ]")
(multispeech-define-voice 'betty "[_: pi:1.4 fr:17000 ]")
(multispeech-define-voice 'ursula "[_: pi:1.3 fr:16000 ]")
(multispeech-define-voice 'rita "[_: pi:1.4 fr:18000 ]")
(multispeech-define-voice 'wendy "[_: pi:1.5 fr:17000 ]")
(multispeech-define-voice 'kit "[_: pi:2 fr:20000 ]")

;;; Modified voices:

;;}}}
;;{{{  the inaudible voice
;;; no special code needed --handled by Emacspeak engine.

(multispeech-define-voice 'inaudible "")

;;}}}
;;{{{  Mapping css parameters to tts codes

;;{{{ voice family codes

(defsubst multispeech-get-family-code (name)
  "Get control code for voice family NAME."
  "")

;;}}}
;;{{{  hash table for mapping families to their dimensions

(defvar multispeech-css-code-tables (make-hash-table)
  "Hash table holding vectors of multispeech codes.
Keys are symbols of the form <FamilyName-Dimension>.
Values are vectors holding the control codes for the 10 settings.")

(defsubst multispeech-css-set-code-table (family dimension table)
  "Set up voice FAMILY.
Argument DIMENSION is the dimension being set,
and TABLE gives the values along that dimension."
  (declare (special multispeech-css-code-tables))
  (let ((key (intern (format "%s-%s" family dimension))))
    (puthash key table multispeech-css-code-tables )))

(defsubst multispeech-css-get-code-table (family dimension)
  "Retrieve table of values for specified FAMILY and DIMENSION."
  (declare (special multispeech-css-code-tables))
  (let ((key (intern (format "%s-%s" family dimension))))
    (gethash key multispeech-css-code-tables)))

;;}}}
;;{{{ volume

;;; Note: volume settings not implemented for Multispeech.
(defvar multispeech-gain-table (make-vector  10 "")
  "Maps CSS volume settings to actual synthesizer codes.")

;;}}}
;;{{{  average pitch

;;; Average pitch for standard male voice is 122hz --this is mapped to
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
	    (format " pi:%s "
		    (second setting)))))
   '(
     (0 0.5)
     (1 0.6)
     (2 0.7)
     (3 0.8)
     (4 0.9 )
     (5 1)
     (6 1.1)
     (7 1.2)
     (8 1.3)
     (9 1.4)))
  (multispeech-css-set-code-table 'paul 'average-pitch table ))

;;}}}
;;{{{  harry average pitch

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
	    (format " pi:%s "
		    (second setting)))))
   '(
     (0 0.4)
     (1 0.5)
     (2 0.6)
     (3 0.7)
     (4 0.8 )
     (5 0.9)
     (6 1)
     (7 1.1)
     (8 1.2)
     (9 1.3)))
  (multispeech-css-set-code-table 'harry 'average-pitch table ))

;;}}}
;;{{{  betty average pitch

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
	    (first setting)
	    (format " pi:%s "
		    (second setting)))))
   '(
     (0 0.9)
     (1 1)
     (2 1.1)
     (3 1.2)
     (4 1.3 )
     (5 1.4)
     (6 1.5)
     (7 1.6)
     (8 1.7)
     (9 1.8)))
  (multispeech-css-set-code-table 'betty 'average-pitch table ))

;;}}}

(defsubst multispeech-get-average-pitch-code (value family)
  "Get  AVERAGE-PITCH for specified VALUE and  FAMILY."
  (or family (setq family 'paul))
  (if value
      (aref (multispeech-css-get-code-table family 'average-pitch)
	    value)
    ""))

;;}}}
;;{{{  pitch range

;;;  This parameter is modelled by an abstract sampling frequency.

;;;  Standard value is 16000 and is  mapped to
;;; a setting of 5.
;;; A value of 15000 refers to a flat monotone voice --maximum value of 16800
;;; refers to a highly animated voice.

;;{{{  paul pitch range

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
	    (first setting)
	    (format " fr:%s "
		    (second setting)))))
   '(
     (0 15000)
     (1 15200)
     (2 15400)
     (3 15600)
     (4 15800 )
     (5 16000 )
     (6 16200)
     (7 16400)
     (8 16600)
     (9 16800)))
  (multispeech-css-set-code-table 'paul 'pitch-range table ))

;;}}}
;;{{{  harry pitch range

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
	    (first setting)
	    (format " fr:%s "
		    (second setting)))))
   '(
     (0 14000)
     (1 14200)
     (2 14400)
     (3 14600)
     (4 14800)
     (5 15000)
     (6 15200)
     (7 15400)
     (8 15600)
     (9 15800)))
  (multispeech-css-set-code-table 'harry 'pitch-range table ))

;;}}}
;;{{{  betty pitch range

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
	    (format " fr:%s "
		    (second setting)))))
   '(
     (0 17000)
     (1 17200)
     (2 17400)
     (3 17600)
     (4 17800 )
     (5 18000)
     (6 18200)
     (7 18400)
     (8 18600)
     (9 18800)))
  (multispeech-css-set-code-table 'betty 'pitch-range table ))

;;}}}
(defsubst multispeech-get-pitch-range-code (value family)
  "Get pitch-range code for specified VALUE and FAMILY."
  (or family (setq family 'paul))
  (if value
      (aref (multispeech-css-get-code-table family 'pitch-range)
	    value)
    ""))

;;}}}
;;{{{  stress

;;;  Not implemented fo Multispeech now.

(defsubst multispeech-get-stress-code (value family)
  "Just a dummy."
  "")

;;}}}
;;{{{  richness

;;; Smoothness and richness vary inversely.

;;{{{  paul richness

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
            (first setting)
	    (format " ri:%s sm:%s "
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
     (8 80 20)
     (9 100  0)))
  (multispeech-css-set-code-table 'paul 'richness table))

;;}}}
;;{{{  harry richness

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table (first setting)
	    (format " ri:%s sm:%s "
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
  (multispeech-css-set-code-table 'harry 'richness table))

;;}}}
;;{{{  betty richness

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table (first setting)
	    (format " ri:%s sm:%s "
		    (second setting)
		    (third setting)))))
   '(
     (0 0 100)
     (1 8 76)
     (2 16 52)
     (3 24  28)
     (4 32 10)
     (5 40 4)
     (6 50 3)
     (7 65 3)
     (8 80 2)
     (9 100  0)))
  (multispeech-css-set-code-table 'betty 'richness table))

;;}}}

(defsubst multispeech-get-richness-code (value family)
  (or family (setq family 'paul))
  (if value 
      (aref (multispeech-css-get-code-table family 'richness)
	    value)
    ""))

;;}}}
;;{{{  punctuations

(defsubst multispeech-get-punctuations-code (value)
  "Return string needed to set specified punctuations mode."
  "")

;;}}}
;;}}}
;;{{{  multispeech-define-voice-from-speech-style

(defun multispeech-define-voice-from-speech-style (name style)
  "Define NAME to be a multispeech voice as specified by settings in STYLE."
  (let* ((family(acss-family style))
	 (command
	  (concat "[_:"
	   (multispeech-get-family-code family)
	   " "
           (multispeech-get-punctuations-code (acss-punctuations style))
	   (multispeech-get-average-pitch-code (acss-average-pitch style) family)
	   (multispeech-get-pitch-range-code (acss-pitch-range style) family)
	   (multispeech-get-stress-code (acss-stress style ) family)
	   (multispeech-get-richness-code (acss-richness style) family)
	   "]")))
    (multispeech-define-voice name command)))

;;}}}
;;{{{ list voices 

(defun multispeech-list-voices ()
  "List defined voices."
  (declare (special multispeech-voice-table))
  (loop for k being the hash-keys of multispeech-voice-table 
	collect   k))

;;}}}
;;{{{ Configurater 

(defvar russian-spelling-data-loaded-p nil
  "Indicates whether Russian spelling data have already been loaded.")

(defun multispeech-configure-tts ()
  "Configure TTS environment to use multilingual speech server."
  (declare (special tts-default-speech-rate
                    multispeech-default-speech-rate
		    dtk-speaker-process
		    emacspeak-unspeakable-rule
		    emacspeak-speak-default-os-coding-system))
  (fset 'tts-list-voices'multispeech-list-voices)
  (fset 'tts-voice-defined-p 'multispeech-voice-defined-p)
  (fset 'tts-get-voice-command 'multispeech-get-voice-command)
  (fset 'tts-define-voice-from-speech-style 'multispeech-define-voice-from-speech-style)
  (setq tts-default-voice nil)
  (setq tts-default-speech-rate multispeech-default-speech-rate)
  (set-default 'tts-default-speech-rate multispeech-default-speech-rate)
  (setq emacspeak-speak-default-os-coding-system 'cyrillic-koi8)
  (set-process-coding-system dtk-speaker-process 'cyrillic-koi8 'cyrillic-koi8)
  (setq-default dtk-speak-nonprinting-chars nil)
  (unless russian-spelling-data-loaded-p
    (let ((coding-system-for-read 'raw-text))
      (load-library "Russian-spelling"))
    (let ((coding-system-for-read 'cyrillic-koi8))
      (load-library "Russian-spelling"))
    (setq russian-spelling-data-loaded-p t)))

;;}}}
(provide 'multispeech-voices)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

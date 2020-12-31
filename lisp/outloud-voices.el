;;; outloud-voices.el --- Define  OutLoud tags  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Module to set up Eloquent voices and personalities
;;; Keywords: Voice, Personality, IBM ViaVoice Outloud
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2008-07-06 10:18:30 -0700 (Sun, 06 Jul 2008) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2018, T. V. Raman
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
;;;  Interface to outloud server.
;;; This module is IBM ViaVoice Outloud specific.
;;; Code:

;;}}}
;;{{{ Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))

(require 'dtk-unicode)

;;}}}
;;{{{ Customizations:

(defcustom outloud-default-speech-rate 50
  "Default speech rate."
  :group 'tts
  :type 'integer
  :set #'(lambda(sym val)
           (set-default sym val)
           (when (string-match "outloud" dtk-program)
             (setq-default dtk-speech-rate val))))

;;}}}
;;{{{ Top level TTS  switcher

;;;###autoload
(defun outloud (&optional device)
  "Start Outloud."
  (interactive "P")
  (outloud-configure-tts)
  (ems--fastload "voice-defs")
  (funcall-interactively #'dtk-select-server "outloud" device)
  (dtk-initialize))

;;}}}
;;{{{  voice table

(defvar outloud-default-voice-string "`v1"
  "Default voice")

(defvar outloud-voice-table (make-hash-table :test #'eq)
  "Association between symbols and strings to set Outloud  voices. ")

(defun outloud-define-voice (name command-string)
  "Map  voice `name' to `command-string'. "
  (cl-declare (special outloud-voice-table))
  (puthash name command-string outloud-voice-table))

(defun outloud-get-voice-command  (name)
  "Retrieve command string for  voice NAME."
  (cl-declare (special outloud-voice-table))
  (cond
   ((listp name)
    (mapconcat #'outloud-get-voice-command name " "))
   (t (or  (gethash name outloud-voice-table) outloud-default-voice-string))))



(defun outloud-voice-defined-p (name)
  "Check if voice `name' is  defined."
  (cl-declare (special outloud-voice-table))
  (gethash name outloud-voice-table))

;;}}}
;;{{{ voice definitions

(outloud-define-voice 'paul  " `v1 ")

;;}}}
;;;  Mapping css parameters to tts codes
;;{{{  hash table for mapping families to their dimensions

(defvar outloud-css-code-tables (make-hash-table)
  "Hash table holding vectors of outloud codes. ")

(defun outloud-css-set-code-table (family dimension table)
  "Set up voice FAMILY. "
  (cl-declare (special outloud-css-code-tables))
  (let ((key (intern (format "%s-%s" family dimension))))
    (puthash key table outloud-css-code-tables)))

(defun outloud-css-get-code-table (family dimension)
  "Retrieve table of values for  FAMILY and DIMENSION."
  (cl-declare (special outloud-css-code-tables))
  (let ((key (intern (format "%s-%s" family dimension))))
    (gethash key outloud-css-code-tables)))

;;}}}
;;{{{  average pitch

;;; Average pitch for standard male voice is 65 --this is mapped to
;;; a setting of 5.
;;; head-size for default male is 50.
;;; Average pitch varies inversely with speaker head size --a child
;;; has a small head and a higher pitched voice.
;;; We change parameter head-size in conjunction with average pitch to
;;; produce a more natural change on the TTS engine.

;;{{{  paul average pitch
;;; median: pitch: 65  head-size 50
(let ((table (make-vector 10 "")))
  (mapc
   #'(lambda (setting)
       (aset table
             (cl-first setting)
             (format
              " `vb%s `vh%s "
              (cl-second setting) (cl-third setting))))
   '(
     (0 40 75) ; pitch, head-size
     (1 45 70)
     (2 50 65)
     (3 55 60)
     (4 60 55)
     (5 65 50)
     (6 70 45)
     (7 75 40)
     (8 80 35)
     (9 85 30)))
  (outloud-css-set-code-table 'paul 'average-pitch table))

;;}}}



(defun outloud-get-average-pitch-code (value family)
  "Get  AVERAGE-PITCH for  VALUE and  FAMILY."
  (or family (setq family 'paul))
  (if value
      (aref (outloud-css-get-code-table family 'average-pitch)
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
  (mapc
   #'(lambda (setting)
       (aset table
             (cl-first setting)
             (format
              " `vf%s  " (cl-second setting))))
   '(
     (0 0)
     (1 5)
     (2  15)
     (3  20)
     (4  25)
     (5  30)
     (6  47)
     (7  64)
     (8  81)
     (9  100)))
  (outloud-css-set-code-table 'paul 'pitch-range table))

;;}}}


(defun outloud-get-pitch-range-code (value family)
  "Get pitch-range code for  VALUE and FAMILY."
  (or family (setq family 'paul))
  (if value
      (aref (outloud-css-get-code-table family 'pitch-range)
            value)
    ""))

;;}}}
;;{{{  stress

;;; On the outloud we map stress to roughness
;;; we also use stress markers `00 .. `4  (disabled after experimentation)
;;{{{  paul stress
(let ((table (make-vector 10 "")))
  (mapc
   #'(lambda (setting)
       (aset table (cl-first setting)
             (format " `vr%s  "
                     (cl-second setting))))
;;; stress markers not used for now.
   '(
     (0 0 "`00")
     (1 5 "`00")
     (2  10 "`0")
     (3  15 "`0")
     (4  20 "`1")
     (5  25 "`1")
     (6  30 "`v2")
     (7  35 "`v2")
     (8  40 "`v3")
     (9  45 "`v4")))
  (outloud-css-set-code-table 'paul 'stress table))


;;}}}
(defun outloud-get-stress-code (value family)
  (or family (setq family 'paul))
  (if value
      (aref (outloud-css-get-code-table family 'stress)
            value)
    ""))

;;}}}
;;{{{  richness

;;{{{  paul richness

(let ((table (make-vector 10 "")))
  (mapc
   #'(lambda (setting)
       (aset table
             (cl-first setting)
             (format
              " `vy%s  `vv%s "
              (cl-second setting) (cl-third setting))))
   '(; whisper, volume
     (0 0 60)
     (1 4 78)
     (2 8 80)
     (3 12 84)
     (4 16 88)
     (5 20 92)
     (6 24 93)
     (7 28 95)
     (8 32 97)
     (9 36 100)))
  (outloud-css-set-code-table 'paul 'richness table))

;;}}}

(defun outloud-get-richness-code (value family)
  (or family (setq family 'paul))
  (if value
      (aref (outloud-css-get-code-table family 'richness)
            value)
    ""))

;;}}}
;;{{{  outloud-define-voice-from-speech-style

(defun outloud-define-voice-from-speech-style (name style)
  "Define NAME  as  per   STYLE."
  (let* ((family(acss-family style))
         (command
          (concat
           (outloud-get-average-pitch-code (acss-average-pitch style) family)
           (outloud-get-pitch-range-code (acss-pitch-range style) family)
           (outloud-get-stress-code (acss-stress style) family)
           (outloud-get-richness-code (acss-richness style) family))))
    (outloud-define-voice name command)))

;;}}}
;;{{{ Configurater

;;;###autoload
(defun outloud-configure-tts ()
  "Configure TTS  to use Outloud."
  (cl-declare (special tts-default-speech-rate tts-default-voice
                       outloud-default-speech-rate
                       dtk-speech-rate-step dtk-speech-rate-base))
  (fset 'tts-voice-defined-p 'outloud-voice-defined-p)
  (fset 'tts-get-voice-command 'outloud-get-voice-command)
  (fset
   'tts-define-voice-from-speech-style 'outloud-define-voice-from-speech-style)
  (setq tts-default-voice 'paul)
  (setq tts-default-speech-rate outloud-default-speech-rate)
  (set-default 'tts-default-speech-rate outloud-default-speech-rate)
  (setq dtk-speech-rate-step 10
        dtk-speech-rate-base 50
        dtk-speech-rate outloud-default-speech-rate)
  (setq-default dtk-speech-rate-step 10
                dtk-speech-rate outloud-default-speech-rate
                dtk-speech-rate-base 50)
  (dtk-unicode-update-untouched-charsets
   '(ascii latin-iso8859-1 latin-iso8859-15 latin-iso8859-9 eight-bit-graphic)))

;;}}}

(provide 'outloud-voices)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

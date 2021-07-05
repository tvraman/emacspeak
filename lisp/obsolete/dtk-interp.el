;;; dtk-interp.el ---  interface to speech server  -*- lexical-binding: t; -*- 
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Interfacing to the speech server
;;; Keywords: TTS, Dectalk, Speech Server
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2008-03-11 18:41:19 -0700 (Tue, 11 Mar 2008) $ |
;;;  $Revision: 4670 $ |
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ introduction

;;; Commentary:
;;; All requests to the speech server are factored out into
;;; this module.
;;; These calls are declared here as defun so they are
;;; inlined by the byte compiler.
;;; This  keeps the code efficient,
;;; but gives us the flexibility to call out to different
;;; speech servers.

;;; Code:

;;}}}
;;{{{ requires

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))

;;}}}
;;{{{ Forward declarations:

;;; From dtk-speak.el

(defvar dtk-speaker-process)
(defvar dtk-punctuation-mode)

(defvar dtk-split-caps)
(defvar dtk-speech-rate)

;;}}}
;;{{{ macros
  
(defmacro tts-with-punctuations (setting &rest body)
  "Set punctuation mode in  body form."
  (declare (indent 1) (debug t))
  `(let ((save-punctuation-mode dtk-punctuation-mode))
     (unwind-protect
         (progn
           (unless (eq ,setting save-punctuation-mode)
             (dtk-interp-set-punctuations ,setting)
             (setq dtk-punctuation-mode ,setting))
           ,@body)
       (unless (eq ,setting save-punctuation-mode)
         (setq dtk-punctuation-mode save-punctuation-mode)
         (dtk-interp-set-punctuations save-punctuation-mode)))))

;;}}}
;;{{{ silence

(defsubst dtk-interp-silence (duration &optional force)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "sh %d%s\n"
                               duration
                               (if force "\nd" ""))))

;;}}}
;;{{{  tone

(defsubst dtk-interp-tone (pitch duration &optional force)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "t %d %d%s\n"
                               pitch duration
                               (if force "\nd" ""))))
;;}}}
;;{{{  queue

(defsubst dtk-interp-queue (text)
  (cl-declare (special dtk-speaker-process))
  (unless (string-match "^[\s]+$" text)
    (process-send-string dtk-speaker-process (format "q {%s }\n" text))))

(defsubst dtk-interp-queue-code (code)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "c {%s }\n" code)))

(defsubst dtk-interp-queue-set-rate (rate)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "r {%s}\n" rate)))

;;}}}
;;{{{  speak

(defsubst dtk-interp-speak ()
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process "d\n"))

;;}}}
;;{{{ say

(defsubst dtk-interp-say (string)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process (format "tts_say { %s}\n" string)))

;;}}}
;;{{{ stop

(defsubst dtk-interp-stop ()
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process "s\n"))

;;}}}
;;{{{ sync

(defsubst dtk-interp-sync ()
  (cl-declare (special dtk-speaker-process
                       dtk-punctuation-mode dtk-speech-rate dtk-split-caps))
  (process-send-string
   dtk-speaker-process
   (format "tts_sync_state %s %s %s\n"
           dtk-punctuation-mode
           (if dtk-split-caps 1 0)
           dtk-speech-rate)))

;;}}}
;;{{{  letter

(defsubst dtk-interp-letter (letter)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "l {%s}\n" letter)))

;;}}}
;;{{{  language

(defsubst dtk-interp-next-language (&optional say_it)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "set_next_lang %s\n" say_it)))

(defsubst dtk-interp-previous-language (&optional say_it)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "set_previous_lang %s\n" say_it)))

(defsubst dtk-interp-language (language say_it)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "set_lang %s %s \n" language say_it)))

(defsubst dtk-interp-preferred-language (alias language)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "set_preferred_lang %s %s \n" alias language)))

(defsubst dtk-interp-list-language ()
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "list_lang\n")))

;;}}}
;;{{{  rate

(defsubst dtk-interp-say-version ()
  "Speak version."
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process "version\n"))

(defsubst dtk-interp-set-rate (rate)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "tts_set_speech_rate %s\n"
                               rate)))

;;}}}
;;{{{ character scale

(defsubst dtk-interp-set-character-scale (factor)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "tts_set_character_scale %s\n"
                               factor)))

;;}}}
;;{{{  split caps

(defsubst dtk-interp-toggle-split-caps (flag)
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "tts_split_caps %s\n"
                               (if flag 1 0))))

;;}}}
;;{{{ punctuations

(defsubst dtk-interp-set-punctuations (mode)
  (cl-declare (special dtk-speaker-process))
  (process-send-string
   dtk-speaker-process
   (format "tts_set_punctuations %s\nd\n" mode)))

;;}}}
;;{{{ reset

(defsubst dtk-interp-reset-state ()
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process "tts_reset \n"))

;;}}}
(provide 'dtk-interp)
;;{{{  local variables

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

;;; tts.el --- Engine-specific TTS Environment  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Engine-Specific TTS Environment setup.
;;; Keywords: Emacspeak,  Audio Desktop tts-env
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, 2011, T. V. Raman
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
;;; MERCHANTABILITY or FITNTTS-ENV FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:

;;; Define data structure and API for setting up, accessing and manipulating TTS environment.
;;; When complete, this will  be used by the various engine configuration functions to set everything in one structure.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
;;}}}
;;{{{ Structure Definition

(cl-defstruct tts-env
  name default-voice
  default-speech-rate speech-rate-step speech-rate-base
  list-voices acss-voice-defined-p
  get-acss-voice-command define-voice-from-acss)

;;}}}
;;{{{ dtk-Program->Key

(defun tts-env-key (tts-name)
  "Return engine key-name for specified dtk-program."
  (cond
   ((string-match "outloud" tts-name) :outloud)
   ((string-match "dtk" tts-name) :dectalk)
   ((string-match "mac$" tts-name) :mac)
   ((string-match "espeak$" tts-name) :espeak)
   (t :plain)))

;;}}}
;;{{{ TTS Env Table:

;;; Store TTS Env structures, keyed by engine name.

(defvar tts-env-table (make-hash-table :test #'eq)
  "TTS environment table keyed by engine name.")

(defun tts-env-get (engine-name)
  "Return tts-env structure for specified engine."
  (cl-declare (special tts-env-table))
  (or (gethash  engine-name tts-env-table)
      (gethash  :plain tts-env-table)))

(defun tts-env-set (engine-name env)
  "Set up engine-name->env mapping."
  (puthash engine-name  env tts-env-table))

;;}}}
;;{{{ Speaker Process->Env

(defvar tts-env-process-table
  (make-hash-table :test #'eq)
  "Maps speaker processes to their associated tts-env.")

(defun tts-env-set-process-env  (speaker env)
  "Setup speaker->env association."
  (cl-declare (special tts-env-process-table))
  (puthash speaker env tts-env-process-table))

(cl-defun tts-env (&optional (speaker dtk-speaker-process))
  "Return tts-env for this speaker."
  (cl-declare (special tts-env-process-table dtk-speaker-process))
  (or (gethash speaker tts-env-process-table)
      (plain-make-tts-env)))

(defun tts-env-gc-process-env ()
  "Garbage collect tts-env for killed processes."
  (cl-declare (special tts-env-process-table))
  (cl-loop
   for key being the hash-keys of tts-env-process-table
   unless (process-live-p key) do
   (remhash key tts-env-process-table)))

(defvar tts-env-gc-timer
  (run-at-time 1800 1800 #'tts-env-gc-process-env)
  "Idle timer that runs every 30 minutes to cleanup stale tts-env objects.")

;;}}}
;;{{{ TTS State:

(defvar tts-state nil
  "Buffer local tts state.")

(make-variable-buffer-local 'tts-state)

(cl-defstruct tts-state
  rate punctuations   quiet
  capitalize split-caps allcaps
  speak-nonprinting-chars  strip-octals
  chunk-separator pronunciations use-auditory-icons)

;;; tts-state-prototype is used as a place to hold all global defaults.
;;; This prototype instance does not  have  pronunciation-dictionary set.

;;; Pronunciation dictionary is consed at run-time.

(defvar tts-state-prototype 
  (make-tts-state
   :rate  100
   :punctuations  'all
   :quiet  nil
   :capitalize  nil
   :split-caps t
   :allcaps nil
   :speak-nonprinting-chars  nil
   :strip-octals  nil
   :chunk-separator ".>)$\""
   :use-auditory-icons t)
  "Global prototype of tts-state used to initialize new tts-state instances. ")

(cl-defun tts-state (&optional (speaker dtk-speaker-process))
  "Return a default tts-state
appropriately initialized for engine used in this speaker process."
  (cl-declare (special dtk-speaker-process tts-state tts-state-prototype))
  (cond
   ((and (boundp 'tts-state) tts-state) tts-state)
   (t
    (let ((env (tts-env speaker)))
      (setq tts-state (copy-tts-state tts-state-prototype))
      (setf (tts-state-rate tts-state)  (tts-env-default-speech-rate env))
      tts-state))))

;;}}}
;;{{{ tts-env: High-level API

(cl-loop
 for field in
 '(name default-voice
        default-speech-rate speech-rate-step speech-rate-base)
 do
 (eval
  `(defun ,(intern (format "tts-%s" field)) ()
     ,(format "Return %s from tts-env." field)
     (,(intern (format "tts-env-%s" field)) (tts-env)))))

(cl-defun tts-voices ()
  "List voices for speaker."
  (funcall (tts-env-list-voices (tts-env))))

(cl-loop
 for field in
 '(acss-voice-defined-p get-acss-voice-command define-voice-from-acss)
 do
 (eval
  `(defun ,(intern (format "tts-%s" field)) (value)
     ,(format "Return result of applying %s from tts-env to `value'." field)
     (funcall (,(intern (format "tts-env-%s" field)) (tts-env)) value))))

;;}}}
;;{{{ tts-state: High level API

(cl-loop
 for field in
 '(rate punctuations   quiet
        capitalize split-caps allcaps
        speak-nonprinting-chars  strip-octals
        chunk-separator pronunciations use-auditory-icons)
 do
 (eval
  `(defun ,(intern (format "tts-%s" field)) ()
     ,(format "Return %s from tts-state." field)
     (,(intern (format "tts-state-%s" field)) (tts-state)))))

;;}}}
;;{{{ Interactive tts state Mutators:

(cl-loop
 for switch in
 '(quiet capitalize split-caps allcaps
         speak-nonprinting-chars  strip-octals use-auditory-icons)
 do
 (eval
  `(defun ,(intern (format "tts-toggle-%s" switch)) ()
     ,(format "Toggle field %s in current tts-state." switch)
     (interactive)
     (setf
      (,(intern (format "tts-state-%s"  switch)) (tts-state))
      (not (,(intern (format "tts-state-%s"  switch)) (tts-state)))))))

(defun tts-set-rate(rate)
  "Set tts rate."
  (interactive "nRate: ")
  (setf (tts-state-rate (tts-state))rate))

(defun tts-set-punctuations(punctuations)
  "Set tts punctuations."
  (interactive
   (list (read (completing-read "Punctuations: " '(all some none)))))
  (setf (tts-state-punctuations (tts-state))punctuations))

(defun tts-set-chunk-separator(chunk-separator)
  "Set tts chunk-separator."
  (interactive "sChunk-Separator: ")
  (setf (tts-state-chunk-separator (tts-state))chunk-separator))

;;}}}

(provide 'tts)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

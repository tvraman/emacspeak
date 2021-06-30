;;;   -*- Syntax: Common-Lisp; Package: tts ; Mode: LISP -*-    ;;;
;;; tts.lisp -- Common Lisp interface to Emacspeak speech servers
;;; $Author: tv.raman.tv $
;;; Description: Interface Common Lisp to Emacspeak TTS servers
;;; Keywords: Next, Emacspeak, Audio Desktop
;;{{{ Copyright:

;;; Copyright (C) 2011 -- 2018, T. V. Raman<tv.raman.tv@gmail.com>
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING. If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;{{{ Introduction:

;;; Commentary:
;;; Interface Common Lisp to Emacspeak TTS servers

;;}}}
;;{{{Package:
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))
(in-package :cl-user)

(defpackage :tts
  (:use :common-lisp)
  (:export
   #:code #:queue #:speak #:letter #:speak-list #:say #:icon
   #:pause #:stop #:force #:rate #:punctuations
   #:init #:shutdown))

(in-package :tts)

;;}}}
;;{{{ Setup:

(defvar *emacspeak*
  (merge-pathnames "emacs/lisp/emacspeak/" (user-homedir-pathname))
  "Pathname specifying Root of Emacspeak installation.")

(defun tts-location (engine)
  "Return location of specified engine as a string."
  (declare (special *emacspeak*))
  (namestring
   (merge-pathnames (concatenate 'string "servers/" engine)
                    *emacspeak*)))

(defvar *tts* nil
  "Handle to tts server connection.")

(defun tts ()
  "Return handle to TTS server."
  (declare (special *tts*))
  *tts*)

;;; A TTS structure holds the engine name, process handle, and input/output streams.
(defstruct tts
  "Holds handle to TTS server instance."
  engine process
  input output
  rate punctuations)

(defun init (&key (engine "outloud"))
  "Initialize TTS  system."
  (declare (special *tts*))
  (setq *tts* (make-tts :engine (tts-location engine)))
  (tts-open))

;;}}}
;;{{{Internal Functions

(defun tts-open ()
  "Open a TTS session."
  (let ((handle (tts)))
    (setf
     (tts-input handle)
     (sb-ext:process-input
      (sb-ext:run-program (tts-engine handle) nil :wait nil :input :stream)))))

(defun icon-file (icon)
  "Convert auditory icon name to a sound-file name."
  (declare (special *emacspeak*))
  (format nil "~a/sounds/pan-chimes/~a.wav"  *emacspeak* icon))

;;}}}
;;{{{Exported Functions

(defun shutdown ()
  "Shutdown a TTS session."
  (let ((handle (tts)))
    (when (tts-input handle) (close (tts-input handle)))
    (setf (tts-input handle) nil)))

(defun code (cmd)
  "Queue TTS code  to engine."
  (let ((i (tts-input (tts))))
    (unless i (setq i (tts-open)))
    (format i "c {~a}~%" cmd)
    (finish-output i)))

(defun icon (icon)
  "Queue auditory icon  to play."
  (let ((i (tts-input (tts))))
    (unless i (setq i (tts-open)))
    (format i "a {~a}~%" (icon-file icon))
    (finish-output i)))

(defun queue (text)
  "Queue text to speak."
  (let ((i (tts-input (tts))))
    (unless i (setq i (tts-open)))
    (format i "q {~a}~%" text)
    (finish-output i)))

(defun pause (ms)
  "Send silence"
  (let ((i (tts-input (tts))))
    (format i "sh {~a}~%" ms)
    (finish-output i))  )

(defun force ()
  "Speak all queued text."
  (let ((i (tts-input (tts))))
    (format i "d~%" )
    (finish-output i)))

(defun stop ()
  "Stop speech."
  (let ((i (tts-input (tts))))
    (format i "s~%")
    (finish-output i)))

(defun speak (text)
  "Speak text."
  (unless (tts-input (tts)) (tts-open))
  (let ((i (tts-input (tts))))
    (format i "q {~a}~%" text)
    (format i "d~%")
    (finish-output i)))

(defun say (text)
  "Speak text with minimal pre-processing."
  (unless (tts-input (tts)) (tts-open))
  (let ((i (tts-input (tts))))
    (format i "tts_say {~a}~%" text)
    (finish-output i)))

(defun speak-list (lines)
  "Speak an arbitrary number of lines."
  (tts)
  (mapc 'tts-queue lines)
  (force))

(defun letter (text)
  "Speak letter."
  (unless (tts-input (tts)) (tts-open))
  (let ((i (tts-input (tts))))
    (format i "l ~a~%" text)
    (finish-output i)))

(defun rate (rate)
  "Set speech rate."
  (unless (tts-input (tts)) (tts-open))
  (let ((i (tts-input (tts))))
    (setf (tts-rate (tts)) rate)
    (format i "tts_set_speech_rate ~a~%"rate)
    (finish-output i)))

(defun punctuations (mode)
  "Set punctuation mode."
  (unless (tts-input (tts)) (tts-open))
  (let ((i (tts-input (tts))))
    (setf (tts-punctuations (tts)) mode)
    (format i "tts_set_punctuations ~a~%"mode)
    (finish-output i)))

;;}}}
(provide 'tts)

;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

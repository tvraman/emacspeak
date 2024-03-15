;;; pip --- Interface To Piper TTS -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Keywords: Emacspeak,  Audio Desktop Piper TTS
;;; LCD Archive Entry:
;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;; A speech interface to Emacs |
;;  $Revision: 4532 $ |
;; Location https://github.com/tvraman/emacspeak
;;;   Copyright:

;; Copyright (C) 1995 -- 2022, T. V. Raman
;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
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

;;; Commentary:
;; Piper TTS is an Open Source neural-net TTS engine.
;; This module exposes Piper TTS to Emacs.
;;; Code:

;;   Required modules
(unless (executable-find "piper") (error "Piper not installed."))
(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))

(defvar pip-data-dir
  (expand-file-name "~/.local/share/piper/datasets")
  "Where voice models live.")

(defvar pip-voices
  (directory-files pip-data-dir 'full "\\.onnx$")
  "Available voices.")

(defvar pip-pip
  (expand-file-name
   "../servers/piper/pipspeak" (file-name-directory load-file-name))
  "Launch Piper TTS pipeline")

(defvar pip-piper nil "process handle")

(defvar pip-model (cl-first pip-voices)
  "Current voice model.")

(defun pip-model-select (voice)
  "Select default from available choices.
Restarts piper pipeline if already running."
  (interactive
   (list (completing-read "Voice Model:" pip-voices nil t)))
  (cl-declare (special pip-voices pip-model))
  (setq pip-model voice)
  (when (process-live-p pip-piper) (pip-stop))
  (when (called-interactively-p 'interactive)
    (pip-speak (format "Selected voice %s" (file-name-base
  pip-model)))))

(defvar pip-device "tts_mono_left"
  "Alsa device for Piper.")

(defun pip-start ()
  "Start the Piper process"
  (interactive)
  (cl-declare (special  pip-piper pip-model pip-device))
  (unless (process-live-p pip-piper)
    (let ((process-connection-type nil))
      (setq  pip-piper
             (start-process  "pip" nil  pip-pip pip-model pip-device))))
  (when (called-interactively-p 'interactive)
    (pip-speak (format  "Piper is running with %s!" pip-model))))

(defun pip-stop ()
  "Stop Piper TTS"
  (interactive)
  (cl-declare (special pip-piper))
  (delete-process pip-piper))
;;;###autoload
(defun pip-speak (text)
  "Speak text"
  (interactive "sText:")
  (cl-declare (special pip-piper))
  (unless (process-live-p pip-piper) (pip-start))
  (process-send-string pip-piper  (format "%s\n" text))
  (process-send-eof pip-piper))

(provide 'pip)
;;; End of file

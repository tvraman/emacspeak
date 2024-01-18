;;; emacspeak-exec.el --- Executable Locations   -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Keywords: Emacspeak,  Audio Desktop Executables
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Commentary:
;; EXEC ==  Executable
;; Single place to define vars that track the location of various
;; executable  programs used in multiple modules.
;; Programs used in a single module will be declared in that module;
;; will be promoted here if they are used in more than one module.
;;; Code:

;;;   Required modules

(eval-when-compile  (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))

;; Variable names: emacspeak-<prog> as far as possible
;; defvar, not defcustom unless absolutely necessary.
;; amixer

(defvar emacspeak-amixer  (executable-find "amixer") "Amixer program")

;; aplay
(defvar emacspeak-aplay  (executable-find "aplay") "APlay program")

;; curl:
(defvar emacspeak-curl-program (executable-find "curl")
  "Curl.")

;; git:

(defvar emacspeak-git (executable-find "git" "Git Executable"))

;; mpv:
(defvar emacspeak-mpv-program (executable-find "mpv")
  "Name of MPV executable.")

;; xsltproc
(defvar emacspeak-xslt-program (executable-find "xsltproc") "xslt engine")

;; sox, soxi and play

(defvar sox-play (executable-find "play") "Location of play")

(defvar sox-sox (executable-find "sox") "Location of sox")

(defvar sox-soxi (executable-find "soxi") "Location of soxi")

;; youtube-dl
(defvar emacspeak-ytdl (executable-find "youtube-dl") "Youtube DL Executable")

(provide 'emacspeak-exec)
;;;  end of file

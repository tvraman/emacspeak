;;; emacspeak-setup.el --- Setup Emacspeak environment  --loaded to start Emacspeak
;;; $Id$
;;; $Author$ 
;;; Description:  File for setting up and starting Emacspeak
;;; Keywords: Emacspeak, Setup, Spoken Output
;;{{{  LCD Archive entry: 
;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision$ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2000, T. V. Raman 
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

(require 'cl)
(eval-when (compile)
(require 'emacspeak-speak))
(defvar emacspeak-directory
  (expand-file-name  "/user/raman/emacs/lisp/emacspeak")
  "Directory where emacspeak is installed. ")

  (defvar emacspeak-sounds-directory
(concat emacspeak-dir
          "/sounds/")

            "Directory containing auditory icons for Emacspeak.")

(defvar emacspeak-play-program
  (or (getenv "EMACSPEAK_PLAY_PROGRAM")
(concat emacspeak-dir "/play"))
"Name of executable that plays sound files. ")

(unless (featurep 'emacspeak)
(setq load-path
      (cons emacspeak-dir 
                              load-path )))

(load-library "emacspeak")
;;; The next 3    lines are to suppress warnings from the byte-compiler
  (eval-when (compile)
    (require 'dtk-speak)
(require 'voice-lock)
(require 'emacspeak)
)

(defvar dtk-default-speech-rate 225
"*Default speech rate at which Dectalk is started. ")

(add-hook 'dtk-startup-hook 
(function (lambda () 
(dtk-set-rate dtk-default-speech-rate  t))))

(setq emacspeak-startup-hook nil )
;;; Use (add-hook 'emacspeak-startup-hook ...)
;;; to add your personal settings. 
(defvar emacspeak-resource-directory (expand-file-name "~/.emacspeak/")
  "Directory where Emacspeak resource files such as pronunciation dictionaries are stored. ")
(emacspeak)

                  ;;; turn on automatic voice locking , split caps and punctuations
;;; for programming modes
 (mapcar
  (function (lambda (hook)
              (add-hook hook
                        (function (lambda ()
                                    (voice-lock-mode 1)
                                    (dtk-set-punctuations "all")
                                    (or dtk-split-caps
                                        (dtk-toggle-split-caps))
                                    (or emacspeak-audio-indentation
                                        (emacspeak-toggle-audio-indentation))
                                    (emacspeak-dtk-sync))))))
  (list 'c-mode-common-hook
        'py-mode-hook
        'lisp-mode-hook
        'emacs-lisp-mode-hook
        'lisp-interaction-mode-hook
        'perl-mode-hook
        'tex-mode-hook
        'tcl-mode-hook
        'html-helper-mode-hook
        'dired-mode-hook))

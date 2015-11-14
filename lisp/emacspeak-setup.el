;;; emacspeak-setup.el --- Setup Emacspeak environment --loaded to start Emacspeak
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  File for setting up and starting Emacspeak
;;; Keywords: Emacspeak, Setup, Spoken Output
;;{{{  LCD Archive entry:
;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2008-06-06 19:00:23 -0700 (Fri, 06 Jun 2008) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2015, T. V. Raman
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
;;{{{ Introduction

;;; Commentary:
;;; Entry point for Emacspeak.

;;; Code:

;;}}}
;;{{{ Required Modules

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'custom)
(eval-when (compile)
  (require 'emacspeak-preamble))

;;}}}
;;{{{  Define locations

;;;###autoload
(defvar emacspeak-directory
  (expand-file-name "../" (file-name-directory load-file-name))
  "Directory where emacspeak is installed. ")
;;;###autoload
(defvar emacspeak-lisp-directory
  (expand-file-name  "lisp/" emacspeak-directory)
  "Directory where emacspeak lisp files are  installed. ")
;;;###autoload
(defvar emacspeak-sounds-directory
  (expand-file-name  "sounds/" emacspeak-directory)
  "Directory containing auditory icons for Emacspeak.")
;;;###autoload
(defvar emacspeak-xslt-directory
  (expand-file-name "xsl/" emacspeak-directory)
  "Directory holding XSL transformations.")

;;;###autoload
(defvar emacspeak-etc-directory
  (expand-file-name  "etc/" emacspeak-directory)
  "Directory containing miscellaneous files  for
  Emacspeak.")
;;;###autoload
(defvar emacspeak-servers-directory
  (expand-file-name  "servers/" emacspeak-directory)
  "Directory containing speech servers  for
  Emacspeak.")
;;;###autoload
(defvar emacspeak-info-directory
  (expand-file-name  "info/" emacspeak-directory)
  "Directory containing  Emacspeak info files.")
;;;###autoload
(defvar emacspeak-resource-directory (expand-file-name "~/.emacspeak/")
  "Directory where Emacspeak resource files such as
pronunciation dictionaries are stored. ")
;;;###autoload
(defvar emacspeak-readme-file
  (expand-file-name "README"
                    emacspeak-directory)
  "README file from where we get SVN revision number.")

;;;###autoload
(defvar emacspeak-media-extensions
  (concat
   "\\."
   (regexp-opt
    (list "wma"
          "wmv"
          "flv"
          "m4a"
          "m4b"
          "flac"
          "ogg"
          "mp3"
          "MP3"
          "mp4")
    'parens)
   "$")
  "Extensions that match media files.")

;;;###autoload
(defvar emacspeak-codename
  "AnswerDog"
  "Code name of present release.")
;;;###autoload
(defsubst emacspeak-setup-get-revision ()
  "Get SHA checksum of current revision that is suitable for spoken output."
  (let ((default-directory emacspeak-directory))
    (if (and (executable-find "git")
             (file-exists-p (expand-file-name ".git"  emacspeak-directory)))
        (substring 
         (shell-command-to-string  "git show HEAD | head -1 | cut -b 8- ")
         0 6)
      "")))

;;;###autoload
(defsubst emacspeak-xslt-get (style)
  "Return fully qualified stylesheet path."
  (expand-file-name style emacspeak-xslt-directory))

;;;###autoload
(defvar emacspeak-version
  (format
   "42.0 %s:  %s"
   emacspeak-codename
   (emacspeak-setup-get-revision))
  "Version number for Emacspeak.")

;;}}}
;;{{{ speech rate

;;;###autoload
(defcustom outloud-default-speech-rate 50
  "Default speech rate for outloud."
  :group 'tts
  :type 'integer)

;;;###autoload
(defcustom mac-default-speech-rate 225
  "Default speech rate for mac."
  :group 'tts
  :type 'integer)

;;;###autoload
;;;###autoload
(defcustom dectalk-default-speech-rate 225
  "*Default speech rate at which TTS is started. "
  :group 'tts
  :type 'integer)
;;;###autoload
(defcustom espeak-default-speech-rate 175
  "Default speech rate for eSpeak."
  :group 'tts
  :type 'integer)

;;}}}
;;{{{ Hooks

(add-to-list 'load-path emacspeak-lisp-directory )
(add-to-list 'load-path (expand-file-name "g-client" emacspeak-lisp-directory ))

(load-library "emacspeak")

(defvar dtk-startup-hook nil)
;;;###autoload
(defun emacspeak-tts-startup-hook ()
  "Default hook function run after TTS is started."
  (tts-configure-synthesis-setup))

(add-hook 'dtk-startup-hook 'emacspeak-tts-startup-hook)


;;;###autoload
(defcustom emacspeak-tts-use-notify-stream nil
  "Set to true to use a separate TTS stream for notifications."
  :type 'boolean
  :group 'emacspeak)
(defun emacspeak-tts-multistream-p (tts-engine)
  "Checks if this tts-engine can support multiple streams."
  (member tts-engine '("outloud" "32-outloud" "espeak" "mac"
                       "cloud-outloud" "cloud-espeak" "cloud-mac")))

(defun emacspeak-tts-notify-hook ()
  "Starts up a notification stream if current synth supports  multiple invocations.
Should be safe to use with any software TTS engine."
  (declare (special dtk-program dtk-notify-process))
  (unless (emacspeak-tts-multistream-p dtk-program)
    (and (process-live-p dtk-notify-process) (delete-process dtk-notify-process)))
  (when  (emacspeak-tts-multistream-p dtk-program)
    (dtk-notify-initialize)))

(when emacspeak-tts-use-notify-stream
  (add-hook 'dtk-startup-hook 'emacspeak-tts-notify-hook 'at-end))

(defvar emacspeak-startup-hook nil)
(defun emacspeak-setup-header-line ()
  "Set up Emacspeak to show a default header line."
  (declare (special emacspeak-use-header-line
                    header-line-format
                    emacspeak-header-line-format))
  (when emacspeak-use-header-line
    (setq header-line-format emacspeak-header-line-format)))

(defun emacspeak-tvr-startup-hook ()
  "Emacspeak startup hook that I use."
  (load-library "emacspeak-alsaplayer")
  (load-library "emacspeak-webspace")
  (load-library "emacspeak-dbus"))

(add-hook 'emacspeak-startup-hook 'emacspeak-setup-header-line)
(add-hook 'emacspeak-startup-hook 'emacspeak-tvr-startup-hook)

(defvar emacspeak-info-already-loaded nil
  "Track info support load.")

(add-hook
 'Info-mode-hook
 #'(lambda ()
     (unless emacspeak-info-already-loaded
       (load-library "emacspeak-info")
       (setq emacspeak-info-already-loaded t))))

;;}}}
(emacspeak)

(provide 'emacspeak-setup)
;;{{{  emacs local variables

;;; local variables:
;;; major-mode: emacs-lisp-mode
;;; folded-file: t
;;; end:

;;}}}

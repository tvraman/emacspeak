;;; emacspeak-sounds.el --- Defines Emacspeak auditory icons  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Module for adding sound cues to emacspeak
;;; Keywords:emacspeak, audio interface to emacs, auditory icons
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-09-01 15:30:13 -0700 (Sat, 01 Sep 2007) $ |
;;;  $Revision: 4670 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2018, T. V. Raman
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
;;{{{  Introduction:

;;; Commentary:
;;; This module provides the interface for generating auditory icons in emacspeak.
;;; Design goal:
;;; 1) Auditory icons should be used to provide additional feedback,
;;; not as a gimmick.
;;; 2) The interface should be usable at all times without the icons:
;;; e.g. when on a machine without a sound card.
;;; 3) General principle for when to use an icon:
;;; Convey information about events taking place in parallel.
;;; For instance, if making a selection automatically moves the current focus
;;; to the next choice,
;;; We speak the next choice, while indicating the fact that something was selected with a sound cue.
;;;  This interface will assume the availability of a shell command "play"
;;; that can take one or more sound files and play them.
;;; This module will also provide a mapping between names in the elisp world and actual sound files.
;;; Modules that wish to use auditory icons should use these names, instead of actual file names.
;;; As of Emacspeak 13.0, this module defines a themes
;;; architecture for  auditory icons.
;;; Sound files corresponding to a given theme are found in
;;; appropriate subdirectories of emacspeak-sounds-directory

;;}}}
;;{{{ required modules

;;; Code:
(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
;;}}}
;;{{{  state of auditory icons

(defvar emacspeak-use-auditory-icons t
  "Control auditory icons.
Use `emacspeak-toggle-auditory-icons' bound to
\\[emacspeak-toggle-auditory-icons].")

(make-variable-buffer-local 'emacspeak-use-auditory-icons)

;;}}}
;;{{{ Setup Audio 

(defun emacspeak-audio-setup (&optional prefix)
  "Call amixer  command."
  (interactive "P")
  (cond
   ((executable-find "amixer")
    (emacspeak-auditory-icon 'close-object)
    (funcall-interactively #'amixer prefix))
   (t (error "amixer not found."))))

;;}}}
;;{{{  Setup sound themes

(defvar emacspeak-default-sound
  (expand-file-name
   "classic/button.wav"
   emacspeak-sounds-directory)
  "Fallback icon.")

(defvar emacspeak-sounds-themes-table
  (make-hash-table)
  "Maps valid sound themes to the file name extension used by that theme.")

;;;###autoload
(defsubst emacspeak-sounds-define-theme (theme-name file-ext)
  "Define a sounds theme for auditory icons. "
  (cl-declare (special emacspeak-sounds-themes-table))
  (setq theme-name (intern theme-name))
  (setf (gethash  theme-name emacspeak-sounds-themes-table)
        file-ext))

(defcustom emacspeak-sounds-default-theme
  (expand-file-name "pan-chimes/" emacspeak-sounds-directory)
  "Default theme for auditory icons. "
  :type '(directory :tag "Sound Theme Directory")
  :group 'emacspeak)

(defcustom emacspeak-play-program
  (cond
   ((getenv "EMACSPEAK_PLAY_PROGRAM")
    (getenv "EMACSPEAK_PLAY_PROGRAM"))
   ((file-exists-p "/usr/bin/aplay") "/usr/bin/aplay")
   ((file-exists-p "/usr/bin/play") "/usr/bin/play")
   ((file-exists-p "/usr/bin/audioplay") "/usr/bin/audioplay")
   ((file-exists-p "/usr/demo/SOUND/play") "/usr/demo/SOUND/play")
   (t (expand-file-name emacspeak-etc-directory "play")))
  "Name of executable that plays sound files. "
  :group 'emacspeak
  :type 'string)

(defvar emacspeak-sounds-current-theme
  emacspeak-sounds-default-theme
  "Name of current theme for auditory icons.
Do not set this by hand;
--use command \\[emacspeak-sounds-select-theme].")

(defun emacspeak-sounds-theme-get-extension (theme-name)
  "Retrieve filename extension for specified theme. "
  (cl-declare (special emacspeak-sounds-themes-table))
  (gethash
   (intern theme-name)
   emacspeak-sounds-themes-table))

(defun emacspeak-sounds-define-theme-if-necessary (theme-name)
  "Define selected theme if necessary."
    (cond
     ((emacspeak-sounds-theme-get-extension theme-name) t)
     ((file-exists-p (expand-file-name "define-theme.el" theme-name))
      (ems--fastload (expand-file-name "define-theme.el" theme-name)))
     (t (error "Theme %s is missing its configuration file. " theme-name))))

(defun emacspeak-sounds-theme-p  (theme)
  "Predicate to test if theme is available."
  (file-exists-p
   (expand-file-name theme emacspeak-sounds-directory)))


(defun emacspeak-sounds-select-theme  (theme)
  "Select theme for auditory icons."
  (interactive
   (list
    (expand-file-name
     (read-directory-name "Theme: " emacspeak-sounds-directory))))
  (cl-declare (special emacspeak-sounds-current-theme emacspeak-sounds-themes-table))
  (setq theme (expand-file-name theme emacspeak-sounds-directory))
  (unless (file-directory-p theme)
    (setq theme  (file-name-directory theme)))
  (unless (file-exists-p theme)
    (error "Theme %s is not installed" theme))
  (setq emacspeak-sounds-current-theme theme)
  (emacspeak-sounds-define-theme-if-necessary theme)
  (emacspeak-auditory-icon 'select-object))

(defun emacspeak-get-sound-filename (sound-name)
  "Get name of  file that produces  auditory icon SOUND-NAME."
  (cl-declare (special emacspeak-sounds-themes-table
                       emacspeak-sounds-current-theme))
  (let ((f
         (expand-file-name
          (format "%s%s"
                  sound-name
                  (emacspeak-sounds-theme-get-extension emacspeak-sounds-current-theme))
          emacspeak-sounds-current-theme)))
    (cond
     ((file-exists-p f) f)
     (t
      (let ((emacspeak-use-auditory-icons nil))
        (message "Icon %s not defined." sound-name))
      emacspeak-default-sound))))

;;}}}
;;{{{  queue an auditory icon

(defun emacspeak-queue-auditory-icon (sound-name)
  "Queue auditory icon SOUND-NAME."
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "a %s\n"
                               (emacspeak-get-sound-filename sound-name))))

;;}}}
;;{{{  native player (

;;}}}
;;{{{  serve an auditory icon

;;;###autoload
(defun emacspeak-serve-auditory-icon (sound-name)
  "Serve auditory icon SOUND-NAME."
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "p %s\n"
                               (emacspeak-get-sound-filename sound-name))))

;;}}}
;;{{{  Play an icon

(defcustom emacspeak-play-args "-q"
  "Set this to nil if using paplay from pulseaudio."
  :type '(choice (string :tag "Arguments" "-q")
                 (const :tag "None" nil))
  :group 'emacspeak)

(defun emacspeak-play-auditory-icon (sound-name)
  "Produce auditory icon SOUND-NAME."
  (cl-declare (special emacspeak-play-program emacspeak-play-args
                       emacspeak-sounds-directory))
  (let ((process-connection-type nil)
        (default-directory emacspeak-sounds-directory))
    (if emacspeak-play-args
        (start-process
         emacspeak-play-program nil emacspeak-play-program
         emacspeak-play-args
         (emacspeak-get-sound-filename sound-name))
      (start-process
       emacspeak-play-program nil emacspeak-play-program
       (emacspeak-get-sound-filename sound-name)))))
;;;###autoload
(defvar emacspeak-sox (executable-find "sox")
  "Name of SoX executable.")


(defun emacspeak-soxplay-auditory-icon (sound-name)
  "Produce auditory icon SOUND-NAME.
This uses SoX play and is specifically for use with headphones."
  (cl-declare (special emacspeak-sox))
  (let ((icon (emacspeak-get-sound-filename sound-name)))
    (call-process shell-file-name nil nil nil shell-command-switch
                  (format emacspeak-sox icon))))

;;}}}
;;{{{ Play icon list:

;;; For now this is like emacspeak-play-auditory-icon,
;;; i.e. wont work via the speech server,
;;; and consequently not for Emacspeak  on a remote machine.

(defun emacspeak-play-auditory-icon-list (icon-list)
  "Play list of icons."
  (cl-declare (special emacspeak-play-program
                       emacspeak-sounds-directory))
  (let ((default-directory  emacspeak-sounds-directory))
    (apply #'start-process "APlay" nil emacspeak-play-program
           (mapcar #'emacspeak-get-sound-filename icon-list))))

;;}}}
;;{{{  setup play function

(defcustom emacspeak-auditory-icon-function 'emacspeak-serve-auditory-icon
  "*Function that plays auditory icons.
play : Launches play-program to play.
Serve: Send a command to the speech-server to play.
Queue : Add auditory icon to speech queue.
soxplay: Use sox to apply effect earwax for headphones.
Native : Use Emacs' builtin sound support.
Use Serve when working with remote speech servers."
  :group 'emacspeak
  :type '(choice
          (const emacspeak-play-auditory-icon)
          (const emacspeak-serve-auditory-icon)
          (const emacspeak-play-auditory-icon)
          (const emacspeak-soxplay-auditory-icon)
          (const emacspeak-queue-auditory-icon)))

;;;###autoload
(defun emacspeak-auditory-icon (icon)
  "Play an auditory ICON."
  (cl-declare (special emacspeak-auditory-icon-function
                       emacspeak-use-auditory-icons))
  (when emacspeak-use-auditory-icons
    (funcall emacspeak-auditory-icon-function icon)))

;;}}}
;;{{{  toggle auditory icons

;;; This is the main entry point to this module:

(defun emacspeak-toggle-auditory-icons (&optional prefix)
  "Toggle use of auditory icons.
Optional interactive PREFIX arg toggles global value."
  (interactive "P")
  (cl-declare (special emacspeak-use-auditory-icons
                       dtk-program emacspeak-auditory-icon-function))
  (cond
   (prefix
    (setq  emacspeak-use-auditory-icons
           (not emacspeak-use-auditory-icons))
    (setq-default emacspeak-use-auditory-icons
                  emacspeak-use-auditory-icons))
   (t (setq emacspeak-use-auditory-icons
            (not emacspeak-use-auditory-icons))))
  (message "Turned %s auditory icons %s"
           (if emacspeak-use-auditory-icons  "on" "off")
           (if prefix "" "locally"))
  (when emacspeak-use-auditory-icons
    (emacspeak-auditory-icon 'on)))

;;}}}
;;{{{  flush sound driver

;;}}}
;;{{{ emacspeak-prompts:

(defvar emacspeak-prompts-directory 
  (expand-file-name "prompts" emacspeak-sounds-directory)
  "Where pre-defined prompt files are located.")

;;;###autoload 
(defun emacspeak-prompt (name)
  "Play  prompt for specified name."
  (cl-declare (special emacspeak-prompts-directory emacspeak-m-player-program))
  (let  ((file (expand-file-name (format "%s.mp3" name)
                                 emacspeak-prompts-directory)))
    (cl-assert (file-exists-p file) t  "File does not exist")
    (call-process emacspeak-m-player-program nil  0 nil  file)))

;;}}}
(provide  'emacspeak-sounds)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

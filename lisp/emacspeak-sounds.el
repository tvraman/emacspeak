;;; emacspeak-sounds.el --- Defines Emacspeak auditory icons  -*- lexical-binding: t; -*-
;;
;; $Author: tv.raman.tv $
;; Description:  Module for adding sound cues to emacspeak
;; Keywords:emacspeak, audio interface to emacs, auditory icons
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; 
;;  $Revision: 4670 $ |
;; Location undetermined
;; 

;;}}}
;;{{{  Copyright:
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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;{{{  Introduction:

;;; Commentary:
;; This module provides the interface for generating auditory icons in
;; emacspeak.  Design goal: 1) Auditory icons should be used to
;; provide additional feedback, not as a gimmick.  2) The interface
;; should be usable at all times without the icons: e.g. when on a
;; machine without a sound card.  3) General principle for when to use
;; an icon: Convey information about events taking place in parallel.
;; For instance, if making a selection automatically moves the current
;; focus to the next choice, We speak the next choice, while
;; indicating the fact that something was selected with a sound cue.
;; This interface will assume the availability of a shell command
;; "play" that can take one or more sound files and play them.  This
;; module will also provide a mapping between names in the elisp world
;; and actual sound files.  Modules that wish to use auditory icons
;; should use these names, instead of actual file names.  As of
;; Emacspeak 13.0, this module defines a themes architecture for
;; auditory icons.  Sound files corresponding to a given theme are
;; found in appropriate subdirectories of emacspeak-sounds-directory

;;}}}
;;{{{ required modules

;;; Code:
(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
;;}}}
;;{{{  state of auditory icons

(defvar-local emacspeak-use-auditory-icons t
  "Control auditory icons.
Use `emacspeak-toggle-auditory-icons' bound to
\\[emacspeak-toggle-auditory-icons].")

;;}}}
;;{{{  setup play function

(defvar emacspeak-auditory-icon-function #'emacspeak-serve-auditory-icon
  "Function that plays auditory icons.
play : Launches play-program to play.
Serve: Send a command to the speech-server to play.
Queue : Add auditory icon to speech queue.
Use Serve when working with remote speech servers.")
;;;###autoload
(defsubst emacspeak-auditory-icon (icon)
  "Play an auditory ICON."
  (when emacspeak-use-auditory-icons
    (funcall emacspeak-auditory-icon-function icon)))

;;}}}
;;{{{  Setup sound themes
(cl-declaim (special emacspeak-sounds-directory))
(defvar emacspeak-default-sound
  (expand-file-name
   "classic/button.wav"
   emacspeak-sounds-directory)
  "Fallback icon.")

(defvar emacspeak-sounds-themes-table
  (make-hash-table)
  "Maps valid sound themes to the file name extension used by that theme.")

(defun emacspeak-sounds-define-theme (theme-name file-ext)
  "Define a sounds theme for auditory icons. "
  (cl-declare (special emacspeak-sounds-themes-table))
  (setq theme-name (intern theme-name))
  (setf (gethash  theme-name emacspeak-sounds-themes-table)
        file-ext))
(defun emacspeak-sounds-theme-get-extension (theme-name)
  "Retrieve filename extension for specified theme. "
  (cl-declare (special emacspeak-sounds-themes-table))
  (gethash
   (intern theme-name)
   emacspeak-sounds-themes-table))



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
     ((and
       (string= emacspeak-play-program (executable-find "pactl"))
       (string=
        emacspeak-sounds-current-theme
        (expand-file-name "ogg-chimes/" emacspeak-sounds-directory)))
      (file-name-nondirectory f))
     ((file-exists-p f) f)
     (t
      (let ((emacspeak-use-auditory-icons nil))
        (message "Icon %s not defined." sound-name))
      emacspeak-default-sound))))
(defun emacspeak-sounds-define-theme-if-necessary (theme-name)
  "Define selected theme if necessary."
  (cond
   ((emacspeak-sounds-theme-get-extension theme-name) t)
   ((file-exists-p (expand-file-name "define-theme.el" theme-name))
    (load (expand-file-name "define-theme.el" theme-name)))
   (t (error "Theme %s is missing its configuration file. "
             theme-name))))

;;;###autoload
(defun emacspeak-sounds-select-theme  (theme)
  "Select theme for auditory icons."
  (interactive
   (list
    (expand-file-name
     (read-directory-name "Theme: " emacspeak-sounds-directory))))
  (cl-declare (special emacspeak-sounds-current-theme
                       emacspeak-sounds-themes-table
                       emacspeak-play-program
                       emacspeak-sounds-directory))
  (when (string= emacspeak-play-program (executable-find "pactl"))
    (error "Only ogg-chimes with Pulse Advanced."))
  (setq theme (expand-file-name theme emacspeak-sounds-directory))
  (unless (file-directory-p theme)
    (setq theme  (file-name-directory theme)))
  (unless (file-exists-p theme)
    (error "Theme %s is not installed" theme))
  (setq emacspeak-sounds-current-theme theme)
  (emacspeak-sounds-define-theme-if-necessary theme)
  t)


(defcustom emacspeak-play-program
  (or
   (executable-find "aplay")
   (executable-find "paplay")
   (executable-find "play")
   (executable-find "pactl"))
  "Play program."
  :type
  '(choice
    (const :tag "Alsa" "/usr/bin/aplay")
    (const :tag "Pulse Basic" "/usr/bin/paplay")
    (const  :tag "Pulse Advanced" "/usr/bin/pactl")
    (const  :tag "SoX" "/usr/bin/play"))
  :set
  #'(lambda(sym val)
      (cl-declare (special emacspeak-play-args
                           emacspeak-sounds-current-theme))
      (set-default sym val)
      (cond
       ((string= (executable-find "pactl") val)
        (setq emacspeak-play-args "play-sample")
        (setq emacspeak-sounds-current-theme
              (expand-file-name "ogg-chimes/" emacspeak-sounds-directory)))
       ((string= (executable-find "paplay") val)
        (setq emacspeak-play-args nil))
       ((string= (executable-find "aplay") val)
        (setq emacspeak-play-args nil)
        (setq emacspeak-sounds-current-theme
              (expand-file-name "pan-chimes/" emacspeak-sounds-directory)))
       ((string= (executable-find "play") val)
        (setq emacspeak-play-args nil))))
  :group 'emacspeak)

(defvar emacspeak-sounds-current-theme
  (expand-file-name "pan-chimes/" emacspeak-sounds-directory)
  "Name of current theme for auditory icons.
Do not set this by hand;
--use command \\[emacspeak-sounds-select-theme].")

(defun emacspeak-sounds-theme-p  (theme)
  "Predicate to test if theme is available."
  (cl-declare (special emacspeak-sounds-directory))
  (file-exists-p
   (expand-file-name theme emacspeak-sounds-directory)))





;;}}}
;;{{{  queue an auditory icon

(defun emacspeak-queue-auditory-icon (sound-name)
  "Queue auditory icon SOUND-NAME."
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "a %s\n"
                               (emacspeak-get-sound-filename sound-name))))

;;}}}
;;{{{  serve an auditory icon

(defun emacspeak-serve-auditory-icon (sound-name)
  "Serve auditory icon SOUND-NAME."
  (cl-declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
                       (format "p %s\n"
                               (emacspeak-get-sound-filename sound-name))))

;;}}}
;;{{{  Play an icon

(defvar emacspeak-play-args nil
  "Set this to nil if using paplay from pulseaudio.
Automatically set to `play-sample' if using pactl.")

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
(defvar emacspeak-sox (executable-find "sox")
  "Name of SoX executable.")

;;}}}
;;{{{  toggle auditory icons

;; This is the main entry point to this module:

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
;;{{{ emacspeak-prompts:

(defvar emacspeak-prompts-directory 
  (expand-file-name "prompts" emacspeak-sounds-directory)
  "Where pre-defined prompt files are located.")

(defun emacspeak-prompt (name)
  "Play  prompt for specified name."
  (cl-declare (special emacspeak-prompts-directory emacspeak-m-player-program))
  (let  ((file (expand-file-name (format "%s.mp3" name)
                                 emacspeak-prompts-directory)))
    (cl-assert (file-exists-p file) t  "File does not exist")
    (when emacspeak-m-player-program
      (ems-with-environment '(("PULSE_SINK"))
        (call-process
         emacspeak-m-player-program nil  0 nil file)))))

;;}}}
(provide  'emacspeak-sounds)
;;{{{  emacs local variables

;; local variables:
;; folded-file: t
;; end:

;;}}}

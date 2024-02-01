;;; emacspeak-sounds.el --- auditory icons  -*- lexical-binding: t; -*-
;;
;; $Author: tv.raman.tv $
;; Description:  Module for adding sound cues to emacspeak
;; Keywords:emacspeak, audio interface to emacs, auditory icons
;;;   LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;;
;;  $Revision: 4670 $ |
;; Location https://github.com/tvraman/emacspeak
;;

;;;   Copyright:
;; Copyright (C) 1995 -- 2024, T. V. Raman
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
;; This module provides the interface for generating auditory icons in
;; emacspeak.
;; @subsection Design goal:
;;
;; @itemize
;; @item   Auditory icons should be used to
;; provide additional feedback, not as a gimmick.
;; @item   The Emacspeak interface
;; should be usable at all times without the icons.
;; @item   General principle for when to use
;; an icon:
;; @item Convey information about events taking place in parallel.
;;@item  For instance, if making a selection automatically moves the current
;; focus to the next choice, We speak the next choice, while
;; indicating the fact that something was selected with an auditory icon.
;;@item  This interface will assume the availability of a shell command
;; @code{play} that can take one or more sound files and play them.
;; @item This module provides  a mapping between names in the elisp world
;; and actual sound files.
;; @item icon-names are symbols; sound files  are fully-qualified file-names.
;; @item Modules that wish to use auditory icons
;; should use these names, instead of actual file names.
;; @item As of
;; Emacspeak 13.0, this module defines a themes architecture for
;; auditory icons.  Sound files corresponding to a given theme are
;; found in appropriate subdirectories of emacspeak-sounds-dir.
;; @item Contrast this with @code{prompts[ that dont belong to any theme.]}
;; @item The auditory icon player is configure via
;; custom option @code{emacspeak-play-program}.
;;@item  That custom setting handles the mapping to various play programs
;; from audio subsystems such as Pulseaudio, and Pipewire.
;; @end itemize
;; @subsection Designing Auditory Icons
;; Here are some notes on what I have learnt while designing and using
;; auditory icons over the years:
;; @enumerate
;;@item  Auditory icons should be short  --- use command @code{soxi} or
;; @code{;} in a dired buffer to see duration of a sound file. Use
;; the bundled themes as a guide.
;; @item Sounds have many properties, eg: duration, gain, pitch, at
;; the basic level.
;;@item  Even more important is the nature of the sound and what it sounds
;; like in the overall activity where that sound is used.
;; @item This is why  the gain of icons  @emph{should never be} normalized in
;; my view---  tuning icons is as complex as picking
;; colors from  a color palette.
;; @item The included themes have been optimized over years of use and
;; are primarily tuned for using with headphones.
;; @end enumerate
;;; Code:
;;;  required modules

(eval-when-compile (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))

;;;   state of auditory icons

(defvar-local emacspeak-use-auditory-icons t
  "Control auditory icons.
Use `emacspeak-toggle-auditory-icons' bound to
\\[emacspeak-toggle-auditory-icons].")

;;;   setup play function

(defvar emacspeak-auditory-icon-function #'emacspeak-play-auditory-icon
  "Function that plays auditory icons.
play : Launches play-program to play.
Serve: Send a command to the speech-server to play.
Queue : Add auditory icon to speech queue.
Use Serve when working with remote speech servers.")

;;;###autoload
(defun emacspeak-auditory-icon (icon)
  "Play an auditory ICON."
  (cl-declare (special emacspeak-use-auditory-icons ))
  (when emacspeak-use-auditory-icons
    (funcall emacspeak-auditory-icon-function icon)))

;;; Sounds Cache:

(defvar emacspeak-sounds-cache (make-hash-table)
  "Cache sound file names.
Key is a sound-name --- a symbol.
Value is a string, a fully qualified filename. ")

(defsubst emacspeak-sounds-cache-put (sound file)
  "Map  sound to file."
  (cl-declare (special emacspeak-sounds-cache))
  (puthash sound file emacspeak-sounds-cache))

(defsubst emacspeak-sounds-cache-get (sound )
  "Return file that is mapped to sound."
  (cl-declare (special emacspeak-sounds-cache))
  (gethash sound emacspeak-sounds-cache))

(defun emacspeak-sounds-resource (icon)
  "Return icon resource, either a fully qualified file name or a icon-name"
  (cl-declare (special emacspeak-sounds-cache))
  (let ((f (emacspeak-sounds-cache-get icon)))
    (cl-assert (and f (file-exists-p f)) t "Icon does not exist.")
    (if (string= emacspeak-play-program emacspeak-pactl) icon f)))

;;;Sound themes

(defvar emacspeak-sounds-current-theme
  (expand-file-name "pan-chimes/" emacspeak-sounds-dir)
  "Name of current theme for auditory icons, a fully-qualified dir. ")

(cl-declaim (special emacspeak-sounds-dir))



(defvar emacspeak-sounds-themes-table
  (make-hash-table)
  "Maps valid sound themes to the file name extension used by that theme.")

(defsubst emacspeak-sounds-define-theme (theme ext)
  "Define a sounds theme for auditory icons. "
  (cl-declare (special emacspeak-sounds-themes-table))
  (setq theme (intern theme))
  (setf (gethash  theme emacspeak-sounds-themes-table) ext))

(defsubst emacspeak-sounds-theme-ext (theme)
  "Retrieve filename extension for specified theme. "
  (cl-declare (special emacspeak-sounds-themes-table))
  (gethash (intern theme) emacspeak-sounds-themes-table))

(defconst emacspeak-paplay (executable-find "paplay" "PaPlay program"))
(defconst emacspeak-pactl (executable-find "pactl") "PaCtl Executable.")

;; Called from emacspeak at startup, and also when selecting themes.
(defun emacspeak-sounds-cache-rebuild (theme)
  "Rebuild sound cache for theme."
  (emacspeak-sounds-cache-prompts)
  (when (file-exists-p theme)
    (cl-loop
     for f in (directory-files theme 'full (emacspeak-sounds-theme-ext theme))
     do
     (emacspeak-sounds-cache-put
      (intern (file-name-sans-extension (file-name-nondirectory f)))
      f))))

(defun emacspeak-sounds-define-theme-if-necessary (theme-name)
  "Define selected theme if necessary."
  (cl-declare (special  emacspeak-sounds-cache))
  (cond
   ((emacspeak-sounds-theme-ext theme-name) t)
   ((file-exists-p (expand-file-name "define-theme.el" theme-name))
    (load (expand-file-name "define-theme.el" theme-name)))
   (t (error "Theme %s is missing its configuration file. " theme-name))))

;;;###autoload
(defun emacspeak-sounds-select-theme  ( theme)
  "Select theme for auditory icons."
  (interactive
   (list (read-directory-name "Theme: " emacspeak-sounds-dir)))
  (cl-declare (special emacspeak-sounds-themes-table
                       emacspeak-play-program emacspeak-sounds-dir))
  (emacspeak-sounds-define-theme-if-necessary theme)
  (unless (file-directory-p theme) (setq theme  (file-name-directory theme)))
  (unless (file-exists-p theme) (error "Theme %s is not installed" theme))
  (emacspeak-sounds-cache-rebuild theme)
  (when (string= emacspeak-play-program emacspeak-pactl) ; upload samples
    (unless
        (member (file-relative-name theme emacspeak-sounds-dir)
                '("ogg-3d/" "ogg-chimes/"))
      (error "%s: Only ogg-3d or ogg-chimes with Pulse Advanced" theme))
    (cl-loop
     for key being the hash-keys of emacspeak-sounds-cache do
     (shell-command
      (format "%s upload-sample %s %s"
              emacspeak-pactl (gethash key emacspeak-sounds-cache) key))))
  (setq emacspeak-sounds-current-theme theme)
  (emacspeak-auditory-icon 'button))

;; need to use pathnames ---
;; cant use our predefined constants such as emacspeak-paplay here.

(defcustom emacspeak-play-program
  (or emacspeak-pactl  emacspeak-paplay sox-play)
  "Play program."
  :type
  '(choice
    (const :tag "Pulse Basic" "/usr/bin/paplay")
    (const  :tag "Pulse Advanced" "/usr/bin/pactl")
    (const  :tag "SoX" "/usr/bin/play"))
  :set
  #'(lambda(sym val)
      (cl-declare (special emacspeak-play-args emacspeak-sounds-current-theme))
      (set-default sym val)
      (cond
       ((string= emacspeak-pactl val)
        (setq emacspeak-play-args "play-sample")
        (setq emacspeak-sounds-current-theme
              (expand-file-name "ogg-chimes/" emacspeak-sounds-dir)))
       ((string= emacspeak-paplay val)
        (setq emacspeak-sounds-current-theme
              (expand-file-name "ogg-chimes/" emacspeak-sounds-dir))
        (setq emacspeak-play-args nil))
       ((string= sox-play val)
        (setq emacspeak-sounds-current-theme
              (expand-file-name "ogg-chimes/" emacspeak-sounds-dir))
        (setq emacspeak-play-args nil))))
  :group 'emacspeak)

(defun emacspeak-sounds-theme-p  (theme)
  "Predicate to test if theme is available."
  (cl-declare (special emacspeak-sounds-dir))
  (file-exists-p
   (expand-file-name theme emacspeak-sounds-dir)))

;;;   queue an auditory icon
(defun emacspeak-queue-auditory-icon (icon)
  "Queue auditory icon ICON."
  (cl-declare (special dtk-speaker-process))
  (process-send-string
   dtk-speaker-process
   (format "a %s\n" (emacspeak-sounds-resource icon))))

;;;   serve an auditory icon
(defun emacspeak-serve-auditory-icon (icon)
  "Serve auditory icon ICON."
  (cl-declare (special dtk-speaker-process))
  (process-send-string
   dtk-speaker-process
   (format "p %s\n" (emacspeak-sounds-resource icon))))

;;;   Play an icon
(defvar emacspeak-play-args nil
  "Set this to nil if using paplay from pulseaudio.
Automatically set to `play-sample' if using pactl.")

(defun emacspeak-play-auditory-icon (icon)
  "Produce auditory icon ICON."
  (cl-declare (special emacspeak-play-program emacspeak-play-args))
  (let ((process-connection-type nil))
    (if emacspeak-play-args
        (start-process
         emacspeak-play-program nil emacspeak-play-program emacspeak-play-args
         (if (string= emacspeak-play-program emacspeak-pactl)
             (symbol-name icon)
           (emacspeak-sounds-resource icon)))
      (start-process
       emacspeak-play-program nil emacspeak-play-program
       (emacspeak-sounds-resource icon)))))

;;;   toggle auditory icons

;; This is the main entry point to this module:

(defun emacspeak-toggle-auditory-icons (&optional prefix)
  "Toggle use of auditory icons.
Optional interactive PREFIX arg toggles global value."
  (interactive "P")
  (cl-declare (special emacspeak-use-auditory-icons))
  (cond
   (prefix
    (setq  emacspeak-use-auditory-icons (not emacspeak-use-auditory-icons))
    (setq-default emacspeak-use-auditory-icons emacspeak-use-auditory-icons))
   (t
    (setq emacspeak-use-auditory-icons (not emacspeak-use-auditory-icons))))
  (message "Turned %s auditory icons %s"
           (if emacspeak-use-auditory-icons  'on 'off)
           (if prefix "" "locally"))
  (when emacspeak-use-auditory-icons (emacspeak-auditory-icon 'on)))

;;;  emacspeak-prompts:

(defvar emacspeak-prompts-dir
  (expand-file-name "prompts" emacspeak-sounds-dir)
  "Where pre-defined prompt files are located.")

(defun emacspeak-sounds-cache-prompts ()
  "Populate sounds cache with prompts"
  (cl-loop
   for f in
   (directory-files emacspeak-prompts-dir 'full ".ogg$")
   do
   (emacspeak-sounds-cache-put
    (intern (file-name-sans-extension (file-name-nondirectory f)))
    f)))

(defun emacspeak-prompt (name)
  "Play  prompt for specified name."
  (cl-declare (special dtk-program emacspeak-use-auditory-icons ))
  (when emacspeak-use-auditory-icons
    (let  ((f (emacspeak-sounds-resource name)))
      (cond
       ((string-match "cloud" dtk-program)
        (emacspeak-serve-auditory-icon name))
       ((string= emacspeak-play-program emacspeak-pactl)
        (start-process
         "pactl" nil emacspeak-pactl "play-sample" (symbol-name f)))
       (t
        (start-process "paplay" nil emacspeak-paplay  f ))))))

(provide  'emacspeak-sounds)

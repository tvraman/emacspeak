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
;; This module provides the interface for generating auditory icons.
;;  It also defines sound themes for auditory icons.
;; @subsection Design goal:
;;
;; @itemize
;; @item   Auditory icons should be used to
;; provide additional feedback, not as a gimmick.
;; @item   The Emacspeak interface
;; should be usable at all times with the icons turned off.
;; @item  Command @code{emacspeak-toggle-auditory-icons} toggles the
;; use of auditory icons. This flag is buffer-local; use an
;; interactive prefix argosy @code{C-u} to toggle auditory icons on/off
;; globally.
;; @item  Use @code{setq-default emacspeak-use-auditory-icons nil)} to turn
;; auditory icons  off at startup; default is to use auditory icons globally.
;; @item   General principle for using auditory icons:
;; @enumerate
;; @item Convey information about events taking place in parallel.
;;@item  For instance, if making a selection automatically moves the current
;; focus to the next choice, We speak the next choice, while
;; indicating the fact that something was selected with an auditory
;; icon.
;; @item Speed up task completion --- auditory icons take less time than
;; the accompanying spoken feedback.
;; @end enumerate
;; @item This module provides  a mapping between names in the elisp
;; world and actual sound files.
;; @item icon names are symbols,
;; sound files  are strings ---  fully-qualified file-names.
;; @item Modules that  use auditory icons
;;  use icon names and not  actual file names.
;; @item Icons are played either using a local player, or by sending
;; appropriate commands to the speech server (local or cloud).
;; @item  This is determined by the value of @code{emacspeak-play-program}.
;; @item As of
;; Emacspeak 13.0, this module defines a themes architecture for
;; auditory icons.  Sound files corresponding to a given theme are
;; found in appropriate subdirectories of emacspeak-sounds-dir.
;; @item There are two supported themes: @code{ogg-chimes} and @code{ogg-3d}.
;; @item Contrast this with @code{prompts} --- they  dont belong to any theme.
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
;; like in the context of the overall speech output  where that sound is used.
;; @item This is why  the gain of icons  @emph{should never be} normalized in
;; my view---  tuning icons is as complex as picking
;; colors from  a color palette.
;; @item The included themes have been optimized over years of use and
;; are primarily tuned for using with headphones.
;; @end enumerate
;; If @code{emacspeak-play-program} is set to @code{nil},
;; we serve icons, otherwise play
;;them using a local player.
;;; Code:
;;  required modules

(eval-when-compile (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))

;;;   Auditory Icons:

(defvar-local emacspeak-use-auditory-icons t
  "Control auditory icons.
Use `emacspeak-toggle-auditory-icons' bound to
\\[emacspeak-toggle-auditory-icons].")

(defun emacspeak-toggle-auditory-icons (&optional prefix)
  "Toggle use of auditory icons.
Optional interactive PREFIX arg toggles global value."
  (interactive "P")
  (cl-declare (special emacspeak-use-auditory-icons))
  (setq  emacspeak-use-auditory-icons (not emacspeak-use-auditory-icons))
  (when prefix
    (setq-default emacspeak-use-auditory-icons emacspeak-use-auditory-icons))
  (message "Turned %s auditory icons %s"
           (if emacspeak-use-auditory-icons  'on 'off)
           (if prefix "" "locally"))
  (when emacspeak-use-auditory-icons (emacspeak-icon 'on)))

;;;###autoload
(defun emacspeak-icon (icon)
  "Produce an auditory ICON."
  (cl-declare (special emacspeak-use-auditory-icons emacspeak-play-program))
  (when emacspeak-use-auditory-icons
    (if   (null emacspeak-play-program) ; serve icon
        (emacspeak-serve-icon icon)
      (emacspeak-play-icon icon))))

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
  (gethash sound emacspeak-sounds-cache
           (gethash 'button emacspeak-sounds-cache)))

(defun emacspeak-sounds-resource (icon)
  "Return  resource, either a fully qualified file name or a
icon-name as string."
  (cl-declare (special emacspeak-sounds-cache))
  (let ((f (emacspeak-sounds-cache-get icon)))
    (cond                                 
     ((null emacspeak-play-program) f) 
     ((string= emacspeak-play-program emacspeak-pactl) ; pactl->sample-name
      (symbol-name icon))
     (t ; sox-play -> filename
      f))))

;;;Sound themes

(defvar emacspeak-sounds-current-theme
  (expand-file-name "ogg-chimes/" emacspeak-sounds-dir)
  "Current theme for  icons, a fully-qualified directory. ")

(defconst emacspeak-pactl (executable-find "pactl") "PaCtl Executable.")

;; Called when  selecting themes.
(defun emacspeak-sounds-cache-rebuild (theme)
  "Rebuild sound cache for theme, a directory containing sound files."
  (when (file-exists-p theme)
    (cl-loop
     for f in (directory-files theme 'full "\\.ogg$")
     do
     (emacspeak-sounds-cache-put
      (intern (file-name-sans-extension (file-name-nondirectory f))) f))))

;;;###autoload
(defun emacspeak-sounds-select-theme  ( theme)
  "Select theme for auditory icons."
  (interactive
   (list
    (expand-file-name
     (completing-read "Theme: " '("ogg-3c" "ogg-chimes") nil 'must-match)
     emacspeak-sounds-dir)))
  (cl-declare (special emacspeak-play-program emacspeak-sounds-dir))
  (emacspeak-sounds-cache-rebuild theme)
  (when (and emacspeak-play-program   ; avoid nil nil comparison
             (string= emacspeak-play-program emacspeak-pactl)
             (not (string-match "cloud" dtk-program))) ; upload samples
    (cl-loop
     for key being the hash-keys of emacspeak-sounds-cache do
     (shell-command
      (format "%s upload-sample %s %s"
              emacspeak-pactl (gethash key emacspeak-sounds-cache) key))))
  (setq emacspeak-sounds-current-theme theme)
  (emacspeak-icon 'button))

;; need to use explicit pathnames ---
;; can't use our predefined constants such as emacspeak-pactl here.
(defvar ems--play-args nil
  "Arguments passed to play program.
Automatically Set when the player is selected, do not set by hand.")

(defcustom emacspeak-play-program
  (or emacspeak-pactl sox-play)
  "Play program.
Pulse: For systems running Pipewire or Pulseaudio.
sox-play: For systems using SoX as the local player.
None: For systems that rely on the speech server playing the icon."
  :type
  '(choice
    (const  :tag "None" nil)
    (const  :tag "Pulse" "/usr/bin/pactl")
    (const  :tag "SoX" "/usr/local/bin/play"))
  :set
  #'(lambda(sym val)
      (set-default sym val)
      (cond; only 3 valid states:
       ((null val) (setq ems--play-args nil)) ; serve icons
       ((string= emacspeak-pactl val); pactl: play-sample
        (setq ems--play-args "play-sample"))
       ((or  (string= "/usr/bin/play" val); sox-play: play file
             (string= "/usr/local/bin/play" val))
        (setq ems--play-args "-q"))))
  :group 'emacspeak)

;;;  emacspeak-prompts:

(defvar emacspeak-prompts-dir
  (expand-file-name "prompts" emacspeak-sounds-dir)
  "Where pre-defined prompt files are located.")

(defun emacspeak-sounds-cache-prompts ()
  "Populate sounds cache with prompts"
  (emacspeak-sounds-cache-rebuild emacspeak-prompts-dir))


;;; Implementation: emacspeak-icon methods
;;;;   queue an auditory icon
(defun emacspeak-queue-icon (icon)
  "Queue auditory icon ICON.
Used by TTS layer to play icons that are found as text property
`auditory-icon' on text being spoken.
This is a private function and  might go away."
  (cl-declare (special dtk-speaker-process))
  (process-send-string
   dtk-speaker-process
   (format "a %s\n" (emacspeak-sounds-resource icon))))

;;;;   serve an auditory icon
(defun emacspeak-serve-icon (icon)
  "Serve auditory icon ICON."
  (cl-declare (special dtk-speaker-process))
  (process-send-string
   dtk-speaker-process
   (format "p %s\n" (emacspeak-sounds-cache-get icon))))

;;;;   Play an icon

;; Should never be called if local player not available
;; ems--play-args is set when emacspeak-play-program is selected.

(defun emacspeak-play-icon (icon)
  "Produce auditory icon ICON using a local player.
Linux: Pipewire and Pulse: pactl.
without Pipewire/Pulse: play from sox."
  (cl-declare (special emacspeak-play-program ems--play-args))
  (let ((process-connection-type nil))
    (start-process
     "Player" nil emacspeak-play-program
     ems--play-args (emacspeak-sounds-resource icon))))

(provide  'emacspeak-sounds)

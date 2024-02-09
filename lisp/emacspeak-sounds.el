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
;; emacspeak. It also defines sound themes for auditory icons.
;; @subsection Design goal:
;;
;; @itemize
;; @item   Auditory icons should be used to
;; provide additional feedback, not as a gimmick.
;; @item   The Emacspeak interface
;; should be usable at all times with the icons turned off.
;; @item  Command @code{emacspeak-toggle-auditory-icons} toggles the
;; use of auditory icons. This flag is buffer-local; use an
;; interactive prefix argosy @code{C-u} to turn auditory icons on/off
;; globally.
;; Use @code{setq-default emacspeak-use-auditory-icons nil)} to turn
;; these off at startup; default is to use auditory icons globally.
;; @item   General principle:
;; @enumerate
;; @item Convey information about events taking place in parallel.
;;@item  For instance, if making a selection automatically moves the current
;; focus to the next choice, We speak the next choice, while
;; indicating the fact that something was selected with an auditory
;; icon.
;; @item Speed of interaction --- auditory icons take less time than
;; the accompanying spoken feedback.
;; @end enumerate
;; @item This module provides  a mapping between names in the elisp
;; world (symbols)
;; and actual sound files.
;; @item icon-names are symbols; sound files  are fully-qualified file-names.
;; @item Modules that  use auditory icons
;; should use these names and not  actual file names.
;; @item Icons are played either using a local player, or by sending
;; appropriate commands to the speech server (local or cloud).
;; @item  This is determined by the value of emacspeak-auditory-icon-function.
;; @item As of
;; Emacspeak 13.0, this module defines a themes architecture for
;; auditory icons.  Sound files corresponding to a given theme are
;; found in appropriate subdirectories of emacspeak-sounds-dir.
;; @item Contrast this with @code{prompts[ that dont belong to any theme.]}
;; @item The auditory icon player is configure via
;; custom option @code{emacspeak-play-program}.
;;@item  That custom setting handles the mapping to various play
;; backends (local or cloud)
;; from audio subsystems such as Pulseaudio and Pipewire.
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
  (cl-declare (special emacspeak-use-auditory-icons emacspeak-play-program
                       emacspeak-auditory-icon-function))
  (when emacspeak-use-auditory-icons
    (when (and (null emacspeak-play-program)
               (eq emacspeak-auditory-icon-function
                   'emacspeak-play-auditory-icon))
      ;; expecting a local player but none available, so turn off icons.
      (setq-default emacspeak-use-auditory-icons nil)
      (message "No valid player for auditory icons."))
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
  "Return icon resource, either a fully qualified file name or a
icon-name as string."
  (cl-declare (special emacspeak-sounds-cache))
  (let ((f (emacspeak-sounds-cache-get icon)))
         (cl-assert  f  t "Icon does not exist.")
    (cond 
     ((and  emacspeak-play-program      ; avoid nil nil comparison
            (string= emacspeak-play-program emacspeak-pactl)) ; pactl -> icon  
      (symbol-name icon))
     (t  f))))

;;;Sound themes

(defvar emacspeak-sounds-current-theme
  (expand-file-name "ogg-chimes/" emacspeak-sounds-dir)
  "Name of current theme for auditory icons, a fully-qualified dir. ")

(cl-declaim (special emacspeak-sounds-dir))

(defconst emacspeak-pactl (executable-find "pactl") "PaCtl Executable.")

;; Called from emacspeak at startup, and also when selecting themes.
(defun emacspeak-sounds-cache-rebuild (theme)
  "Rebuild sound cache for theme."
  (emacspeak-sounds-cache-prompts)
  (when (file-exists-p theme)
    (cl-loop
     for f in (directory-files theme 'full ".ogg")
     do
     (emacspeak-sounds-cache-put
      (intern (file-name-sans-extension (file-name-nondirectory f)))
      f))))

(defun emacspeak-sounds-define-theme-if-necessary (theme)
  "Define selected theme if necessary."
  (if (file-exists-p (expand-file-name "define-theme.el" theme))
      (load (expand-file-name "define-theme.el" theme))
    (message "Theme %s is missing its configuration file. " theme)))

;;;###autoload
(defun emacspeak-sounds-select-theme  ( theme)
  "Select theme for auditory icons."
  (interactive
   (list
    (expand-file-name
     (completing-read "Theme: " '("ogg-3c" "ogg-chimes"))
     emacspeak-sounds-dir)))
  (cl-declare (special emacspeak-play-program emacspeak-sounds-dir))
  (emacspeak-sounds-define-theme-if-necessary theme)
  (unless (file-directory-p theme) (setq theme  (file-name-directory theme)))
  (unless (file-exists-p theme) (message "Theme %s is not installed" theme))
  (emacspeak-sounds-cache-rebuild theme)
  (when (and emacspeak-play-program     ; avoid nil nil comparison
             (string= emacspeak-play-program emacspeak-pactl)
             (not (string-match "cloud" dtk-program))) ; upload samples
    (cl-loop
     for key being the hash-keys of emacspeak-sounds-cache do
     (shell-command
      (format "%s upload-sample %s %s"
              emacspeak-pactl (gethash key emacspeak-sounds-cache) key))))
  (setq emacspeak-sounds-current-theme theme)
  (emacspeak-auditory-icon 'button))

;; need to use explicit pathnames ---
;; can't use our predefined constants such as emacspeak-pactl here.

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
      (cond ; todo: should we reset icon player  when prog  becomes non-null
       ((null  val)                     ; no local player. Use server
        (setq emacspeak-auditory-icon-function #'emacspeak-serve-auditory-icon))
       ((string= emacspeak-pactl val)
        (setq emacspeak-play-args "play-sample"))
       ((or  (string= "/usr/bin/play" val)
             (string= "/usr/local/bin/play" val))
        (setq emacspeak-play-args "-q"))))
  :group 'emacspeak)

(defsubst emacspeak-sounds-theme-p  (theme)
  "Predicate to test if theme is available."
  (cl-declare (special emacspeak-sounds-dir))
  (file-exists-p
   (expand-file-name theme emacspeak-sounds-dir)))

;;;   queue an auditory icon
(defun emacspeak-queue-auditory-icon (icon)
  "Queue auditory icon ICON.
Used by TTS layer to play icons that are found as text property
`auditory-icon' on text being spoken"
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
  "Arguments passed to play program.")
;; Should never be called if local player not available

(defun emacspeak-play-auditory-icon (icon)
  "Produce auditory icon ICON using a local player.
Linux: Pipewire and Pulse: pactl.
Mac, Linux without Pipewire/Pulse: play from sox."
  (cl-declare (special emacspeak-play-program emacspeak-play-args))
  (let ((process-connection-type nil))
        (start-process
         emacspeak-play-program nil emacspeak-play-program
         emacspeak-play-args (emacspeak-sounds-resource icon))))

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
    (cond
     ((string-match "cloud" dtk-program) (emacspeak-serve-auditory-icon name))
     ((and emacspeak-play-program
           (string= emacspeak-play-program emacspeak-pactl))
      (start-process
       "prompt" nil emacspeak-pactl emacspeak-play-args
       (emacspeak-sounds-resource name))))))

(provide  'emacspeak-sounds)

;;; emacspeak-m-player.el --- Control mplayer from Emacs  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description: Controlling mplayer from emacs
;;; Keywords: Emacspeak, m-player streaming media
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2008-06-29 17:58:19 -0700 (Sun, 29 Jun 2008) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (c) 1995 -- 2018, T. V. Raman
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ Introduction:

;;; Commentary:

;;; Defines an Emacspeak front-end for interacting with @code{mplayer}.
;;; Program @code{mplayer}  is a versatile media player capable of playing many
;;; streaming media formats.
;;; This module provides complete access to all @code{mplayer} functionality
;;; from a convenient Emacs interface.
;;;
;;;@subsection Usage
;;;
;;; The main entry-point is command @code{emacspeak-multimedia}
;;;bound to @kbd{C-e ;}.
;;; This prompts for and launches the desired media stream.
;;; Once a stream is playing, you can control it with single-letter keystrokes
;;; in the @code{*M-Player*} buffer.
;;; Alternatively, you can switch away from that buffer to do real work,
;;; And invoke @code{m-player} commands by  first pressing @kbd{C-e ;}.
;;; As an example, pressing @kbd{v} in the @code{*M-Player*} buffer
;;; prompts for and sets the volume;
;;; When not in the @code{*M-Player*} buffer, you can achieve the same
;;; by pressing @kbd{C-e ; v}.
;;; Press @kbd{C-h b} in the @code{*M-Player*}
;;; buffer  to list  @code{m-player} keybindings.
;;;
;;; Code:

;;}}}
;;{{{  Required modules
(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'ansi-color)
(require 'ladspa)
(require 'ido)
(require 'emacspeak-amark)
(require 'locate)
(require 'comint)

;;}}}
;;{{{ Stream Metadata:

(cl-defstruct emacspeak-m-player-metadata
  title artist album info
  year comment track genre)

(defvar-local emacspeak-m-player-metadata nil
  "Instance of stream metadata for this buffer.")


(defun emacspeak-m-player-display-metadata ()
  "Display metadata after refreshing it if needed."
  (interactive)
  (let ((data (emacspeak-m-player-refresh-metadata)))
    (with-output-to-temp-buffer "M Player Metadata"
      (cl-loop
       for f in
       (cl-rest (mapcar #'car (cl-struct-slot-info 'emacspeak-m-player-metadata)))
       do
       (when (cl-struct-slot-value 'emacspeak-m-player-metadata f data)
         (princ
          (format
           "%s:\t%s\n"
           f
           (cl-struct-slot-value 'emacspeak-m-player-metadata f data))))))
    (message "Displayed metadata in other window.")
    (emacspeak-auditory-icon 'task-done)))

;;}}}
;;{{{ define a derived mode for m-player interaction

(defconst  emacspeak-media-shortcuts-directory
  (expand-file-name "media/radio/" emacspeak-directory)
  "*Directory where we organize   and media shortcuts. ")

(defvar emacspeak-m-player-process nil
  "Process handle to m-player.")

(defun emacspeak-m-player-dispatch (command)
  "Dispatch command to m-player."
  (cl-declare (special emacspeak-m-player-process))
  (with-current-buffer (process-buffer emacspeak-m-player-process)
    (erase-buffer)
    (process-send-string emacspeak-m-player-process
                         (format "pausing_keep %s\n" command))
    (accept-process-output emacspeak-m-player-process 0.1)
    (unless (zerop (buffer-size))
      (buffer-substring-no-properties (point-min) (1-  (point-max))))))

(defvar-local  emacspeak-m-player-current-directory nil
  "Records current directory of media being played.
This is set to nil when playing Internet  streams.")

(defsubst ems--seconds-string-to-duration (sec)
  "Return seconds formatted as time if valid, otherwise return as is."
  (let ((v (car  (read-from-string sec))))
    (cond
     ((and (numberp v) (not (cl-minusp v)))
      (format-seconds "%.2h:%.2m:%.2s%z" v))
     (t sec))))

(defsubst ems--duration-to-seconds (d)
  "Convert hh:mm:ss to seconds."
  (let*
      ((sign (string-match "^-" d))
       (v
        (mapcar
         #'car
         (mapcar
          #'read-from-string (split-string (if sign (substring d 1) d) ":")))))
    (* (if sign -1 1)
       (+
        (* 3600 (cl-first v))
        (* 60 (cl-second v))
        (cl-third v)))))

(defun emacspeak-m-player-mode-line ()
  "Meaningful mode-line for *M-Player* buffers."
  (interactive)
  (cl-declare (special emacspeak-m-player-process))
  (dtk-notify-speak
   (cond
    ((eq 'run (process-status emacspeak-m-player-process))
     (let ((info (emacspeak-m-player-get-position)))
       (when info
         (concat
          (propertize "Position:  " 'pause 90)
          (ems--seconds-string-to-duration (cl-first info))
          (propertize " of " 'personality voice-smoothen-extra)
          (ems--seconds-string-to-duration (cl-third info))
          (propertize " in " 'personality voice-smoothen-extra)
          (cl-second info)))))
    (t (format "Process MPlayer not running.")))))



(define-derived-mode emacspeak-m-player-mode comint-mode
  "M-Player Interaction"
  "Major mode for m-player interaction. \n\n
\\{emacspeak-m-player-mode-map}"
  (progn
    (setq emacspeak-m-player-metadata (make-emacspeak-m-player-metadata))
    (setq buffer-undo-list t)))

;;}}}
;;{{{ emacspeak-m-player


(defgroup emacspeak-m-player nil
  "Emacspeak media player settings."
  :group 'emacspeak)
;;;###autoload
(defcustom emacspeak-m-player-program
  (executable-find "mplayer")
  "Media player program."
  :type 'string
  :group 'emacspeak-m-player)

(defvar emacspeak-m-player-openal-options
  '("-ao" "openal")
  "Additional options to use openal  --- this gives us hrtf for instance.")

(defvar emacspeak-m-player-default-options
  (list
   "-msglevel"          ; reduce chattiness  while preserving metadata
   (mapconcat
    #'identity
    '("all=4"
      "header=0" "vo=0" "ao=0"
      "decaudio=0" "decvideo=0" "open=0"
      "network=0" "statusline=0" "cplayer=0"
      "seek=0"
      ) ":")
   "-slave"  "-softvol" "-softvol-max" "300" "-quiet")
  "Default options for MPlayer.")

(defcustom emacspeak-m-player-options
  (copy-sequence emacspeak-m-player-default-options)
  "Options passed to mplayer."
  :type  '(repeat
           (string :tag "option"))
  :group 'emacspeak-m-player)
;;;###autoload
(defcustom emacspeak-media-location-bindings  nil
  "*Map specific key sequences to launching MPlayer accelerators
on a specific directory."
  :group 'emacspeak-m-player
  :group 'emacspeak-media
  :type '(repeat
          :tag "Emacspeak Media Locations"
          (list
           :tag "KeyBinding"
           (string :tag "Key")
           (directory :tag "Directory")))
  :set #'(lambda (sym val)
           (mapc
            #'(lambda (binding)
                (let ((key (cl-first binding))
                      (directory (cl-second binding)))
                  (emacspeak-m-player-bind-accelerator directory (kbd key))))
            val)
           (set-default sym val)))

(defvar emacspeak-media-directory-regexp
  (regexp-opt '("mp3" "audio" "music"))
  "Pattern matching locations where we store media.")

;;;###autoload
(defun emacspeak-multimedia  ()
  "Start or control Emacspeak multimedia player.

Uses current context to prompt for media to play.
Controls media playback when already playing a stream.

\\{emacspeak-m-player-mode-map}."
  (interactive)
  (cl-declare (special emacspeak-m-player-process))
  (cond
   ((and emacspeak-m-player-process
         (eq 'run (process-status emacspeak-m-player-process))
         (buffer-live-p (process-buffer emacspeak-m-player-process)))
    (with-current-buffer (process-buffer emacspeak-m-player-process)
      (call-interactively #'emacspeak-m-player-command)))
   (t
    (call-interactively #'emacspeak-m-player))))


(defun emacspeak-m-player-pop-to-player ()
  "Pop to m-player buffer."
  (interactive)
  (cl-declare (special emacspeak-m-player-process))
  (unless (process-live-p emacspeak-m-player-process)
    (emacspeak-multimedia))
  (pop-to-buffer (process-buffer emacspeak-m-player-process))
  (emacspeak-speak-mode-line))

(defun emacspeak-m-player-command (key)
  "Invoke MPlayer commands."
  (interactive (list (read-key-sequence "Key: ")))
  (unless (eq 'run (process-status emacspeak-m-player-process))
    (emacspeak-multimedia))
  (call-interactively
   (or (lookup-key emacspeak-m-player-mode-map key) 'undefined)))

(defun emacspeak-m-player-playlist-p (resource)
  "Check if specified resource matches a playlist type."
  (cl-declare (special emacspeak-m-player-playlist-pattern))
  (string-match emacspeak-m-player-playlist-pattern resource))

;;;###autoload
(defun emacspeak-m-player-bind-accelerator (directory key)
  "Binds key to invoke m-player  on specified directory."
  (interactive
   (list
    (read-directory-name"Media Directory: ")
    (read-key-sequence "Key: ")))
  (let ((command
         (eval
          `(defun
               ,(intern (format "emacspeak-media-%s"
                                (file-name-base
                                 (directory-file-name directory))))
               ()
             ,(format "Launch media from directory %s" directory)
             (interactive)
             (cl-declare  (special emacspeak-m-player-current-directory))
             (setq emacspeak-m-player-current-directory ,directory)
             (emacspeak-m-player-accelerator ,directory)))))
    (global-set-key key command)))

(defvar emacspeak-m-player-accelerator-p nil
  "Flag set by accelerators. Let-binding this causes default-directory
etc to be ignored when guessing directory.")


(defun emacspeak-m-player-accelerator (directory)
  "Launch MPlayer on specified directory."
  (let ((ido-case-fold t)
        (emacspeak-m-player-accelerator-p t)
        (emacspeak-media-shortcuts-directory (expand-file-name directory)))
    (call-interactively #'emacspeak-multimedia)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defun emacspeak-media-guess-directory ()
  "Guess default directory."
  (cl-declare (special emacspeak-media-directory-regexp
                       emacspeak-m-player-accelerator-p))
  (cond
   ((or (eq major-mode 'dired-mode) (eq major-mode 'locate-mode)) nil)
   (emacspeak-m-player-accelerator-p   emacspeak-media-shortcuts-directory)
   ((or ;  dir  contains media:
     (string-match emacspeak-media-directory-regexp default-directory)
     (directory-files default-directory   nil emacspeak-media-extensions))
    default-directory)
   (t   emacspeak-media-shortcuts-directory)))

;;;###autoload
(defun emacspeak-m-player-url (url &optional playlist-p)
  "Call emacspeak-m-player with specified URL."
  (interactive (list (car (browse-url-interactive-arg "Media URL: "))))
  (ems-with-messages-silenced
   (emacspeak-m-player url playlist-p)))

(defvar-local  emacspeak-m-player-file-list nil
  "List  that records list of files being played.")

(defun emacspeak-m-player-directory-files (directory)
  "Return media files in directory by searching recursively. "
  (cl-declare (special emacspeak-media-extensions))
  (directory-files-recursively directory emacspeak-media-extensions))

(defvar-local emacspeak-m-player-url-p nil
  "Flag that records if we are playing a stream URL")


(defun emacspeak-media-read-resource ()
  "Read resource from minibuffer with contextual smarts."
  (let ((completion-ignore-case t)
        (read-file-name-function
         (if (eq major-mode 'locate-mode)
             #'read-file-name-default
           #'ido-read-file-name))
        (read-file-name-completion-ignore-case t)
        (default-filename
          (when (or (eq major-mode 'dired-mode) (eq major-mode 'locate-mode))
            (dired-get-filename nil 'no-error)))
        (dir (emacspeak-media-guess-directory))
        (result nil))
    (setq result
          (expand-file-name
           (funcall read-file-name-function
            "Media Resource: "
            dir  
            default-filename 'must-match)))
    result))

(defun emacspeak-m-player-refresh-metadata ()
  "Populate metadata fields from currently playing  stream."
  (cl-declare (special emacspeak-m-player-metadata))
  (with-current-buffer (process-buffer emacspeak-m-player-process)
    (cl-loop
     for  f in
     '(title artist album year comment track genre)
     do
     (aset emacspeak-m-player-metadata
           (cl-struct-slot-offset 'emacspeak-m-player-metadata f)
           (cl-second
            (split-string
             (emacspeak-m-player-slave-command (format "get_meta_%s" f))
             "="))))
    emacspeak-m-player-metadata))
(defvar emacspeak-m-player-cue-info nil
  "Set to T if  ICY info cued automatically.")

(defun emacspeak-m-player-process-filter (process output)
  "Filter function that captures metadata.
Also cleanup ANSI escape sequences."
  (cl-declare (special emacspeak-m-player-cue-info
                       ansi-color-control-seq-regexp))
  (when (process-live-p process)
    (with-current-buffer (process-buffer process)
      (when (and emacspeak-m-player-metadata
                 (emacspeak-m-player-metadata-p emacspeak-m-player-metadata)
                 (string-match "ICY Info:" output))
        (setf
         (emacspeak-m-player-metadata-info emacspeak-m-player-metadata)
         (format "%s" output))
        (when emacspeak-m-player-cue-info
          (emacspeak-auditory-icon 'progress)
          (emacspeak-m-player-stream-info)))
      (goto-char (process-mark process))
      (let ((start (point)))
        (insert output)
        (save-excursion
          (goto-char start)
          (while (re-search-forward ansi-color-control-seq-regexp  (point-max) 'no-error)
            (delete-region (match-beginning 0) (match-end 0))))))))

(defun emacspeak-m-player-amark-save ()
  "Ensure that amarks are saved in the directory of the resource ."
  (interactive)
  (cl-declare (special emacspeak-m-player-process))
  (when (process-live-p emacspeak-m-player-process)
    (with-current-buffer
        (process-buffer emacspeak-m-player-process)
      (emacspeak-amark-save))))


;;;###autoload
(defun emacspeak-m-player (resource &optional play-list)
  "Play specified resource using m-player.  Optional prefix argument
play-list interprets resource as a play-list.  Second interactive
prefix arg adds option -allow-dangerous-playlist-parsing to mplayer.
Resource is a media resource or playlist containing media resources.
The player is placed in a buffer in emacspeak-m-player-mode."
  (interactive
   (list
    (emacspeak-media-read-resource)
    current-prefix-arg))
  (cl-declare (special
               emacspeak-m-player-file-list emacspeak-m-player-current-directory
               emacspeak-media-directory-regexp
               emacspeak-media-shortcuts-directory emacspeak-m-player-process
               emacspeak-m-player-program emacspeak-m-player-options))
  (when (and emacspeak-m-player-process
             (eq 'run (process-status emacspeak-m-player-process))
             (y-or-n-p "Stop currently playing music? "))
    (emacspeak-m-player-quit)
    (setq emacspeak-m-player-process nil))
  (let ((buffer (get-buffer-create "*M-Player*"))
        (alsa-device (getenv "ALSA_DEFAULT"))
        (process-connection-type nil)
        (playlist-p
         (or play-list
             (emacspeak-m-player-playlist-p resource)))
        (options (copy-sequence emacspeak-m-player-options))
        (file-list nil))
    (with-current-buffer buffer
      (emacspeak-m-player-mode)
      (setq emacspeak-m-player-url-p
            (and
             (not emacspeak-m-player-accelerator-p)
             (or
              (string-match emacspeak-media-shortcuts-directory resource )
              (string-match "^http" resource))))
      (unless emacspeak-m-player-url-p  ; not a URL
        (setq
         resource (expand-file-name resource))
        (setq emacspeak-m-player-current-directory (file-name-directory resource)))
      (if (file-directory-p resource)
          (setq file-list (emacspeak-m-player-directory-files resource))
        (setq file-list (list resource)))
      (when (and alsa-device (not (string= alsa-device "default")))
        (setq options
              (nconc options
                     (list "-ao" (format "alsa:device=%s" alsa-device)))))
      (setq options
            (cond
             ((and play-list  (listp play-list)(< 4   (car play-list)))
              (nconc options
                     (list "-allow-dangerous-playlist-parsing" "-playlist"
                           resource)))
             (playlist-p
              (nconc options (list "-playlist" resource)))
             (file-list (nconc options file-list))
             (t
              (nconc options (list resource)))))
      (setq buffer-undo-list t)
      (setq emacspeak-m-player-process
            (apply
             #'start-process "MPLayer" buffer
             emacspeak-m-player-program options))
      (set-process-filter  emacspeak-m-player-process
                           #'emacspeak-m-player-process-filter)
      (when (and
             emacspeak-m-player-current-directory
             (file-exists-p emacspeak-m-player-current-directory))
        (cd emacspeak-m-player-current-directory)
        (emacspeak-amark-load))
      (setq  emacspeak-m-player-file-list file-list)
      (emacspeak-auditory-icon 'progress)
      (when (called-interactively-p 'interactive)
        (message "MPlayer opened  %s"
                 (if (file-directory-p resource)
                     (car (last (split-string resource "/" t)))
                   (file-name-nondirectory resource)))))))

;;;###autoload
(defun emacspeak-m-player-using-openal (resource &optional play-list)
  "Use openal as the audio output driver. Adding hrtf=true to
~/.alsoftrc gives HRTF. You need to have openal installed and have an
mplayer that has been compiled with openal support to use this
feature. Calling spec is like `emacspeak-m-player'."
  (interactive
   (list
    (emacspeak-media-read-resource)
    current-prefix-arg))
  (cl-declare (special emacspeak-m-player-options
                       emacspeak-m-player-openal-options
                       emacspeak-m-player-process))
  (when (and emacspeak-m-player-process
             (eq 'run (process-status emacspeak-m-player-process))
             (y-or-n-p "Stop currently playing music? "))
    (emacspeak-m-player-quit))
  (unless (process-live-p emacspeak-m-player-process)
    (let ((emacspeak-m-player-options
           (append emacspeak-m-player-openal-options
                   emacspeak-m-player-options)))
      (funcall-interactively #'emacspeak-m-player resource play-list))))

(defvar emacspeak-m-player-hrtf-options
  '("-af" "hrtf" "-af" "resample=48000")
  "Additional options to use built-in HRTF.")


(defun emacspeak-m-player-using-hrtf ()
  "Add af resample=48000,hrtf to startup options.
This will work if the soundcard is set to 48000."
  (interactive)
  (cl-declare (special emacspeak-m-player-options emacspeak-m-player-hrtf-options
                       emacspeak-m-player-process))
  (when (and emacspeak-m-player-process
             (eq 'run (process-status emacspeak-m-player-process))
             (y-or-n-p "Stop currently playing music? "))
    (emacspeak-m-player-quit))
  (unless (process-live-p emacspeak-m-player-process)
    (let ((emacspeak-m-player-options
           (append emacspeak-m-player-hrtf-options
                   emacspeak-m-player-options)))
      (call-interactively #'emacspeak-m-player))))

;;;###autoload
(defun emacspeak-m-player-shuffle ()
  "Launch M-Player with shuffle turned on."
  (interactive)
  (cl-declare (special emacspeak-m-player-options))
  (let ((emacspeak-m-player-options
         (append emacspeak-m-player-options (list "-shuffle"))))
    (call-interactively #'emacspeak-m-player)))


(defun emacspeak-m-player-load (resource  &optional append)
  "Load specified resource into a running  m-player.
Interactive prefix arg appends the new resource to what is playing."
  (interactive
   (list
    (ems-with-messages-silenced
     (let ((completion-ignore-case t)
           (read-file-name-completion-ignore-case t))
       (read-file-name
        "MP3 Resource: "
        (if
            (string-match "\\(mp3\\)\\|\\(audio\\)"
                          (expand-file-name default-directory))
            default-directory
          emacspeak-media-shortcuts-directory)
        (when (eq major-mode 'dired-mode)
          (dired-get-filename)))))
    current-prefix-arg))
  (cl-declare (special emacspeak-media-extensions
                       emacspeak-media-shortcuts-directory))
  (unless (string-match "^[a-z]+:"  resource)
    (setq resource (expand-file-name resource)))
  (emacspeak-m-player-dispatch
   (format "loadfile %s %s" resource
           (if append 1 ""))))

;;}}}
;;{{{ Table of slave commands:

(defvar emacspeak-m-player-command-list nil
  "Cache of MPlayer slave commands.")

(defun emacspeak-m-player-command-list ()
  "Return MPlayer slave command table, populating it if
necessary."
  (cl-declare (special emacspeak-m-player-command-list))
  (cond
   (emacspeak-m-player-command-list emacspeak-m-player-command-list)
   (t
    (let ((commands
           (split-string
            (shell-command-to-string
             (format "%s -input cmdlist"
                     emacspeak-m-player-program))
            "\n" 'omit-nulls)))
      (setq emacspeak-m-player-command-list
            (cl-loop  for c in commands
                      collect
                      (split-string c " " 'omit-nulls)))))))

;;}}}
;;{{{ commands

(defun emacspeak-m-player-get-position ()
  "Return list suitable to use as an amark. --- see emacspeak-amark.el.
Return value is of the form (position filename length)."
  (cl-declare (special emacspeak-m-player-process))
  (with-current-buffer (process-buffer emacspeak-m-player-process)
;;; dispatch command twice to avoid flakiness in mplayer
    (emacspeak-m-player-dispatch "get_time_pos\nget_file_name\nget_time_length\n")
    (let* ((output (emacspeak-m-player-dispatch "get_time_pos\nget_file_name\nget_time_length\n") )
           (lines (split-string output "\n" 'omit-nulls))
           (fields
            (cl-loop
             for l in lines
             collect (cl-second (split-string l "=")))))
      (list
       (format "%s" (cl-first fields))  ; position
       (if (cl-second fields)
           (substring (cl-second  fields) 1 -1)
         "")
       (format "%s" (cl-third fields))))))

(defun emacspeak-m-player-current-filename ()
  "Return filename of currently playing track."
  (cl-second
   (split-string
    (emacspeak-m-player-dispatch "get_file_name\n")
    "=")))

(defun emacspeak-m-player-scale-speed (factor)
  "Scale speed by specified factor."
  (interactive "nFactor:")
  (emacspeak-m-player-dispatch
   (format "af_add scaletempo=scale=%f:speed=pitch" factor)))

(defun emacspeak-m-player-slower ()
  "Slow down playback.
This affects pitch."
  (interactive)
  (emacspeak-m-player-scale-speed 0.9091))

(defun emacspeak-m-player-faster ()
  "Speed up  playback.
This affects pitch."
  (interactive)
  (emacspeak-m-player-scale-speed 1.1))

(defun emacspeak-m-player-half-speed ()
  "Scale speed by 0.5."
  (interactive)
  (emacspeak-m-player-scale-speed 0.5))

(defun emacspeak-m-player-double-speed()
  "Scale speed by 2.0"
  (interactive)
  (emacspeak-m-player-scale-speed 2.0))

(defun emacspeak-m-player-reset-speed ()
  "Reset playing speed to normal."
  (interactive)
  (emacspeak-m-player-dispatch
   "speed_set 1.0"))

(defun emacspeak-m-player-play-tracks-jump (step)
  "Move within the play tree."
  (interactive"nSkip Tracks:")
  (unless (zerop step)
    (emacspeak-m-player-dispatch
     (format "pt_step %d" step))))

(defun emacspeak-m-player-previous-track ()
  "Move to previous track."
  (interactive)
  (emacspeak-m-player-play-tracks-jump -1))

(defun emacspeak-m-player-next-track ()
  "Move to next track."
  (interactive)
  (emacspeak-m-player-play-tracks-jump 1))

(defun emacspeak-m-player-play-tree-up (step)
  "Move within the play tree."
  (interactive
   (list
    (read-from-minibuffer "Move by: ")))
  (emacspeak-m-player-dispatch
   (format "pt_up %s" step)))

(defun emacspeak-m-player-alt-src-step (step)
  "Move within an ASF playlist."
  (interactive
   (list
    (read-from-minibuffer "Move by: ")))
  (emacspeak-m-player-dispatch
   (format "alt_src_step %s" step)))

(defun emacspeak-m-player-seek-relative (offset)
  "Seek  by offset into stream from current position.
Time offset can be specified as a number of seconds, or as HH:MM:SS."
  (interactive
   (list
    (read-from-minibuffer "Offset: ")))
  (when (string-match ":" offset)
(setq offset (ems--duration-to-seconds offset)))
  (emacspeak-m-player-dispatch (format "seek %s" offset)))

(defun emacspeak-m-player-seek-percentage (pos)
  "Seek  to absolute specified pos in percent."
  (interactive
   (list
    (read-from-minibuffer "Seek to percentage: ")))
  (emacspeak-m-player-dispatch
   (format "seek %s 1" pos)))

(defun emacspeak-m-player-seek-absolute (pos)
  "Seek  to absolute specified pos in seconds.
The time position can also be specified as HH:MM:SS."
  (interactive
   (list
    (read-from-minibuffer "Seek to time position: ")))
  (when (string-match ":" pos)
    (setq pos (ems--duration-to-seconds pos)))
  (emacspeak-m-player-dispatch (format "seek %s 2" pos)))

(defun emacspeak-m-player-beginning-of-track()
  "Move to beginning of track."
  (interactive)
  (emacspeak-m-player-seek-absolute "0"))

(defun emacspeak-m-player-end-of-track()
  "Move to beginning of track."
  (interactive)
  (emacspeak-m-player-seek-absolute "99"))

(defun emacspeak-m-player-backward-10s ()
  "Move back by 10 seconds."
  (interactive)
  (emacspeak-m-player-seek-relative "-10"))

(defun emacspeak-m-player-forward-10s ()
  "Move forward by 10 seconds."
  (interactive)
  (emacspeak-m-player-seek-relative "10"))

(defun emacspeak-m-player-backward-1min ()
  "Move back by 1 minute."
  (interactive)
  (emacspeak-m-player-seek-relative "-60"))

(defun emacspeak-m-player-forward-1min ()
  "Move forward by 1 minute."
  (interactive)
  (emacspeak-m-player-seek-relative "60"))

(defun emacspeak-m-player-backward-10min ()
  "Move backward by ten minutes."
  (interactive)
  (emacspeak-m-player-seek-relative "-600"))

(defun emacspeak-m-player-forward-10min ()
  "Move forward by ten minutes."
  (interactive)
  (emacspeak-m-player-seek-relative "600"))

(defun emacspeak-m-player-pause ()
  "Pause or unpause media player."
  (interactive)
  (emacspeak-m-player-dispatch "pause"))

(defvar ems--m-player-mark "LastStopped"
  "Name used to  mark position where we quit a stream.")

(defun emacspeak-m-player-quit ()
  "Quit media player."
  (interactive)
  (cl-declare (special emacspeak-amark-list ems--m-player-mark
                       emacspeak-m-player-url-p emacspeak-m-player-process))
  (let ((kill-buffer-query-functions nil))
    (when (eq (process-status emacspeak-m-player-process) 'run)
      (let ((buffer (process-buffer emacspeak-m-player-process)))
        (with-current-buffer buffer
          (unless
              (or
               emacspeak-m-player-url-p ;;;dont amark streams
               (string-equal emacspeak-media-shortcuts-directory
                             (substring default-directory 0 -1)))
            (emacspeak-m-player-amark-add ems--m-player-mark)
            (emacspeak-m-player-amark-save))
          (emacspeak-m-player-dispatch "quit")
          (emacspeak-auditory-icon 'close-object)
          (and (buffer-live-p buffer) (kill-buffer buffer))))
      (unless (eq (process-status emacspeak-m-player-process) 'exit)
        (delete-process  emacspeak-m-player-process))
      (setq emacspeak-m-player-process nil)
      (emacspeak-speak-mode-line))))


(defun emacspeak-m-player-volume-up ()
  "Increase volume."
  (interactive)
  (emacspeak-m-player-dispatch "volume 1"))


(defun emacspeak-m-player-volume-down ()
  "Decrease volume."
  (interactive)
  (emacspeak-m-player-dispatch "volume -1"))

(defvar emacspeak-m-player-active-filters nil
  "Caches filters that are active.")
(make-variable-buffer-local 'emacspeak-m-player-active-filters)


(defun emacspeak-m-player-volume-change (value)
  "Change volume to specified absolute value."
  (interactive"sChange Volume to:")
  (cl-declare (special emacspeak-m-player-active-filters))
  (cl-pushnew "volume" emacspeak-m-player-active-filters :test #'string=)
  (emacspeak-m-player-dispatch
   (format "volume %s, 1" value)))


(defun emacspeak-m-player-balance ()
  "Set left/right balance."
  (interactive)
  (emacspeak-m-player-dispatch
   (format "balance %s"
           (read-from-minibuffer "Balance -- Between -1 and 1:"))))


(defun emacspeak-m-player-slave-command (command)
  "Dispatch slave command read from minibuffer."
  (interactive
   (list
    (completing-read "Slave Command: " (emacspeak-m-player-command-list))))
  (with-current-buffer (process-buffer emacspeak-m-player-process)
    (let* ((args
            (when (cdr (assoc command emacspeak-m-player-command-list))
              (read-from-minibuffer
               (mapconcat #'identity
                          (cdr (assoc command emacspeak-m-player-command-list))
                          " "))))
           (result
            (emacspeak-m-player-dispatch (format "%s %s" command args))))
      (when result
        (setq result (replace-regexp-in-string  "^ans_" "" result))
        (setq result (replace-regexp-in-string  "_" " " result)))
      (when (called-interactively-p 'interactive)
        (message   "%s"
                   (or result "Waiting")))
      result)))

(defun emacspeak-m-player-delete-filter (filter)
  "Delete filter."
  (interactive
   (list
    (with-current-buffer (process-buffer emacspeak-m-player-process)
      (completing-read "Filter:"
                       (or emacspeak-m-player-active-filters
                           emacspeak-m-player-filters nil nil)))))
  (cl-declare (special emacspeak-m-player-filters
                       emacspeak-m-player-active-filters))
  (with-current-buffer (process-buffer emacspeak-m-player-process)
    (let* ((result (emacspeak-m-player-dispatch (format "af_del %s" filter))))
      (setq emacspeak-m-player-active-filters (remove  filter emacspeak-m-player-active-filters))
      (when result
        (setq result (replace-regexp-in-string  "^ans_" "" result))
        (setq result (replace-regexp-in-string  "_" " " result)))
      (message   "%s" (or result "Waiting")))))


(defun emacspeak-m-player-display-percent ()
  "Display current percentage."
  (interactive)
  (dtk-speak-and-echo (emacspeak-m-player-slave-command "get_percent_pos")))


(defun emacspeak-m-player-stream-info (&optional toggle-cue)
  "Speak and display metadata if available.
Interactive prefix arg toggles automatic cueing of ICY info updates."
  (interactive "P")
  (cl-declare (special emacspeak-m-player-metadata emacspeak-m-player-cue-info))
  (with-current-buffer (process-buffer emacspeak-m-player-process)
    (unless   emacspeak-m-player-metadata  (error "No metadata"))
    (let* ((m (emacspeak-m-player-metadata-info  emacspeak-m-player-metadata))
           (info (and m (cl-second (split-string m "=")))))
      (when toggle-cue
        (setq emacspeak-m-player-cue-info (not emacspeak-m-player-cue-info)))
      (if toggle-cue
          (progn
            (emacspeak-auditory-icon
             (if emacspeak-m-player-cue-info 'on 'off))
            (message "ICY messages  turned %s."
                     (if emacspeak-m-player-cue-info "on" "off")))
        (message"%s" (format "%s" (or info  "No Stream Info")))))))


(defun emacspeak-m-player-get-length ()
  "Display length of track in seconds."
  (interactive)
  (dtk-speak-and-echo (emacspeak-m-player-dispatch "get_time_length")))

(defconst emacspeak-m-player-display-cmd
  "get_time_pos\nget_percent_pos\nget_time_length\nget_file_name\n"
  "Command we send MPlayer to display position.")

(defun emacspeak-m-player-display-position ()
  "Display current position in track and its length."
  (interactive)
  (cl-declare (special emacspeak-m-player-display-cmd))
  (let ((fields nil)
        (result (emacspeak-m-player-dispatch emacspeak-m-player-display-cmd)))
    (when result
      (setq result (replace-regexp-in-string  "^ans_" "" result))
      (setq fields
            (mapcar
             #'(lambda (s) (split-string s "="))
             (split-string  result "\n"))))
    (cond
     (fields                       ; speak them after audio formatting
      (cl-loop
       for f in fields do
       (put-text-property 0 (length (cl-first f))
                          'personality 'voice-smoothen (cl-first f))
       (put-text-property 0 (length (cl-second f))
                          'personality 'voice-bolden (cl-second f)))
      (setq result
            (cl-loop
             for f in fields
             collect
             (concat (cl-first f) " " (cl-second f) "\n ")))
      (tts-with-punctuations 'some
                             (dtk-speak-and-echo (apply #'concat result))))
     (t (dtk-speak-and-echo "Waiting")))))

(defun emacspeak-m-player-load-file(f)
  "Load specified file."
  (interactive "fMedia File:")
  (emacspeak-m-player-dispatch
   (format "loadfile %s"
           (expand-file-name f))))

(defun emacspeak-m-player-load-playlist(f)
  "Load specified playlist file."
  (interactive "fPlaylist File:")
  (emacspeak-m-player-dispatch
   (format "loadlist %s"
           (expand-file-name f))))

(defconst emacspeak-m-player-filters
  '("extrastereo" "volnorm" "surround"
    "channels=1:0:0:0:1"
    "channels=1:1:0:1:1"
    "channels=1:2"
    "ladspa=bs2b:bs2b:700:4.5"
    "ladspa=ZamAutoSat-ladspa.so:ZamAutoSat:"
    "ladspa=tap_pinknoise.so:tap_pinknoise:0.5:-2:-12"
    "ladspa=ZamHeadX2-ladspa.so:ZamHeadX2:0:60:2.5"
    "ladspa=ZamHeadX2-ladspa.so:ZamHeadX2:0:30:2.5"
    "ladspa=ZamHeadX2-ladspa.so:ZamHeadX2:0:45:2.5"
    "ladspa=ZamHeadX2-ladspa.so:ZamHeadX2:0:15:2.5"
    "ladspa=amp:amp_stereo:2"
    "ladspa=amp:amp_stereo:0.5"
    "ladspa=tap_autopan:tap_autopan:.0016:100:1.5, ladspa=tap_autopan:tap_autopan:.06:33:2"
    "bs2b=cmoy" "bs2b=jmeier" "bs2b")
  "Table of useful MPlayer filters.")

(defun emacspeak-m-player-add-autopan ()
  "Add predefined autopan effect."
  (interactive)
  (emacspeak-m-player-add-filter
   (concat
    "ladspa=tap_autopan:tap_autopan:.0016:100:1,"
    "ladspa=tap_autopan:tap_autopan:.016:33:1")))

(defun emacspeak-m-player-add-autosat ()
  "Add predefined ZamAutoSat (auto saturation) effect."
  (interactive)
  (emacspeak-m-player-add-filter
   "ladspa=ZamAutoSat-ladspa.so:ZamAutoSat:"))

(defun emacspeak-m-player-add-filter (filter-name &optional edit)
  "Adds specified filter. Prompts with one of several pre-defined
filters. Optional interactive prefix arg `edit' enables editing the
selected filter before it is applied."
  (interactive
   (list
    (completing-read "Filter:"
                     emacspeak-m-player-filters
                     nil nil)
    current-prefix-arg))
  (cl-declare (special emacspeak-m-player-process
                       emacspeak-m-player-active-filters))
  (when edit
    (setq filter-name
          (read-from-minibuffer
           "Edit Filter: " filter-name)))
  (when (process-live-p  emacspeak-m-player-process)
    (push filter-name emacspeak-m-player-active-filters)
    (emacspeak-m-player-dispatch (format "af_add %s" filter-name))))

(defun emacspeak-m-player-left-channel ()
  "Play both channels on left channel."
  (interactive)
  (let ((filter-name "channels=2:1:0:0:1:0"))
    (when (process-live-p  emacspeak-m-player-process)
      (emacspeak-m-player-dispatch (format "af_add %s" filter-name)))))

(defun emacspeak-m-player-right-channel ()
  "Play on right channel."
  (interactive)
  (let ((filter-name "channels=2:1:0:1:1:1"))
    (when (process-live-p  emacspeak-m-player-process)
      (emacspeak-m-player-dispatch (format "af_add %s" filter-name)))))

(defun emacspeak-m-player-clear-filters ()
  "Clear all active filters"
  (interactive)
  (cl-declare (special emacspeak-m-player-process
                       emacspeak-m-player-active-filters))
  (setq emacspeak-m-player-active-filters nil)
  (when (process-live-p emacspeak-m-player-process)
    (emacspeak-m-player-dispatch "af_clr")
    (emacspeak-auditory-icon 'delete-object)))

(defun emacspeak-m-player-customize-options ()
  "Use Customize to manipulate MPlayer options."
  (interactive)
  (customize-variable 'emacspeak-m-player-options)
  (goto-char (point-min))
  (search-forward "INS"))

;;}}}
;;{{{ Media History:

(defvar emacspeak-m-player-history nil)
;;;###autoload
(defvar emacspeak-m-player-media-history nil
  "Record media urls we played.")

(defun emacspeak-m-player-from-media-history (posn)
  "Play media from media-history.
Numeric arg `posn' specifies position in history."
  (interactive "p")
  (cl-declare (special emacspeak-m-player-history))
  (setq posn (1- posn))
  (cl-assert (and emacspeak-m-player-history (> (length emacspeak-m-player-history)  posn)) t "Not that many elements in media history")
  (apply #'emacspeak-m-player (elt emacspeak-m-player-history posn)))

;;}}}
;;{{{ Reset Options:

(defun emacspeak-m-player-reset-options ()
  "Reset MPlayer options to initial defaults."
  (interactive)
  (cl-declare (special emacspeak-m-player-default-options
                       emacspeak-m-player-options))
  (setq emacspeak-m-player-options
        emacspeak-m-player-default-options)
  (message "Reset options."))

;;}}}
;;{{{ equalizer

;;; Equalizer presets:
;;; Cloned from VLC and munged for m-player.
;;; VLC uses -20db .. 20db; mplayer uses -12db .. 12db
;;; See http://advantage-bash.blogspot.com/2013/05/mplayer-presets.html

(defvar emacspeak-m-player-equalizer-presets 
  '(
      ("flat" . [0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0])
      ("classical" . [0.0 0.0 0.0 0.0 0.0 -4.4 -4.4 -4.4 -5.8 -6.5])
      ("club" . [0.0 0.0 4.8 3.3 3.3 3.3 1.9 0.0 0.0 0.0])
      ("dance" . [5.7 4.3 1.4 0.0 0.0 -3.4 -4.4 -4.3 0.0 0.0])
      ("full-bass" . [-4.8 5.7 5.7 3.3 1.0 -2.4 -4.8 -6.3 -6.7 -6.7])
      ("full-bass-and-treble" . [4.3 3.3 0.0 -4.4 -2.9 1.0 4.8 6.7 7.2 7.2])
      ("full-treble" . [-5.8 -5.8 -5.8 -2.4 1.4 6.7 9.6 9.6 9.6 10.1])
      ("headphones" . [2.8 6.7 3.3 -2.0 -1.4 1.0 2.8 5.7 7.7 8.6])
      ("large-hall" . [6.2 6.2 3.3 3.3 0.0 -2.9 -2.9 -2.9 0.0 0.0])
      ("live" . [-2.9 0.0 2.4 3.3 3.3 3.3 2.4 1.4 1.4 1.4])
      ("party" . [4.3 4.3 0.0 0.0 0.0 0.0 0.0 0.0 4.3 4.3])
      ("pop" . [-1.0 2.8 4.3 4.8 3.3 0.0 -1.4 -1.4 -1.0 -1.0])
      ("reggae" . [0.0 0.0 0.0 -3.4 0.0 3.8 3.8 0.0 0.0 0.0])
      ("rock" . [4.8 2.8 -3.4 -4.8 -2.0 2.4 5.3 6.7 6.7 6.7])
      ("ska" . [-1.4 -2.9 -2.4 0.0 2.4 3.3 5.3 5.7 6.7 5.8])
      ("soft" . [2.8 1.0 0.0 -1.4 0.0 2.4 4.8 5.7 6.7 7.2])
      ("soft-rock" . [2.4 2.4 1.4 0.0 -2.4 -3.4 -2.0 0.0 1.4 5.3])
      ("techno" . [4.8 3.3 0.0 -3.4 -2.9 0.0 4.8 5.7 5.8 5.3]))
  "MPlayer equalizer presets.")

(defsubst ems--equalizer-preset-get (name)
  "Return vector of numbers for specified preset."
  (cl-declare (special  emacspeak-m-player-equalizer-presets))
  (cdr (assoc name emacspeak-m-player-equalizer-presets)))

(defconst emacspeak-m-player-equalizer (make-vector 10 0)
  "Vector holding equalizer settings.")

(defconst  emacspeak-m-player-equalizer-bands
  ["31.25 Hz"
   "62.50 Hz"
   "125.00 Hz"
   "250.00 Hz"
   "500.00 Hz"
   "1.00 kHz"
   "2.00 kHz"
   "4.00 kHz"
   "8.00 kHz"
   "16.00 kHz"]
  "Center frequencies for the 10 equalizer bands in MPlayer.")

(defun emacspeak-m-player-equalizer-control (v)
  "Manipulate values in specified vector using minibuffer.
Applies  the resulting value at each step."
  (interactive)
  (cl-declare (special emacspeak-m-player-equalizer-bands))
  (let ((column 0)
        (key nil)
        (result  (mapconcat #'number-to-string v  ":"))
        (continue t))
;;; First, clear any equalizers in effect:
    (emacspeak-m-player-dispatch "af_del equalizer")
;;; Apply specified vector:
    (emacspeak-m-player-dispatch (format "af_add equalizer=%s" result))
    (while  continue
      (setq key
            (read-key-sequence
             (format "G%s:%s (%s)" column (aref v column)
                     (aref emacspeak-m-player-equalizer-bands column))))
      (cond
       ((equal key "e")
        (aset
         v column
         (read-number
          (format
           "Value for G%s:%s (%s)"
           column (aref v column)
           (aref emacspeak-m-player-equalizer-bands column)))))
       ((equal key [left])
        (setq column (% (+ 9  column) 10)))
       ((equal key [right])
        (setq column (% (1+ column) 10)))
       ((equal key [up])
        (aset v   column (min 12 (1+ (aref v column)))))
       ((equal key [down])
        (aset v   column (max -12 (1- (aref v column)))))
       ((equal key [prior])
        (aset v   column (min 12 (+ 4  (aref v column)))))
       ((equal key [next])
        (aset v   column (max -12 (- (aref v column)  4))))
       ((equal key [home])
        (aset v   column 12))
       ((equal key [end])
        (aset v   column -12))
       ((equal key "\C-g")
        (emacspeak-m-player-dispatch "af_del equalizer")
        (error "Did not change equalizer."))
       ((equal key "\C-m")
        (setq emacspeak-m-player-equalizer v)
        (setq continue nil))
       (t (message "Invalid key")))
      (setq result (mapconcat #'number-to-string v  ":"))
      (emacspeak-m-player-dispatch (format "af_cmdline equalizer %s" result)))
    result))

(defun emacspeak-m-player-add-equalizer (&optional reset)
  "Add equalizer to playing stream.  Equalizer is updated as each change
is made, and the final effect set by pressing RET.  Interactive prefix
arg `reset' starts with all filters set to 0."
  (interactive "P")
  (cl-declare (special emacspeak-m-player-process emacspeak-m-player-equalizer
                       emacspeak-m-player-active-filters))
  (cond
   ((eq 'run  (process-status emacspeak-m-player-process))
    (emacspeak-m-player-equalizer-control
     (if reset  (make-vector 10 0)
       emacspeak-m-player-equalizer))
    (emacspeak-auditory-icon 'close-object)
    (push "equalizer" emacspeak-m-player-active-filters))
   (t (message "No stream playing at present."))))

(defun emacspeak-m-player-equalizer-preset  (name)
  "Prompts for  equalizer preset and applies it to current stream.

The following presets are available:
 
flat classical club dance full-bass full-bass-and-treble
 full-treble headphones large-hall live party pop reggae rock
 ska soft soft-rock techno "
  (interactive
   (list
    (completing-read
     "MPlayer Equalizer Preset:"
     emacspeak-m-player-equalizer-presets
     nil 'must-match)))
  (cl-declare (special emacspeak-m-player-active-filters))
  (cl-declare (special emacspeak-m-player-equalizer-presets  emacspeak-m-player-equalizer))
  (let ((result nil)
        (p (ems--equalizer-preset-get name)))
    (setq emacspeak-m-player-equalizer p)
    (setq result  (mapconcat #'number-to-string p  ":"))
    (emacspeak-m-player-dispatch "af_del equalizer")
    (cl-pushnew "equalizer" emacspeak-m-player-active-filters :test #'string=)
    (emacspeak-m-player-dispatch (format "af_add equalizer=%s" result))))

;;}}}
;;{{{ Key Bindings:

(cl-declaim (special emacspeak-m-player-mode-map))

(defvar emacspeak-m-player-bindings
  '(
    ("M-," emacspeak-m-player-set-clip-start)
    ("M-." emacspeak-m-player-set-clip-end)
    (";" emacspeak-m-player-pop-to-player)
    ("%" emacspeak-m-player-display-percent)
    ("(" emacspeak-m-player-left-channel)
    (")" emacspeak-m-player-right-channel)
    ("+" emacspeak-m-player-volume-up)
    ("," emacspeak-m-player-backward-10s)
    ("-" emacspeak-m-player-volume-down)
    ("." emacspeak-m-player-forward-10s)
    ("<" emacspeak-m-player-backward-1min)
    ("<down>" emacspeak-m-player-forward-1min)
    ("<end>" emacspeak-m-player-end-of-track)
    ("<home>" emacspeak-m-player-beginning-of-track)
    ("<left>" emacspeak-m-player-backward-10s)
    ("<next>" emacspeak-m-player-forward-10min)
    ("<prior>" emacspeak-m-player-backward-10min)
    ("<right>" emacspeak-m-player-forward-10s)
    ("<up>" emacspeak-m-player-backward-1min)
    ("=" emacspeak-m-player-volume-up)
    (">" emacspeak-m-player-forward-1min)
    ("?" emacspeak-m-player-display-position)
    ("\\" emacspeak-m-player-persist-process)
    ("/" emacspeak-m-player-restore-process)
    ("C" emacspeak-m-player-clear-filters)
    ("E" emacspeak-m-player-add-equalizer)
    ("C-m" emacspeak-m-player-load)
    ("DEL" emacspeak-m-player-reset-speed)
    ("L" emacspeak-m-player-locate-media)
    ("M" emacspeak-m-player-display-metadata)
    ("M-l" emacspeak-m-player-load-playlist)
    ("C-l" ladspa)
    ("A" emacspeak-m-player-amark-add)
    ("O" emacspeak-m-player-reset-options)
    ("P" emacspeak-m-player-apply-reverb-preset)
    ("Q" emacspeak-m-player-quit)
    ("R" emacspeak-m-player-edit-reverb)
    ("S" emacspeak-m-player-amark-save)
    ("x" emacspeak-m-player-pan)
    ("w" emacspeak-m-player-write-clip)
    ("SPC" emacspeak-m-player-pause)
    ("[" emacspeak-m-player-slower)
    ("]" emacspeak-m-player-faster)
    ("G" emacspeak-m-player-seek-percentage)
    ("a" emacspeak-m-player-add-autopan)
    ("b" emacspeak-wizards-view-buffers-filtered-by-m-player-mode)
    ("c" emacspeak-m-player-slave-command)
    ("d" emacspeak-m-player-delete-filter)
    ("e" emacspeak-m-player-equalizer-preset)
    ("f" emacspeak-m-player-add-filter)
    ("g" emacspeak-m-player-seek-absolute)
    ("i" emacspeak-m-player-stream-info)
    ("j" emacspeak-m-player-amark-jump)
    ("k" emacspeak-m-player-quit)
    ("l" emacspeak-m-player-get-length)
    ("m" emacspeak-m-player-mode-line)
    ("n" emacspeak-m-player-next-track)
    ("o" emacspeak-m-player-customize-options)
    ("p" emacspeak-m-player-previous-track)
    ("q" bury-buffer)
    ("r" emacspeak-m-player-seek-relative)
    ("s" emacspeak-m-player-scale-speed)
    ("t" emacspeak-m-player-play-tracks-jump)
    ("u" emacspeak-m-player-url)
    ("v" emacspeak-m-player-volume-change)
    ("z" emacspeak-m-player-add-autosat)
    ("{" emacspeak-m-player-half-speed)
    ("}" emacspeak-m-player-double-speed)
    )
  "Key bindings used by Emacspeak M-Player.")

(cl-loop for k in emacspeak-m-player-bindings do
         (emacspeak-keymap-update  emacspeak-m-player-mode-map k))

(defun emacspeak-m-player-volume-set (&optional arg)
  "Set Volume in steps from 1 to 9."
  (interactive "P")
  (cl-declare (special last-input-event))
  (let ((vol-step
         (cond
          ((not (called-interactively-p 'interactive)) arg)
          (t
           (read (format "%c" last-input-event))))))
    (cl-assert
     (and (integerp vol-step) (< 0 vol-step) (< vol-step 10))
     nil "Volume step should be between 1 and 9")
    (emacspeak-m-player-volume-change (* 11 vol-step))))

(cl-loop
 for i from 1 to 9 do
 (define-key emacspeak-m-player-mode-map (kbd (format "%s" i)) 'emacspeak-m-player-volume-set))

;;}}}
;;{{{ YouTube Player

(defcustom emacspeak-m-player-youtube-dl
  (executable-find "youtube-dl")
  "YouTube download tool"
  :type 'string
  :group 'emacspeak-m-player)

(defsubst ems--m-p-get-yt-audio-first-fmt (url)
  "Get first available audio format code for   YT URL"
  (substring 
   (shell-command-to-string
    (format 
     "%s -F '%s' | grep '^[0-9]'   |grep audio |  head -1 | cut -f 1 -d \' \'"
     emacspeak-m-player-youtube-dl url))
   0 -1))

(defsubst ems--m-p-get-yt-audio-last-fmt (url)
  "Get last  available (best audio format code for   YT URL"
  (substring 
   (shell-command-to-string
    (format 
     "%s -F '%s' | grep '^[0-9]'   | grep audio |tail -1 | cut -f 1 -d \' \'"
     emacspeak-m-player-youtube-dl url))
   0 -1))
;;;###autoload
(defun emacspeak-m-player-youtube-player (url &optional best)
  "Use youtube-dl and mplayer to stream the audio for YouTube content.
Default is to pick smallest (lowest quality) audio format.
Optional prefix arg `best' chooses highest quality."
  (interactive
   (list
    (emacspeak-eww-read-url)
    current-prefix-arg))
  (cl-declare (special emacspeak-m-player-youtube-dl))
  (unless (file-executable-p emacspeak-m-player-youtube-dl)
    (error "Please install youtube-dl first."))
  (let ((u
         (shell-command-to-string
          (format "%s -f %s -g '%s' 2> /dev/null"
                  emacspeak-m-player-youtube-dl
                  (if best
                      (ems--m-p-get-yt-audio-last-fmt url)
                    (ems--m-p-get-yt-audio-first-fmt url))
                  url))))
    (when (= 0 (length  u)) (error "Error retrieving Media URL "))
    (setq u (substring u 0 -1))
    (kill-new u)
    (emacspeak-m-player u)))

;;}}}
;;{{{ pause/resume


(defun emacspeak-m-player-pause-or-resume ()
  "Pause/resume if m-player is running. For use  in
emacspeak-silence-hook."
  (cl-declare (special emacspeak-m-player-process))
  (when (and emacspeak-m-player-process
             (eq 'run (process-status emacspeak-m-player-process)))
    (emacspeak-m-player-pause)))
(add-hook 'emacspeak-silence-hook 'emacspeak-m-player-pause-or-resume)

;;}}}
;;{{{ AMarks:




(defun emacspeak-m-player-amark-add (name &optional prompt-position)
  "Set AMark `name' at current position in current audio stream.
Interactive prefix arg prompts for position.
As the default, use current position."
  (interactive "sAMark Name:\nP")
  (let* ((pos (emacspeak-m-player-get-position))
         (file-name (cl-second pos)))
    (when
        (and file-name  (not (zerop (length file-name))))
      (setq pos
            (cond
             (prompt-position (read-number "Position: "))
             (t  (cl-first pos))))
      (emacspeak-amark-add file-name name pos)
      (message "Added Amark %s in %s at %s" name file-name pos))))

(defun ems-file-index (name file-list)
  "Return index of name in file-list."
  (cl-position (expand-file-name name) file-list :test #'string=))


(defun emacspeak-m-player-amark-jump ()
  "Jump to specified AMark."
  (interactive)
  (cl-declare (special emacspeak-m-player-file-list))
  (with-current-buffer (process-buffer emacspeak-m-player-process)
    (let* ((amark (call-interactively #'emacspeak-amark-find))
           (files emacspeak-m-player-file-list)
           (current
            (ems-file-index (cl-second (emacspeak-m-player-get-position)) files))
           (new (ems-file-index (emacspeak-amark-path  amark) files)))
      (cond ; move to marked file if found, otherwise load
       ((and current new) ;skip in current play list
        (emacspeak-m-player-play-tracks-jump (- new current)))
       (t (emacspeak-m-player-dispatch
           (format "loadfile \"%s\""
                   (shell-quote-argument
                    (expand-file-name (emacspeak-amark-path amark)))))))
                                        ; now jump to marked position
      (emacspeak-m-player-seek-absolute (emacspeak-amark-position amark)))))

;;}}}
;;{{{ Adding specific Ladspa filters:

;;; tap_reverb filter

(defcustom emacspeak-m-player-reverb-filter
  '("ladspa=tap_reverb:tap_reverb" 10000 -2 -10 1 1 1 1 6)
  "Tap Reverb Settings."
  :type
  '(list
    (const :tag "Ladspa Tap Reverb" :value "ladspa=tap_reverb:tap_reverb")
    (integer :tag "Decay MS" :value 2500)
    (integer :tag "Dry-Level" :value 0)
    (integer :tag "Wet-Level" :value 0)
    (choice :tag "Comb Filter"
            (const :tag "On" :value 1)
            (const :tag "Off" :value 0))
    (choice :tag "Allpass Filter"
            (const :tag "On" :value 1)
            (const :tag "Off" :value 0))
    (choice :tag "Bandpass Filter"
            (const :tag "On" :value 1)
            (const :tag "Off" :value 0))
    (choice :tag "Enhanced Stereo"
            (const :tag "On" :value 1)
            (const :tag "Off" :value 0))
    (choice :tag "Reverb Preset"
            (const :tag "AfterBurn" :value 0)
            (const :tag "AfterBurn (Long)" :value 1)
            (const :tag "Ambience" :value 2)
            (const :tag "Ambience (Thick)" :value 3)
            (const :tag "Ambience (Thick) - HD" :value 4)
            (const :tag "Cathedral" :value 5)
            (const :tag "Cathedral - HD" :value 6)
            (const :tag "Drum Chamber" :value 7)
            (const :tag "Garage" :value 8)
            (const :tag "Garage (Bright)" :value 9)
            (const :tag "Gymnasium" :value 10)
            (const :tag "Gymnasium (Bright)" :value 11)
            (const :tag "Gymnasium (Bright) - HD" :value 12)
            (const :tag "Hall (Small)" :value 13)
            (const :tag "Hall (Medium)" :value 14)
            (const :tag "Hall (Large)" :value 15)
            (const :tag "Hall (Large) - HD" :value 16)
            (const :tag "Plate (Small)" :value 17)
            (const :tag "Plate (Medium)" :value 18)
            (const :tag "Plate (Large)" :value 19)
            (const :tag "Plate (Large) - HD" :value 20)
            (const :tag "Pulse Chamber" :value 21)
            (const :tag "Pulse Chamber (Reverse)" :value 22)
            (const :tag "Resonator (96 ms)" :value 23)
            (const :tag "Resonator (152 ms)" :value 24)
            (const :tag "Resonator (28 ms)" :value 25)
            (const :tag "Room (Small)" :value 26)
            (const :tag "Room (Medium)" :value 27)
            (const :tag "Room (Large)" :value 28)
            (const :tag "Room (Large) - HD" :value 29)
            (const :tag "Slap Chamber" :value 30)
            (const :tag "Slap Chamber - HD" :value 31)
            (const :tag "Slap Chamber (Bright)" :value 32)
            (const :tag "Slap Chamber (Bright) - HD" :value 33)
            (const :tag "Smooth Hall (Small)" :value 34)
            (const :tag "Smooth Hall (Medium)" :value 35)
            (const :tag "Smooth Hall (Large)" :value 36)
            (const :tag "Smooth Hall (Large) - HD" :value 37)
            (const :tag "Vocal Plate" :value 38)
            (const :tag "Vocal Plate - HD" :value 39)
            (const :tag "Warble Chamber" :value 40)
            (const :tag "Warehouse" :value 41)
            (const :tag "Warehouse - HD" :value 42)))
  :group 'emacspeak-m-player)

(defun emacspeak-m-player-edit-reverb ()
  "Edit  current ladspa reverb filter.
See option emacspeak-m-player-reverb-filter to customize reverb filter values.
You need to use mplayer built with ladspa support, and have package
tap-reverb already installed."
  (interactive)
  (cl-declare (special emacspeak-m-player-reverb-filter))
  (let ((ladspa(or  (getenv "LADSPA_PATH")
                    "/usr/lib/ladspa"))
        (filter nil)
        (orig-filter
         (mapconcat
          #'(lambda (v) (format "%s" v))
          emacspeak-m-player-reverb-filter ":")))
    (unless ladspa (error "Environment variable LADSPA_PATH not set."))
    (unless (getenv "LADSPA_PATH") (setenv "LADSPA_PATH" ladspa))
    (unless (file-exists-p (expand-file-name "tap_reverb.so" ladspa))
      (error "Package tap_reverb not installed."))
    (setq filter (read-from-minibuffer "Reverb: " orig-filter))
    (setq emacspeak-m-player-reverb-filter(split-string filter ":"))
    (emacspeak-m-player-dispatch "af_clr")
    (emacspeak-m-player-dispatch (format "af_add %s" filter))))

(defconst emacspeak-m-player-reverb-preset-table
  '(
    ("AfterBurn"   0)
    ("AfterBurn (Long)"   1)
    ("Ambience"   2)
    ("Ambience (Thick)"   3)
    ("Ambience (Thick) - HD"   4)
    ("Cathedral"   5)
    ("Cathedral - HD"   6)
    ("Drum Chamber"   7)
    ("Garage"   8)
    ("Garage (Bright)"   9)
    ("Gymnasium"   10)
    ("Gymnasium (Bright)"   11)
    ("Gymnasium (Bright) - HD"   12)
    ("Hall (Small)"   13)
    ("Hall (Medium)"   14)
    ("Hall (Large)"   15)
    ("Hall (Large) - HD"   16)
    ("Plate (Small)"   17)
    ("Plate (Medium)"   18)
    ("Plate (Large)"   19)
    ("Plate (Large) - HD"   20)
    ("Pulse Chamber"   21)
    ("Pulse Chamber (Reverse)"   22)
    ("Resonator (96 ms)"   23)
    ("Resonator (152 ms)"   24)
    ("Resonator (28 ms)"   25)
    ("Room (Small)"   26)
    ("Room (Medium)"   27)
    ("Room (Large)"   28)
    ("Room (Large) - HD"   29)
    ("Slap Chamber"   30)
    ("Slap Chamber - HD"   31)
    ("Slap Chamber (Bright)"   32)
    ("Slap Chamber (Bright) - HD"   33)
    ("Smooth Hall (Small)"   34)
    ("Smooth Hall (Medium)"   35)
    ("Smooth Hall (Large)"   36)
    ("Smooth Hall (Large) - HD"   37)
    ("Vocal Plate"   38)
    ("Vocal Plate - HD"   39)
    ("Warble Chamber"   40)
    ("Warehouse"   41)
    ("Warehouse - HD"   42))
  "Table mapping tap reverb preset names to values.")

(defconst emacspeak-m-player-tap-reverb-presets
  '(("AfterBurn" 2.8)
    ("AfterBurn (Long)" 4.8)
    ("Ambience" 1.1)
    ("Ambience (Thick)" 1.2)
    ("Ambience (Thick) - HD" 1.2)
    ("Cathedral" 10)
    ("Cathedral - HD" 10)
    ("Drum Chamber" 3.6)
    ("Garage" 2.3)
    ("Garage (Bright)" 2.3)
    ("Gymnasium" 5.9)
    ("Gymnasium (Bright)" 5.9)
    ("Gymnasium (Bright) - HD" 5.9)
    ("Hall (Small)" 2.0)
    ("Hall (Medium)" 3.0)
    ("Hall (Large)" 5.1)
    ("Hall (Large) - HD" 5.1)
    ("Plate (Small)" 1.7)
    ("Plate (Medium)" 2.6)
    ("Plate (Large)" 5.7)
    ("Plate (Large) - HD" 5.7)
    ("Pulse Chamber" 3.1)
    ("Pulse Chamber (Reverse)" 3.1)
    ("Resonator (96 ms)" 4.0)
    ("Resonator (152 ms)" 4.2)
    ("Resonator (208 ms)" 5.1)
    ("Room (Small)" 1.9)
    ("Room (Medium)" 2.8)
    ("Room (Large)" 4.4)
    ("Room (Large) - HD" 4.4)
    ("Slap Chamber" 2.3)
    ("Slap Chamber - HD" 2.9)
    ("Slap Chamber (Bright)" 3.4)
    ("Slap Chamber (Bright) - HD" 3.7)
    ("Smooth Hall (Small)" 1.8)
    ("Smooth Hall (Medium)" 3.0)
    ("Smooth Hall (Large)" 5.9)
    ("Smooth Hall (Large) - HD" 5.9)
    ("Vocal Plate" 3.1)
    ("Vocal Plate - HD" 3.1)
    ("Warble Chamber" 4.0)
    ("Warehouse" 6.0)
    ("Warehouse - HD" 6.0))
  "Table of tap-reverb presets along with recommended decay values.")

(defun emacspeak-m-player-apply-reverb-preset (preset)
  "Prompt for a predefined reverb preset and apply it.
You need to use mplayer built with ladspa support, and have package
tap-reverb already installed."
  (interactive
   (list
    (let ((completion-ignore-case t))
      (completing-read "Preset: "
                       emacspeak-m-player-tap-reverb-presets nil 'must-match))))
  (cl-declare (special emacspeak-m-player-tap-reverb-presets
                       emacspeak-m-player-reverb-preset-table
                       emacspeak-m-player-process
                       emacspeak-m-player-reverb-filter))
  (let ((setting (assoc preset emacspeak-m-player-tap-reverb-presets))
        (ladspa (getenv "LADSPA_PATH"))
        (filter-spec nil)
        (filter nil))
    (unless (process-live-p emacspeak-m-player-process)
      (error "No media playing  currently."))
    (unless ladspa
      (setq ladspa (setenv "LADSPA_PATH" "/usr/lib/ladspa")))
    (unless (file-exists-p (expand-file-name "tap_reverb.so" ladspa))
      (error "Package tap_reverb not installed."))
    (setq filter-spec
          `("ladspa=tap_reverb:tap_reverb"
            ,(round (* 1000 (cl-second setting))) ;  delay  in ms
            0 -7                               ; dry and wet db
            1 1 1 1
                                        ; preset name
            ,(cadr (assoc (cl-first setting)
                          emacspeak-m-player-reverb-preset-table))))
    (setq emacspeak-m-player-reverb-filter filter-spec)
    (setq filter (mapconcat #'(lambda (v) (format "%s" v)) filter-spec ":"))
    (emacspeak-m-player-dispatch "af_clr")
    (emacspeak-m-player-dispatch
     (format "af_add %s" filter))
    (emacspeak-auditory-icon 'button)))

;;}}}
;;{{{ Play RSS Stream:
;;;###autoload
(defun emacspeak-m-player-play-rss (rss-url)
  "Play an RSS stream by converting to  an M3U playlist."
  (interactive
   (list
    (emacspeak-eww-read-url)))
  (let* ((file (make-temp-file  "rss-media" nil ".m3u"))
         (buffer (find-file-noselect file)))
    (message "Retrieving playlist.")
    (with-current-buffer buffer
      (insert-buffer-substring
       (emacspeak-xslt-xml-url
        (emacspeak-xslt-get "rss2m3u.xsl")
        rss-url))
      (save-buffer))
    (emacspeak-m-player file 'playlist)))
;;}}}
;;{{{ Use locate to construct media playlist:

(defvar emacspeak-locate-media-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'emacspeak-locate-play-results-as-playlist)
    map)
  "Keymap used to play locate results.")

;;;###autoload
(defun emacspeak-m-player-locate-media (pattern)
  "Locate media matching specified pattern.  The results can be
played as a play-list by pressing [RET] on the first line.
Pattern is first converted to a regexp that accepts common
punctuation separators (-,._\'\") in place of white-space.
Results are placed in a Locate buffer and can be played using
M-Player --- use \\[emacspeak-dired-open-this-file] locally bound to C-RET 
to play individual tracks."
  (interactive "sSearch Pattern: ")
  (cl-declare  (special emacspeak-media-extensions))
  (let ((inhibit-read-only t)
        (locate-make-command-line #'(lambda (s) (list locate-command "-i" "--regexp" s))))
    (locate-with-filter
     (mapconcat #'identity
                (split-string pattern)
                "[ '/\"_.,-]")
     emacspeak-media-extensions)
    (goto-char (point-min))
    (message "Buffer: %s" (current-buffer))
    (put-text-property
     (line-beginning-position) (line-end-position)
     'keymap  emacspeak-locate-media-map)
    (emacspeak-auditory-icon 'open-object)
    (rename-buffer (format "Media  matching %s" pattern))
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{ MultiPlayer Support:

(defun emacspeak-m-player-persist-process (&optional name)
  "Persists current m-player process instance by renaming its buffer.
Optional interactive prefix arg prompts for name to use for  player."
  (interactive "P")
  (cl-declare (special  emacspeak-m-player-process))
  (when (process-live-p emacspeak-m-player-process)
    (with-current-buffer  (process-buffer emacspeak-m-player-process)
      (set (make-local-variable 'emacspeak-m-player-process)emacspeak-m-player-process)
      (set-default 'emacspeak-m-player-process nil)
      (rename-buffer
       (if name
           (format "*%s*" (read-from-minibuffer "Name: "))
         "*Persisted-M-Player*")
       'unique))
    (when (called-interactively-p 'interactive)
      (emacspeak-auditory-icon 'task-done)
      (dtk-notify-say "persisted current process. You can now start another player."))))

(defun emacspeak-m-player-restore-process ()
  "Restore emacspeak-m-player-process from current buffer.
Check first if current buffer is in emacspeak-m-player-mode."
  (interactive)
  (cl-declare (special emacspeak-m-player-process))
  (unless (eq major-mode 'emacspeak-m-player-mode)
    (error "This is not an MPlayer buffer."))
  (let ((proc
         (or (get-buffer-process (current-buffer))
             emacspeak-m-player-process)))
    (cond
     ((process-live-p proc)
      (setq emacspeak-m-player-process proc)
      (set-default 'emacspeak-m-player-process proc)
      (emacspeak-auditory-icon 'open-object)
      (message "Restored  player process."))
     (t (error "No live player here.")))))

;;}}}
;;{{{ Panning:

(defvar emacspeak-m-player-panner 0
  "The 11 pre-defined panning locations,.")

(make-variable-buffer-local 'emacspeak-m-player-panner)

(defun emacspeak-m-player-pan ()
  "Pan from left to right   and back from right to left one step at a time."
  (interactive)
  (cl-declare (special emacspeak-m-player-panner emacspeak-m-player-process))
  (unless (process-live-p emacspeak-m-player-process) (error "No   player."))
  (let* ((this (abs  (/ emacspeak-m-player-panner 10.0)))
         (pan (format "%.1f:%.1f" (- 1  this)  this)))
    (emacspeak-m-player-dispatch  "af_del pan, channels")
    (emacspeak-m-player-dispatch (format "af_add pan=2:%s:%s" pan pan))
    (setq emacspeak-m-player-panner (1+ emacspeak-m-player-panner))
    (when (= 10 emacspeak-m-player-panner) (setq emacspeak-m-player-panner -10))
    (message "Panned  to %.1f %.1f" (- 1 this) this)))

;;}}}
;;{{{ Apply Ladspa to MPlayer:

(defun emacspeak-m-player-ladspa-cmd (plugin)
  "Convert Ladspa Plugin to M-Player command args."
  (format
   "ladspa=%s:%s:%s"
   (ladspa-plugin-library plugin) (ladspa-plugin-label plugin)
   (mapconcat #'ladspa-control-value (ladspa-plugin-controls plugin) ":")))

(defun emacspeak-m-player-add-ladspa ()
  "Apply plugin to running MPlayer.
Copies  invocation string to kill-ring so it can be added easily to
our pre-defined filters if appropriate."
  (interactive)
  (cl-declare (special emacspeak-m-player-process))
  (unless (eq major-mode 'ladspa-mode) (error "This is not a Ladspa buffer"))
  (unless (get-text-property (point) 'ladspa)
    (error "No Ladspa Plugin here."))
  (unless (process-live-p emacspeak-m-player-process)
    (error "No running MPlayer."))
  (let ((result nil)
        (plugin (get-text-property (point) 'ladspa))
        (args nil))
    (when
        (cl-some
         #'null (mapcar #'ladspa-control-value (ladspa-plugin-controls plugin)))
      (ladspa-instantiate))
    (setq args (emacspeak-m-player-ladspa-cmd plugin))
    (kill-new args)
    (setq result
          (emacspeak-m-player-dispatch (format "af_add %s" args)))
    (when (called-interactively-p 'interactive)
      (message   "%s"
                 (or result "Waiting")))))


(defun emacspeak-m-player-delete-ladspa ()
  "Delete plugin from  running MPlayer."
  (interactive)
  (cl-declare (special emacspeak-m-player-process))
  (unless (eq major-mode 'ladspa-mode) (error "This is not a Ladspa buffer"))
  (unless (process-live-p emacspeak-m-player-process)
    (error "No running MPlayer."))

  (emacspeak-m-player-dispatch "af_del ladspa"))

;;}}}
;;{{{ Clipping:

;;; Functionality restored from emacspeak-alsaplayer.el:

(defvar-local emacspeak-m-player-clip-start nil
  "Start position of clip.")

(defvar-local emacspeak-m-player-clip-end nil
  "End position of clip.")

(defun emacspeak-m-player-set-clip-start    ()
  "Set start of clip marker."
  (interactive)
  (setq emacspeak-m-player-clip-start
        (read (cl-first (emacspeak-m-player-get-position))))
  (when  (called-interactively-p 'interactive)
    (message "mark set at %s" emacspeak-m-player-clip-start)
    (emacspeak-auditory-icon 'mark-object)))

(defun emacspeak-m-player-set-clip-end    ()
  "Set end of clip marker."
  (interactive)
  (cl-declare (special emacspeak-m-player-clip-end))
  (setq emacspeak-m-player-clip-end
        (read (cl-first (emacspeak-m-player-get-position))))
  (when  (called-interactively-p 'interactive)
    (message "mark set at %s" emacspeak-m-player-clip-end)
    (emacspeak-auditory-icon 'mark-object)))

(defun emacspeak-m-player-write-clip ()
  "Invoke mp3splt to clip selected range in current file."
  (interactive)
  (cl-declare (special emacspeak-sox
                       emacspeak-m-player-clip-end emacspeak-m-player-clip-start))
  (cl-assert emacspeak-sox  nil "SoX needs to be installed to use this command.")
  (cl-assert (eq major-mode 'emacspeak-m-player-mode) nil "Not in an MPlayer buffer.")
  (cl-assert (numberp emacspeak-m-player-clip-start) nil "Set start of clip with M-[")
  (cl-assert (numberp emacspeak-m-player-clip-end) nil "Set end of clip with M-]")
  (let ((file (cl-second (emacspeak-m-player-get-position))))
    (shell-command
     (format "%s '%s' 'clip-%s'  trim %s %s"
             emacspeak-sox file file 
             emacspeak-m-player-clip-start
             (- emacspeak-m-player-clip-end emacspeak-m-player-clip-start)))
    (message "Wrote clip to clip-%s" file)))

;;}}}
(provide 'emacspeak-m-player)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}


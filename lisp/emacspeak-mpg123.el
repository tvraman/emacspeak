;;; emacspeak-mpg123.el --- Speech enable MP3 Player
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak extension to speech-enable MP3 player
;;; Keywords: Emacspeak, WWW interaction
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1995 -- 2007, T. V. Raman<raman@cs.cornell.edu>
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

;;{{{ required modules

(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; This module speech-enables  MPG123
;;; MPG123 is an MP3 player.

;;; Code:

;;}}}
;;{{{ helpers 
(defsubst emacspeak-mpg123-current-track ()
  "Return current rtrack number if on a valid line."
  (when (mpg123:in-music-list-p)
    (let ((start nil))
      (save-excursion
        (beginning-of-line)
        (skip-chars-forward " ")
        (setq start (point))
        (skip-chars-forward "0-9")
        (string-to-number (buffer-substring-no-properties
                           start (point)))))))

(defsubst emacspeak-mpg123-get-music-info (n attr)
  "Return attribute from music alist.
mpg123 defines this as a macro which causes compile trouble."
  (declare (special mpg123*music-alist))
  (cdr (assq attr
             (assoc n mpg123*music-alist))))

(defsubst emacspeak-mpg123-speak-line ()
  "Speak just the title"
  (emacspeak-mpg123-speak-title))
;;}}}
;;{{{ advice interactive commands to speak

(defadvice mpg123-backward (after emacspeak pre act comp)
  "Speak position."
  (when (interactive-p)
    (emacspeak-mpg123-speak-current-time)))

(defadvice mpg123-forward (after emacspeak pre act comp)
  "Speak position."
  (when (interactive-p)
    (emacspeak-mpg123-speak-current-time)))
(defadvice mpg123-backward-10 (after emacspeak pre act comp)
  "Speak position."
  (when (interactive-p)
    (emacspeak-mpg123-speak-current-time)))
(defadvice mpg123-forward-10 (after emacspeak pre act comp)
  "Speak position."
  (when (interactive-p)
    (emacspeak-mpg123-speak-current-time)))

  

(defadvice mpg123-next-line (after emacspeak pre act comp)
  "Speak line moveed to."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-mpg123-speak-line)))

(defadvice mpg123-prev-line (after emacspeak pre act comp)
  "Speak line moveed to."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-mpg123-speak-line)))

(defadvice mpg123-mark-position (after emacspeak pre act comp)
  "Speak line moveed to."
  (when (interactive-p)
    (emacspeak-mpg123-speak-line)))

(defadvice  mpg123-yank-line (after emacspeak pre act comp)
  "Speak track we yanked."
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-mpg123-speak-title)))

(defadvice  mpg123-kill-line (before emacspeak pre act comp)
  "Speak track we killed."
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-mpg123-speak-title)))

(defadvice  mpg123-quit (after emacspeak pre act comp)
  "Speak modeline of what becomes current."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{ additional status commands

(defun emacspeak-mpg123-speak-filename ()
  "Speak filename of the current song."
  (interactive)
  (message
   (emacspeak-mpg123-get-music-info (emacspeak-mpg123-current-track)
                                    'filename)))

(defun emacspeak-mpg123-speak-title ()
  "Speak title of the current song."
  (interactive)
  (message
   (emacspeak-mpg123-get-music-info (emacspeak-mpg123-current-track) 'name)))

(defun emacspeak-mpg123-speak-length ()
  "Speak duration of the current song."
  (interactive)
  (message 
   (emacspeak-mpg123-get-music-info (emacspeak-mpg123-current-track) 'length)))
(defun emacspeak-mpg123-speak-current-time ()
  "Speak time in current track."
  (interactive)
  (declare (special mpg123-mode-map))
  (unless (mpg123:in-music-list-p)
    (error "Not on a valid MP3 song"))
  (let ((start nil))
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward "^:")
      (skip-chars-backward "^ ")
      (setq start (point))
      (skip-chars-forward "0-9:")
      (dtk-speak (buffer-substring  start (point))))))

;;{{{ keys 
(declaim (special mpg123-mode-map))
(define-key mpg123-mode-map "t" 'emacspeak-mpg123-speak-title)
(define-key mpg123-mode-map "l"
  'emacspeak-mpg123-speak-length)
(define-key mpg123-mode-map '[left]
  'emacspeak-aumix-wave-decrease)
(define-key mpg123-mode-map '[right] 'emacspeak-aumix-wave-increase)
(define-key mpg123-mode-map "c"
  'emacspeak-mpg123-speak-current-time)
(define-key mpg123-mode-map "."
  'emacspeak-mpg123-speak-filename)

;;}}}

;;}}}
;;{{{  playlist support 

;;; Commentary:
;;; Ideally this should be part of mpg123.el 
;;; Play an mp3 playlist  with a random shuffle.
;;;  Allow skipping of tracks with a single keystroke.

(defvar emacspeak-mp3-playlist-process  nil
  "Process that is playing the playlist. ")

(defvar emacspeak-mp3-play-program "mpg123"
  "Program that plays mp3 files. ")

(defun emacspeak-mp3-playlist-play (playlist &optional dont-shuffle)
  "Play a playlist. 
Optional interactive prefix arg says not to shuffle  the list. 
Use command \\[emacspeak-mp3-playlist-skip] 
to skip to the next track. "
  (interactive
   (list
    (read-file-name "Playlist: ")
    current-prefix-arg))
  (declare (special emacspeak-mp3-playlist-process
                    emacspeak-mp3-play-program))
  (setq emacspeak-mp3-playlist-process
        (apply 'start-process
               "*emacspeak-mp3*" "*emacspeak-mp3*"
               emacspeak-mp3-play-program
               (delq nil 
                     (list 
                      (unless dont-shuffle "--shuffle")
                      "-@"
                      (expand-file-name playlist))))))

(defun emacspeak-mp3-playlist-skip ()
  "Skip currently playing track. "
  (interactive)
  (declare (special emacspeak-mp3-playlist-process))
  (process-send-string
   emacspeak-mp3-playlist-process
   (format "%c" 3))
  (message "Skipped track. "))

(defun emacspeak-mp3-playlist-stop ()
  "Kill currently playing playlist. "
  (interactive)
  (declare (special emacspeak-mp3-playlist-process))
  (kill-process emacspeak-mp3-playlist-process)
  (message "Stopped playlist. "))

(declaim (special mpg123-mode-map))

(define-key  mpg123-mode-map "L"
  'emacspeak-mp3-playlist-play)
(define-key mpg123-mode-map "S"
  'emacspeak-mp3-playlist-skip)
(define-key mpg123-mode-map "K" 'emacspeak-mp3-playlist-stop)

;;}}}
;;{{{ additional temporal navigation

(defun emacspeak-mpg123-forward-minute (arg)
  "Forward by ARG minutes."
  (interactive "p")
  (mpg123-forward (* 60 arg)))

(defun emacspeak-mpg123-backward-minute (arg)
  "Move back by specified number of minutes."
  (interactive "p")
  (mpg123-forward (* -60 arg)))

(define-key mpg123-mode-map "\M-f"
  'emacspeak-mpg123-forward-minute)
(define-key mpg123-mode-map "\M-b" 'emacspeak-mpg123-backward-minute)
;;}}}
(provide 'emacspeak-mpg123)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

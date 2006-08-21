;;; emacspeak-m-player.el --- Control mplayer from Emacs
;;; $Id$
;;; $Author$
;;; Description: Controlling mplayer from emacs 
;;; Keywords: Emacspeak, m-player streaming media 
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

;;; Copyright (c) 1995 -- 2003, T. V. Raman
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

;;; Defines a simple derived mode for interacting with mplayer.
;;; mplayer  is a versatile media player capable of playing many
;;; streaming formats  and is especially useful for playing windows
;;; media (WMA) and streaming windows media (ASF) files.
;;;mplayer is available  on the WWW:
;;; RPM package
;;;http://mirrors.sctpc.com/dominik/linux/pkgs/mplayer/i586/mplayer-0.90pre5-2.i586.rpm
;;;You may need the  win32 codecs which can be downloaded from 
;;;http://ftp.lug.udel.edu/MPlayer/releases/w32codec-0.60.tar.bz2
;;;Mplayer FAQ at
;;;http://www.mplayerhq.hu/DOCS/faq.html
;;; Mplayer docs at 
;;; http://www.mplayerhq.hu/DOCS/
;;; Code:

;;}}}
;;{{{  Required modules

(require 'emacspeak-preamble)
;;}}}
;;{{{ define a derived mode for m-player interaction 
(defvar emacspeak-m-player-process nil
  "Process handle to m-player." )

(define-prefix-command 'emacspeak-m-player-prefix-command
  'emacspeak-m-player-mode-map)
(declaim (special emacspeak-aumix-multichannel-capable-p
                  emacspeak-use-auditory-icons))
(define-derived-mode emacspeak-m-player-mode fundamental-mode 
  "M-Player Interaction"
  "Major mode for m-player interaction. \n\n
\\{emacspeak-m-player-mode-map}"
  (when (and (not  emacspeak-aumix-multichannel-capable-p)
             emacspeak-use-auditory-icons)
    (emacspeak-toggle-auditory-icons))
  (setq emacspeak-m-player-process (get-buffer-process
                                    (current-buffer))))
  
(declaim (special emacspeak-m-player-mode-map))

;;}}}
;;{{{ emacspeak-m-player
;;;###autoload
(defgroup emacspeak-m-player nil
  "Emacspeak media player settings.")
(defcustom emacspeak-m-player-program "mplayer"
  "Media player program."
  :type 'string
  :group 'emacspeak-m-player)

(defcustom emacspeak-m-player-options 
  (list "-slave" "-quiet"
	"-nortc")
  "Options passed to mplayer."
  :type  '(repeat
	   (string :tag "option"))
  :group 'emacspeak-m-player)
;;;###autoload
(defun emacspeak-m-player (resource )
  "Play specified resource using m-player.
Resource is an  MP3 file or m3u playlist.
The player is placed in a buffer in emacspeak-m-player-mode."
  (interactive
   (list
    (let ((completion-ignore-case t)
          (emacspeak-speak-messages nil)
          (minibuffer-history emacspeak-realaudio-history))
      (emacspeak-pronounce-define-local-pronunciation
       emacspeak-realaudio-shortcuts-directory " shortcuts/ ")
      (read-file-name "Media resource: "
                      emacspeak-realaudio-shortcuts-directory
                      emacspeak-realaudio-last-url))))
  (declare (special emacspeak-m-player-process
                    emacspeak-m-player-program
                    emacspeak-m-player-options))
  (unless (string-match "^http:"  resource)
    (setq resource
          (expand-file-name resource)))
  (when (and emacspeak-m-player-process
             (eq 'run (process-status
                       emacspeak-m-player-process))
             (y-or-n-p "Stop currently playing music? "))
    (kill-buffer (process-buffer emacspeak-m-player-process))
    (setq emacspeak-m-player-process nil))
  (let ((process-connection-type nil)
        (playlist-p (or
                     (string-match ".m3u$"  resource)
                     (string-match ".pls$"  resource)))
        (options (copy-sequence emacspeak-m-player-options)))
    (setq options
          (nconc options
                 (if playlist-p
		     (list "-playlist" resource)
		   (list resource))))
    (setq emacspeak-m-player-process
          (apply 'start-process
                 "m-player" "m-player" emacspeak-m-player-program
                 options))
    (switch-to-buffer (process-buffer emacspeak-m-player-process))
    (emacspeak-m-player-mode)
    (ansi-color-for-comint-mode-on)))

;;}}}
;;{{{ commands 

(defsubst emacspeak-m-player-dispatch (command)
  "Dispatch command to m-player."
  (declare (special emacspeak-m-player-process))
  (process-send-string
   emacspeak-m-player-process
   (format "%s\n" command)))

(defun emacspeak-m-player-play-tree-step (step)
  "Move within the play tree."
  (interactive
   (list
    (read-from-minibuffer "Move by: ")))
  (emacspeak-m-player-dispatch
   (format "pt_step %d" step)))
(defun emacspeak-m-player-play-tree-up (step)
  "Move within the play tree."
  (interactive
   (list
    (read-from-minibuffer "Move by: ")))
  (emacspeak-m-player-dispatch
   (format "pt_up %d" step)))
(defun emacspeak-m-player-alt-src-step (step)
  "Move within an ASF playlist."
  (interactive
   (list
    (read-from-minibuffer "Move by: ")))
  (emacspeak-m-player-dispatch
   (format "alt_src_step %d" step)))
(defun emacspeak-m-player-seek-relative (offset)
  "Seek  by offset into stream from current position."
  (interactive
   (list
    (read-from-minibuffer "Offset: ")))
  (emacspeak-m-player-dispatch
   (format "seek %d" offset)))
(defun emacspeak-m-player-seek-absolute (position)
  "Seek  to absolute specified position."
  (interactive
   (list
    (read-from-minibuffer "Seek to percentage: ")))
  (emacspeak-m-player-dispatch
   (format "seek %d 1" position )))
(defun emacspeak-m-player-pause ()
  "Pause or unpause media player."
  (interactive)
  (emacspeak-m-player-dispatch
   "pause"))
(defun emacspeak-m-player-quit ()
  "Quit media player."
  (interactive)
  (emacspeak-m-player-dispatch
   "quit"))

;;}}}
;;{{{ keys 

(define-key emacspeak-m-player-mode-map [left]
  'emacspeak-aumix-wave-decrease)
(define-key emacspeak-m-player-mode-map [right] 'emacspeak-aumix-wave-increase)
(define-key emacspeak-m-player-mode-map "s"
  'emacspeak-m-player-seek-relative)
(define-key emacspeak-m-player-mode-map "S"
  'emacspeak-m-player-seek-absolute)
(define-key emacspeak-m-player-mode-map "p" 'emacspeak-m-player-play-tree-step)
(define-key emacspeak-m-player-mode-map "P"
  'emacspeak-m-player-play-tree-up)

(define-key emacspeak-m-player-mode-map "a"
  'emacspeak-m-player-alt-src-step)
(define-key emacspeak-m-player-mode-map " " 'emacspeak-m-player-pause)
(define-key emacspeak-m-player-mode-map "q" 'emacspeak-m-player-quit)
;;}}}
(provide 'emacspeak-m-player)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

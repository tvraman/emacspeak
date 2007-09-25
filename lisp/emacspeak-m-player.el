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
;;;  $Revision: 4532 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (c) 1995 -- 2007, T. V. Raman
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
(require 'desktop)
(require 'dired)
(require 'comint)

;;}}}
;;{{{ define a derived mode for m-player interaction 

(defvar emacspeak-m-player-process nil
  "Process handle to m-player." )

(define-prefix-command 'emacspeak-m-player-prefix-command
  'emacspeak-m-player-mode-map)

(define-derived-mode emacspeak-m-player-mode comint-mode 
  "M-Player Interaction"
  "Major mode for m-player interaction. \n\n
\\{emacspeak-m-player-mode-map}"
  (setq emacspeak-m-player-process (get-buffer-process (current-buffer))))

;;}}}
;;{{{ emacspeak-m-player
;;;###autoload

(defgroup emacspeak-m-player nil
  "Emacspeak media player settings."
  :group 'emacspeak)

(defcustom emacspeak-m-player-program "mplayer"
  "Media player program."
  :type 'string
  :group 'emacspeak-m-player)
(defvar emacspeak-m-player-default-options
  (list "-slave"  "-nortc" )
  "Default options for MPlayer.")

(defcustom emacspeak-m-player-options 
  (copy-sequence emacspeak-m-player-default-options)
  "Options passed to mplayer."
  :type  '(repeat
           (string :tag "option"))
  :group 'emacspeak-m-player)

;;;###autoload
(defun emacspeak-multimedia  ()
  "Start or control Emacspeak multimedia player."
  (interactive )
  (declare (special emacspeak-m-player-process))
  (cond
   ((and emacspeak-m-player-process
         (eq 'run (process-status emacspeak-m-player-process)))
    (call-interactively 'emacspeak-m-player-command)   )
   (t  (call-interactively 'emacspeak-m-player))))

(defun emacspeak-m-player-command (command-char)
  "Invoke MPlayer commands."
  (interactive
   (list
    (read-char "MPlayer Command: ")))
  (declare (special emacspeak-m-player-process))
  (cond
   ((=  command-char ?\;)
    (pop-to-buffer (process-buffer
                    emacspeak-m-player-process)
                   nil 'norecord)
    (set-window-text-height nil 3)
    (emacspeak-speak-mode-line))
   (t
    (save-window-excursion
      (call-interactively
       (lookup-key emacspeak-m-player-mode-map
                   (format "%c" command-char)))))))


(defsubst emacspeak-m-player-playlist-p (resource)
  "Check if specified resource matches a playlist type."
  (string-match
   (regexp-opt
    (list ".m3u$" ".asx$" ".pls$" ".rpm$" ".ram$"  ))
   resource))

;;;###autoload
(defun emacspeak-m-player (resource &optional play-list noselect)
  "Play specified resource using m-player.
Optional prefix argument play-list interprets resource as a play-list.
Resource is a media resource or playlist containing media resources.
The player is placed in a buffer in emacspeak-m-player-mode."
  (interactive
   (list
    (let ((completion-ignore-case t)
          (emacspeak-speak-messages nil)
          (minibuffer-history emacspeak-realaudio-history))
      (emacspeak-pronounce-define-local-pronunciation
       emacspeak-realaudio-shortcuts-directory " shortcuts/ ")
      (read-file-name "MP3 Resource: "
                      (if
                          (string-match
                           "mp3" (expand-file-name default-directory))
                          default-directory
                        emacspeak-realaudio-shortcuts-directory)
                      (when (eq major-mode 'dired-mode)
                        (dired-get-filename))))
    current-prefix-arg
    current-prefix-arg))
  (declare (special emacspeak-realaudio-history emacspeak-realaudio-shortcuts-directory
                    emacspeak-m-player-process
                    emacspeak-m-player-program
                    emacspeak-m-player-options))
  (unless (string-match "^[a-z]+:"  resource)
    (setq resource (expand-file-name resource)))
  (when (and emacspeak-m-player-process
             (eq 'run (process-status
                       emacspeak-m-player-process))
             (y-or-n-p "Stop currently playing music? "))
    (emacspeak-m-player-quit)
    (setq emacspeak-m-player-process nil))
  (let ((process-connection-type nil)
        (playlist-p
         (or play-list
             (emacspeak-m-player-playlist-p resource)))
        (options (copy-sequence emacspeak-m-player-options)))
    (setq options
          (cond
           (playlist-p
            (nconc options (list "-playlist" resource)))
           ((file-directory-p resource)
            (nconc options
                   (directory-files (expand-file-name resource)
                                    'full
                                    "\\(ogg$\\)\\|\\(mp3$\\)\\|\\(MP3$\\)")))
           (t (nconc options (list
                              (shell-quote-wildcard-pattern resource))))))
    (setq emacspeak-m-player-process
          (get-buffer-process
           (apply 'make-comint
                  "m-player" emacspeak-m-player-program
                  nil
                  options)))
    (save-excursion
      (set-buffer (process-buffer emacspeak-m-player-process))
      (emacspeak-m-player-mode)
      (setq buffer-undo-list t)
      (ansi-color-for-comint-mode-on))
    (unless noselect
      (switch-to-buffer (process-buffer emacspeak-m-player-process))
      (set-window-text-height nil 3))))

;;}}}
;;{{{ commands 

(defsubst emacspeak-m-player-dispatch (command)
  "Dispatch command to m-player."
  (declare (special emacspeak-m-player-process))
  (save-excursion
    (set-buffer (process-buffer emacspeak-m-player-process))
    (erase-buffer))
  (process-send-string
   emacspeak-m-player-process
   (format "%s\n" command)))

(defun emacspeak-m-player-scale-speed (factor)
  "Scale speed by specified factor."
  (interactive "nFactor:")
  (emacspeak-m-player-dispatch
   (format "speed_mult %f" factor)))
(defun emacspeak-m-player-slower ()
  "Slow down playback."
  (interactive)
  (emacspeak-m-player-scale-speed 0.9091))
(defun emacspeak-m-player-faster ()
  "Speed up  playback."
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

(defun emacspeak-m-player-play-tree-step (step)
  "Move within the play tree."
  (interactive"nSkip Tracks:")
  (emacspeak-m-player-dispatch
   (format "pt_step %d" step)))

(defun emacspeak-m-player-previous-track ()
  "Move to previous track."
  (interactive)
  (emacspeak-m-player-play-tree-step -1))

(defun emacspeak-m-player-next-track ()
  "Move to next track."
  (interactive)
  (emacspeak-m-player-play-tree-step 1))

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
  "Seek  by offset into stream from current position."
  (interactive
   (list
    (read-from-minibuffer "Offset: ")))
  (emacspeak-m-player-dispatch
   (format "seek %s" offset)))

(defun emacspeak-m-player-seek-absolute (position)
  "Seek  to absolute specified position."
  (interactive
   (list
    (read-from-minibuffer "Seek to percentage: ")))
  (emacspeak-m-player-dispatch
   (format "seek %s 1" position )))

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
  (emacspeak-m-player-dispatch
   "pause"))

(defun emacspeak-m-player-quit ()
  "Quit media player."
  (interactive)
  (when (eq (process-status emacspeak-m-player-process) 'run)
    (emacspeak-m-player-dispatch "quit"))
  (unless (eq (process-status emacspeak-m-player-process) 'exit)
    (delete-process  emacspeak-m-player-process))
  (bury-buffer)
  (emacspeak-speak-mode-line))

(defun emacspeak-m-player-volume-up ()
  "Increase volume."
  (interactive)
  (emacspeak-m-player-dispatch "volume 1"))

(defun emacspeak-m-player-volume-down ()
  "Decrease volume."
  (interactive)
  (emacspeak-m-player-dispatch "volume -1"))
   

(defun emacspeak-m-player-display-position ()
  "Display current position in track and its length."
  (interactive)
  (emacspeak-m-player-dispatch
   "get_percent_pos\nget_time_length\n"))

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

(defvar emacspeak-m-player-filters
  '(("hrtf" . "hrtf")
    ("sweep". "sweep")
    ("extrastereo" . "extrastereo")
    ("volnorm" . "volnorm")
    ("surround" . "surround"))
  "Table of useful MPlayer filters.")

(defun emacspeak-m-player-add-filter ()
  "Adds specified filter  to use for the next invocation of MPlayer."
  (interactive)
  (let ((filter-name
         (completing-read "Filter:"
                          emacspeak-m-player-filters)))
    (setq emacspeak-m-player-options
          (append emacspeak-m-player-options
                  (list "-af" filter-name)))))

(defun emacspeak-m-player-customize-options ()
  "Use Customize to manipulate MPlayer options."
  (interactive)
  (customize-variable 'emacspeak-m-player-options)
  (goto-char (point-min))
  (search-forward "INS"))

;;}}}
;;{{{ equalizer 

(defvar emacspeak-m-player-equalizer (make-vector 10 12)
  "Vector holding equalizer settings.")

(defun emacspeak-m-player-equalizer-control (v)
  "Manipulate values in specified vector using minibuffer."
  (interactive)
  (let ((column 0)
        (key nil)
        (continue t))
    (while  continue
      (setq key  (read-key-sequence
                  (format "G%s:%s" column (aref v column))))
      (cond
       ((equal key [left])
        (setq column (% (+ 9  column) 10)))
       ((equal key [right])
        (setq column (% (1+ column) 10)))
       ((equal key [up])
        (aset v   column
              (min 12 (1+ (aref v column)))))
       ((equal key [down])
        (aset v   column
              (max -12 (1- (aref v column)))))
       ((equal key [prior])
        (aset v   column
              (min 12 (+ 4  (aref v column)))))
       ((equal key [next])
        (aset v   column
              (max -12 (- (aref v column)  4))))
       ((equal key [home])
        (aset v   column 12))
       ((equal key [end])
        (aset v   column -12))
       ((equal key "\C-g") (error "Did not change equalizer."))
       ((equal key "\C-m")
        (setq continue nil))))
    (mapconcat
     #'(lambda (value) (format "%d" value))
     v  ":")))

(defun emacspeak-m-player-add-equalizer ()
  "Add equalizer for next MPlayer invocation.

Use arrow keys, page-up, page-down, home and end keys to
  manipulate the values.
Hit enter to finish setting the equalizer values.

The Mplayer equalizer provides 10 bands, G0 -- G9, see the
  MPlayer man page for details."
  (interactive)
  (declare (special emacspeak-m-player-equalizer
                    emacspeak-m-player-options))
  (setq emacspeak-m-player-options
        (append emacspeak-m-player-options
                (list "-af"
                      (format "equalizer=%s"
                              (emacspeak-m-player-equalizer-control emacspeak-m-player-equalizer))))))
(defun emacspeak-m-player-reset-options ()
  "Reset MPlayer options to initial defaults."
  (interactive)
  (declare (special emacspeak-m-player-default-options
                    emacspeak-m-player-options))
  (setq emacspeak-m-player-options
        emacspeak-m-player-default-options)
  (message "Reset options."))

  
;;}}}
;;{{{ keys
(declaim (special emacspeak-m-player-mode-map))
(loop for k in 
      '(
        ("\C-m" emacspeak-m-player)
        (":" emacspeak-m-player)
        ("e" emacspeak-m-player-add-equalizer)
        ("o" emacspeak-m-player-customize-options)
        ("O" emacspeak-m-player-reset-options)
        ("f" emacspeak-m-player-add-filter)
        ("b" bury-buffer)
        ("l" emacspeak-m-player-load-file)
        ("L" emacspeak-m-player-load-playlist)
        ("?" emacspeak-m-player-display-position)
        ("t" emacspeak-m-player-play-tree-step)
        ("p" emacspeak-m-player-previous-track)
        ("n" emacspeak-m-player-next-track)
        ("," emacspeak-m-player-backward-10s)
        ("." emacspeak-m-player-forward-10s)
        ([left] emacspeak-m-player-backward-10s)
        ([right] emacspeak-m-player-forward-10s)
        ([up] emacspeak-m-player-backward-1min)
        ([down] emacspeak-m-player-forward-1min)
        ("<" emacspeak-m-player-backward-1min)
        (">" emacspeak-m-player-forward-1min)
        ([prior] emacspeak-m-player-backward-10min)
        ([next] emacspeak-m-player-forward-10min)
        ([home] emacspeak-m-player-beginning-of-track)
        ([end] emacspeak-m-player-end-of-track)
        ("s" emacspeak-m-player-scale-speed)
        ("[" emacspeak-m-player-slower)
        ("]" emacspeak-m-player-faster)
        ("{" emacspeak-m-player-half-speed)
        ("}" emacspeak-m-player-double-speed)
        ("\d" emacspeak-m-player-reset-speed)
        ("r" emacspeak-m-player-seek-relative)
        ("g" emacspeak-m-player-seek-absolute)
        (" " emacspeak-m-player-pause)
        ("q" emacspeak-m-player-quit)
        ("-" emacspeak-m-player-volume-down)
        ("=" emacspeak-m-player-volume-up)
        ("+" emacspeak-m-player-volume-up)
        )
      do
      (emacspeak-keymap-update  emacspeak-m-player-mode-map k))

;;}}}
(provide 'emacspeak-m-player)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

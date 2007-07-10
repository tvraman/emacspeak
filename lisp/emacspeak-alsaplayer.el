;;; emacspeak-alsaplayer.el --- Control alsaplayer from Emacs
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description: Controlling alsaplayer from emacs 
;;; Keywords: Emacspeak, alsaplayer
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

;;; Defines a simple derived mode for interacting with
;;; alsaplayer.
;;; alsaplayer navigation commands  work via single keystrokes.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'emacspeak-preamble)
;;}}}
;;{{{ define a derived mode for alsaplayer interaction

(defvar emacspeak-alsaplayer-process nil
  "Process handle to alsaplayer." )

;;;###autoload
(define-prefix-command 'emacspeak-alsaplayer-prefix-command
  'emacspeak-alsaplayer-mode-map)

(define-derived-mode emacspeak-alsaplayer-mode fundamental-mode 
  "Alsaplayer Interaction"
  "Major mode for alsaplayer interaction. \n\n
\\{emacspeak-alsaplayer-mode-map}")

;;}}}
;;{{{ launch  emacspeak-alsaplayer


(defgroup emacspeak-alsaplayer nil
  "AlsaPlayer from emacs.")

;;;###autoload
(defcustom emacspeak-alsaplayer-auditory-feedback t
  "Turn this on if you want spoken feedback and auditory icons from alsaplayer."
  :type 'boolean
  :group 'emacspeak-alsaplayer)
;;;###autoload
(defcustom emacspeak-alsaplayer-program
  "alsaplayer"
  "Alsaplayer executable."
  :type 'string
  :group 'emacspeak-alsaplayer)
;;;###autoload
(defcustom emacspeak-alsaplayer-media-directory
  (expand-file-name "~/mp3/")
  "Directory to look for media files."
  :type 'directory
  :group 'emacspeak-alsaplayer)
(defvar emacspeak-alsaplayer-buffer "*alsaplayer*"
  "Buffer for alsaplayer interaction.")

;;;###autoload
(defun emacspeak-alsaplayer-launch ()
  "Launch Alsaplayer.
user is placed in a buffer associated with the newly created
Alsaplayer session."
  (interactive)
  (declare (special emacspeak-alsaplayer-program
                    emacspeak-alsaplayer-buffer))
  (let ((buffer (get-buffer-create emacspeak-alsaplayer-buffer)))
    (save-excursion
      (set-buffer buffer)
      (setq buffer-undo-list t)
      (shell-command
       (format "%s -r -i daemon &" emacspeak-alsaplayer-program)
       (current-buffer)))
    (switch-to-buffer buffer)
    (emacspeak-alsaplayer-mode)
    (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
      (emacspeak-auditory-icon 'open-object)
      (emacspeak-speak-mode-line))))

;;}}}
;;{{{  Invoke commands:

(defun emacspeak-alsaplayer-send-command(command &optional watch-pattern no-refresh)
  "Send command to Alsaplayer.
Optional second arg watch-pattern specifies line of output to
  focus on.  Optional third arg no-refresh is used to avoid
  getting status twice."
  (declare (special emacspeak-alsaplayer-buffer))
  (save-excursion
    (set-buffer (get-buffer-create emacspeak-alsaplayer-buffer))
    (erase-buffer)
    (shell-command
     (format "%s %s %s"
             emacspeak-alsaplayer-program
             command
             (if no-refresh
                 ""
               "; alsaplayer --status"))
     (current-buffer)))
  (when (and watch-pattern
             (eq (current-buffer) (get-buffer emacspeak-alsaplayer-buffer)))
    (goto-char (point-min))
    (search-forward watch-pattern  nil t)))
         
(defun emacspeak-alsaplayer-add-to-queue (resource)
  "Add specified resource to queue."
  (interactive
   (list
    (read-file-name "Media Resource: "
                    (if 
                        (string-match "mp3" (expand-file-name default-directory))
                        default-directory
                      emacspeak-alsaplayer-media-directory))))
  (emacspeak-alsaplayer-send-command
   (format "--enqueue %s"
           (if (file-directory-p resource)
               (format "%s/*" resource)
             resource))
   "playlist_length:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defun emacspeak-alsaplayer-replace-queue (resource)
  "Replace currently playing music."
  (interactive
   (list
    (read-file-name "New MP3 Resource: "
                    emacspeak-alsaplayer-media-directory)))
  (emacspeak-alsaplayer-send-command
   (format "--replace %s"
           (if (file-directory-p resource)
               (format "%s/*" resource)
             resource))
   "playlist"
   "_length:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defun emacspeak-alsaplayer-status ()
  "Show alsaplayer status"
  (interactive)
  (emacspeak-alsaplayer-send-command "--status"
                                     "position:"
                                     'no-refresh)
  (when (interactive-p)
    (unless (eq (current-buffer)
                (get-buffer emacspeak-alsaplayer-buffer))
      (switch-to-buffer emacspeak-alsaplayer-buffer))
    (emacspeak-speak-line)
    (when  emacspeak-alsaplayer-auditory-feedback
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'select-object))))

(defun emacspeak-alsaplayer-pause ()
  "Pause or resume alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--pause"
                                     "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'button)))

(defun emacspeak-alsaplayer-next ()
  "Next  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--next"
                                     "path:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defun emacspeak-alsaplayer-previous ()
  "Previous  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--prev"
                                     "path:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))
    
(defun emacspeak-alsaplayer-start ()
  "Start  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--start"
                                     "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'open-object)))

(defun emacspeak-alsaplayer-stop ()
  "Stop  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--stop"
                                     "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'close-object)))

(defun emacspeak-alsaplayer-relative (offset)
  "Relative seek  alsaplayer"
  (interactive "sOffset")
  (emacspeak-alsaplayer-send-command
   (format  "--relative %s" offset)
   "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))

(defun emacspeak-alsaplayer-speed (setting)
  "Set speed in alsaplayer."
  (interactive "sSpeed")
  (emacspeak-alsaplayer-send-command
   (format "--speed %s" setting)
   "speed:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defun emacspeak-alsaplayer-volume (setting)
  "Set volume."
  (interactive "sVolume")
  (emacspeak-alsaplayer-send-command
   (format "--volume %d" setting)
   "volume:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defun emacspeak-alsaplayer-seek (offset)
  "Absolute seek  alsaplayer"
  (interactive "sPosition")
  (emacspeak-alsaplayer-send-command
   (format "--seek %s" offset)
   "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))

(defun emacspeak-alsaplayer-jump (track)
  "Jump to specified track."
  (interactive "sTrack Number:")
  (emacspeak-alsaplayer-send-command
   (format "--jump %s" track)
   "path:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))

(defun emacspeak-alsaplayer-clear ()
  "Clear or resume alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--clear"
                                     "playlist_length:")
  (when (and emacspeak-alsaplayer-auditory-feedback
             (interactive-p))
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'delete-object)))

(defun emacspeak-alsaplayer-quit ()
  "Quit or resume alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--quit")
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (when (eq major-mode 'emacspeak-alsaplayer-mode)
      (kill-buffer (current-buffer)))
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{ additional temporal navigation 

(defun emacspeak-alsaplayer-forward-10-seconds ( )
  "Skip forward by  seconds."
  (interactive)
  (emacspeak-alsaplayer-send-command "--relative 10"
                                     "position:")
  (when (interactive-p)
    (emacspeak-speak-line)))

(defun emacspeak-alsaplayer-backward-10-seconds()
  "Skip backward by  10 seconds."
  (interactive)
  (emacspeak-alsaplayer-send-command
   "--relative -10"
   "position:")
  (when (interactive-p)
    (emacspeak-speak-line)))

(defun emacspeak-alsaplayer-forward-minute ( minutes)
  "Skip forward by  minutes."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (format "--relative %s"
           (* 60 (or minutes 1)))
   "position:")
  (when (interactive-p)
    (emacspeak-speak-line)))

(defun emacspeak-alsaplayer-backward-minute ( minutes)
  "Skip backwards by  minutes."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (format
    "--relative -%s"
    (* 60 (or minutes 1)))
   "position:")
  (when (interactive-p)
    (emacspeak-speak-line)))

(defun emacspeak-alsaplayer-forward-ten-minutes ( minutes)
  "Skip forward by  chunks of ten minutes."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (format
    "--relative %s"
    (* 600 (or minutes 1)))
   "position:")
  (when (interactive-p)
    (emacspeak-speak-line)))

(defun emacspeak-alsaplayer-backward-ten-minutes ( minutes)
  "Skip backwards by  chunks of minutes."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (format
    "--relative -%s"
    (* 600 (or minutes 1)))
   "position:")
  (when (interactive-p)
    (emacspeak-speak-line)))

;;}}}
;;{{{  saving positions, marking and clipping:

(defvar emacspeak-alsaplayer-mark nil
  "Saved mark position.")

(defsubst emacspeak-alsaplayer-get-position ()
  "Return currently displayed position."
  (declare (special emacspeak-alsaplayer-buffer))
  (save-excursion
    (set-buffer emacspeak-alsaplayer-buffer)
    (goto-char (point-min))
    (when (search-forward "position:" nil t)
      (second
       (split-string
        (buffer-substring-no-properties
         (line-beginning-position)
         (line-end-position))
        ": ")))))

(defun emacspeak-alsaplayer-mark-position   ()
  "Mark currently displayed position."
  (interactive)
  (declare (special emacspeak-alsaplayer-mark))e
  (setq emacspeak-alsaplayer-mark
        (emacspeak-alsaplayer-get-position))
  (when (and (interactive-p)
             emacspeak-alsaplayer-mark)
    (message "mark set at %s"
             emacspeak-alsaplayer-mark)
    (emacspeak-auditory-icon 'mark-object)))

(defun emacspeak-alsaplayer-where ()
  "Speak current position and copy it to kill ring."
  (interactive)
  (let ((where (emacspeak-alsaplayer-get-position)))
    (when where
      (kill-new where)
      (emacspeak-auditory-icon 'yank-object)
      (message "%s" where))))

(defsubst emacspeak-alsaplayer-get-path ()
  "Return currently displayed path."
  (declare (special emacspeak-alsaplayer-buffer))
  (save-excursion
    (set-buffer emacspeak-alsaplayer-buffer)
    (goto-char (point-min))
    (when (search-forward "path:" nil t)
      (second
       (split-string
        (buffer-substring-no-properties
         (line-beginning-position)
         (line-end-position))
        ": ")))))

(defun emacspeak-alsaplayer-info ()
  "Speak current path and copy it to kill ring."
  (interactive)
  (let ((path (emacspeak-alsaplayer-get-path)))
    (when path
      (kill-new path)
      (emacspeak-auditory-icon 'yank-object)
      (message "%s" path))))

(defvar emacspeak-alsaplayer-mp3split-program "mp3splt"
  "Program used to clip mp3 files.")

(defun emacspeak-alsaplayer-clip (path start end)
  "Invoke mp3splt to clip selected range."
  (interactive
   (list
    (read-file-name "Path:")
    (read-minibuffer "Start: " emacspeak-alsaplayer-mark)
    (read-minibuffer "End: ")))
  (cd (file-name-directory path))
  (shell-command
   (format "%s %s %s %s"
           emacspeak-alsaplayer-mp3split-program
           path
           (format "%d.%d"
                   (/ start 60)
                   (% start 60))
           (format "%d.%d"
                   (/ end 60)
                   (% end 60)))))
           
                    

;;}}}
;;{{{ bind keys

(declaim (special emacspeak-alsaplayer-mode-map))

(loop for k in
      '(
        ("m" emacspeak-alsaplayer-mark-position)
        ("w" emacspeak-alsaplayer-where)
        ("x" emacspeak-alsaplayer-clip)
        ("." emacspeak-alsaplayer-forward-10-seconds)
        ("i" emacspeak-alsaplayer-info)
        ("," emacspeak-alsaplayer-backward-10-seconds)
        (">" emacspeak-alsaplayer-forward-minute)
        ("<" emacspeak-alsaplayer-backward-minute)
        ("]" emacspeak-alsaplayer-forward-ten-minutes)
        ("[" emacspeak-alsaplayer-backward-ten-minutes)

        ("a"
         emacspeak-alsaplayer-add-to-queue)
        ("A"
         emacspeak-alsaplayer-replace-queue)
        ("c"
         emacspeak-alsaplayer-clear)
        ("g"
         emacspeak-alsaplayer-seek)
        ("j" emacspeak-alsaplayer-jump)
        ("l" emacspeak-alsaplayer-launch)
        (" "
         emacspeak-alsaplayer-pause)
        ("n"
         emacspeak-alsaplayer-next)
        ("p"
         emacspeak-alsaplayer-previous)
        ("q"
         emacspeak-alsaplayer-quit)
        ("r" emacspeak-alsaplayer-relative)
        ("s"
         emacspeak-alsaplayer-start)
        ("S"
         emacspeak-alsaplayer-stop)
        ("/" emacspeak-alsaplayer-speed)
        ("?"
         emacspeak-alsaplayer-status)
        ("v" emacspeak-alsaplayer-volume)
        ("l" emacspeak-alsaplayer-launch)
        )
      do
      (emacspeak-keymap-update  emacspeak-alsaplayer-mode-map k))

;;}}}
(provide 'emacspeak-alsaplayer)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

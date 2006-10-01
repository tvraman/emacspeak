;;; emacspeak-alsaplayer.el --- Control alsaplayer from Emacs
;;; $Id$
;;; $Author$
;;; Description: Controlling alsaplayer from emacs 
;;; Keywords: Emacspeak, alsaplayer
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

;;; Copyright (c) 1995 -- 2006, T. V. Raman
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
\\{emacspeak-alsaplayer-mode-map}"
  )

;;}}}
;;{{{ launch  emacspeak-alsaplayer

(defcustom emacspeak-alsaplayer-auditory-feedback t
  "Turn this on if you want spoken feedback and auditory icons from alsaplayer."
  :type 'boolean
  :group 'emacspeak-alsaplayer)

(defcustom emacspeak-alsaplayer-program
  "alsaplayer"
  "Alsaplayer executable."
  :type 'string
  :group 'emacspeak-alsaplayer)

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
                    emacspeak-alsaplayer-buffer))  (let ((buffer (get-buffer-create emacspeak-alsaplayer-buffer)))
    (save-excursion
      (set-buffer buffer)
      (emacspeak-alsaplayer-mode)
      (shell-command
       (format "%s -r -i daemon &"
               emacspeak-alsaplayer-program))
       (current-buffer))
    (switch-to-buffer buffer) 
    (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
      (emacspeak-auditory-icon 'open-object)
      (emacspeak-speak-mode-line))))

;;}}}
;;{{{  Invoke commands:

(defun emacspeak-alsaplayer-send-command(command &optional
  watch-pattern no-refresh)
  "Send command to Alsaplayer.
Optional second arg watch-pattern specifies line of output to
  focus on.
Optional third arg no-refresh is used to avoid getting status
  twice."
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
             (eq (current-buffer) emacspeak-alsaplayer-buffer))
    (goto-char (point-min))
    (search-forward watch-pattern nil t)))
         
(defun emacspeak-alsaplayer-add-to-queue (resource)
  "Add specified resource to queue."
  (interactive
   (list
    (read-file-name "Media Resource: "
                    (if 
                        (string-match (format ".*%s.*"
                                              emacspeak-alsaplayer-media-directory)
                                      (expand-file-name default-directory))
                        default-directory
                      emacspeak-alsaplayer-media-directory))))
  (emacspeak-alsaplayer-send-command
   (format "--enqueue %s"
           (if (file-directory-p resource)
               (format "%s/*.mp3" resource)
             resource))
   "playlist")
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
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
               (format "%s/*.mp3" resource)
             resource))
           "playlist")
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'select-object)))

(defun emacspeak-alsaplayer-status ()
  "Show alsaplayer status"
  (interactive)
  (emacspeak-alsaplayer-send-command "--status"
                                     "position:"
                                     'no-refresh)
    (when (interactive-p)
      (emacspeak-speak-line)
      (when  emacspeak-alsaplayer-auditory-feedback
        (emacspeak-auditory-icon 'open-object))))

(defun emacspeak-alsaplayer-pause ()
  "Pause or resume alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--pause"
                                     "position:")
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'button)))

(defun emacspeak-alsaplayer-next ()
  "Next  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--next")
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'select-object)))

(defun emacspeak-alsaplayer-previous ()
  "Previous  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--previous")
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'select-object)))
    
(defun emacspeak-alsaplayer-start ()
  "Start  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--start")
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'open-object)))

(defun emacspeak-alsaplayer-stop ()
  "Stop  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command list "--stop")
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'close-object)))

(defun emacspeak-alsaplayer-relative (offset)
  "Relative seek  alsaplayer"
  (interactive "sOffset")
  (emacspeak-alsaplayer-send-command
   (format  "--relative %s" offset))
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'large-movement)))

(defun emacspeak-alsaplayer-speed (setting)
  "Set speed in alsaplayer."
  (interactive "sSpeed")
  (emacspeak-alsaplayer-send-command
   (format "--speed %s" setting))
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'select-object)))

(defun emacspeak-alsaplayer-volume (setting)
  "Set volume."
  (interactive "sVolume")
  (emacspeak-alsaplayer-send-command
   (format "--volume" setting))
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'select-object)))

(defun emacspeak-alsaplayer-seek (offset)
  "Absolute seek  alsaplayer"
  (interactive "sPosition")
  (emacspeak-alsaplayer-send-command
   (format "--seek %s" offset))
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'large-movement)))

(defun emacspeak-alsaplayer-jump (track)
  "Jump to specified track."
  (interactive "sTrack Number:")
  (emacspeak-alsaplayer-send-command
   (format "--jump %s" track))
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'large-movement)))

(defun emacspeak-alsaplayer-clear ()
  "Clear or resume alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command "--clear")
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
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

(defun emacspeak-alsaplayer-forward-second ( seconds)
  "Skip forward by  seconds."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
    (format "--relative %s"
            (or seconds 1))))

(defun emacspeak-alsaplayer-backward-second ( seconds)
  "Skip backward by  seconds."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (format
    "--relative -%s"
            (or seconds 1))))

(defun emacspeak-alsaplayer-forward-minute ( minutes)
  "Skip forward by  minutes."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (format
    "--relative %s"
            (* 60 (or minutes 1)))))

(defun emacspeak-alsaplayer-backward-minute ( minutes)
  "Skip backwards by  minutes."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (format
    "--relative -%s"
            (* 60 (or minutes 1)))))

(defun emacspeak-alsaplayer-forward-ten-minutes ( minutes)
  "Skip forward by  chunks of ten minutes."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (format
    "--relative %s"
            (* 600 (or minutes 1)))))

(defun emacspeak-alsaplayer-backward-ten-minutes ( minutes)
  "Skip backwards by  chunks of minutes."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (format
    "--relative -%s"
            (* 600 (or minutes 1)))))

;;}}}
;;{{{ bind keys

(declaim (special emacspeak-alsaplayer-mode-map))

(loop for k in
      '(
("." emacspeak-alsaplayer-forward-second)
("," emacspeak-alsaplayer-backward-second)
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
("l"
  emacspeak-alsaplayer-launch)
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

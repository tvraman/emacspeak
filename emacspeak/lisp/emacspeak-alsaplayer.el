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

;;; Copyright (c) 1995 -- 2004, T. V. Raman
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
;;; This is work in progress:
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
(make-variable-buffer-local 'emacspeak-alsaplayer-process)

(defvar emacspeak-alsaplayer-session 0
  "Alsaplayer session name associated with this buffer.")
(make-variable-buffer-local 'emacspeak-alsaplayer-session)
;;;###autoload

(define-prefix-command 'emacspeak-alsaplayer-prefix-command
  'emacspeak-alsaplayer-mode-map)

(define-derived-mode emacspeak-alsaplayer-mode fundamental-mode 
  "Alsaplayer Interaction"
  "Major mode for alsaplayer interaction. \n\n
\\{emacspeak-alsaplayer-mode-map}"
  )

(declaim (special emacspeak-alsaplayer-mode-map))

;;}}}
;;{{{ launch  emacspeak-alsaplayer

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

(defvar emacspeak-alsaplayer-buffer-name "alsaplayer"
  "Name of alsaplayer buffer.")
(defsubst emacspeak-alsaplayer-get-session-id ()
  "Return session id from alsaplayer output."
  (substring
   (second
    (split-string
     (buffer-string)))
   (+ 1 
      (length emacspeak-alsaplayer-program)
      1)
   -1))

;;;###autoload
(defun emacspeak-alsaplayer-launch ()
  "Launch Alsaplayer.
user is placed in a buffer associated with the newly created
Alsaplayer session."
  (interactive)
  (declare (special emacspeak-alsaplayer-buffer-name
                    emacspeak-alsaplayer-session))
  (let ((process-connection-type t)
        (process nil)
        (buffer (get-buffer-create emacspeak-alsaplayer-buffer-name)))
    (save-excursion
      (set-buffer buffer)
      (emacspeak-alsaplayer-mode)
      (setq process
            (start-process
             "alsaplayer"
             emacspeak-alsaplayer-buffer-name
             emacspeak-alsaplayer-program
             "-i" "daemon"))
      (accept-process-output process)
      (setq emacspeak-alsaplayer-session
            (emacspeak-alsaplayer-get-session-id))
      (erase-buffer)
      (setq process
            (start-process
             "alsaplayer" emacspeak-alsaplayer-buffer-name emacspeak-alsaplayer-program
             "-n"
             (format "%s" emacspeak-alsaplayer-session)
             "--status")))
    (switch-to-buffer emacspeak-alsaplayer-buffer-name)))

;;}}}
;;{{{  Invoke commands:

(defun emacspeak-alsaplayer-send-command(command-list)
  "Send command to Alsaplayer."
  (declare (special emacspeak-alsaplayer-session))
  (let ((process nil))
    (setq process
          (apply 'start-process
                 "alsaplayer" nil emacspeak-alsaplayer-program
                 "-n"
                 (format "%s" emacspeak-alsaplayer-session)
                 command-list))
    (when (eq major-mode 'emacspeak-alsaplayer-mode)
      (erase-buffer)
      (setq process
            (start-process
             "alsaplayer" emacspeak-alsaplayer-buffer-name emacspeak-alsaplayer-program
             "-n"
             (format "%s" emacspeak-alsaplayer-session)
             "--status")))))

(defun emacspeak-alsaplayer-add-to-queue (resource)
  "Add specified resource to queue."
  (interactive
   (list
    (read-file-name "MP3 Resource: "
                    emacspeak-alsaplayer-media-directory
                    (when (eq major-mode 'dired-mode)
		      (dired-get-filename)))))
  (emacspeak-alsaplayer-send-command
   (cond
    ((file-directory-p resource)
     (nconc
      (list "--enqueue")
      (directory-files
       (expand-file-name resource)
       'full
       "mp3$")))
    (t
     (list "--enqueue"
           (expand-file-name resource))))))

(defun emacspeak-alsaplayer-replace-queue (resource)
  "Add specified resource to queue."
  (interactive
   (list
    (read-file-name "MP3 Resource: "
                    emacspeak-alsaplayer-media-directory
                    (when (eq major-mode 'dired-mode)
		      (dired-get-filename)))))
  (emacspeak-alsaplayer-send-command
   (cond
    ((file-directory-p resource)
     (nconc
      (list "--replace")
      (directory-files
       (expand-file-name resource)
       'full
       "mp3$")))
    (t
     (list "--replace"
           (expand-file-name resource))))))

(defun emacspeak-alsaplayer-status ()
  "Show alsaplayer status"
  (interactive)
  (emacspeak-alsaplayer-send-command
   (list "--status")))

    
(defun emacspeak-alsaplayer-pause ()
  "Pause or resume alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command
   (list "--pause")))

(defun emacspeak-alsaplayer-next ()
  "Next  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command
   (list "--next")))

(defun emacspeak-alsaplayer-previous ()
  "Previous  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command
   (list "--prev")))
    
(defun emacspeak-alsaplayer-start ()
  "Start  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command
   (list "--start")))

(defun emacspeak-alsaplayer-stop ()
  "Stop  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command
   (list "--stop")))

(defun emacspeak-alsaplayer-relative (offset)
  "Relative seek  alsaplayer"
  (interactive "sOffset")
  (emacspeak-alsaplayer-send-command
   (list "--relative"
         offset)))

(defun emacspeak-alsaplayer-speed (setting)
  "Set speed in alsaplayer."
  (interactive "sSpeed")
  (emacspeak-alsaplayer-send-command
   (list "--speed"
         setting)))

(defun emacspeak-alsaplayer-volume (setting)
  "Set volume."
  (interactive "sVolume")
  (emacspeak-alsaplayer-send-command
   (list "--volume"
         setting)))

(defun emacspeak-alsaplayer-seek (offset)
  "Absolute seek  alsaplayer"
  (interactive "sOffset")
  (emacspeak-alsaplayer-send-command
   (list "--seek"
         offset)))

(defun emacspeak-alsaplayer-jump (track)
  "Jump to specified track."
  (interactive "sTrack Number:")
  (emacspeak-alsaplayer-send-command
   (list "--jump"
         track)))
(defun emacspeak-alsaplayer-clear ()
  "Clear or resume alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command
   (list "--clear")))

(defun emacspeak-alsaplayer-quit ()
  "Quit or resume alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command
   (list "--quit")))

;;}}}
(provide 'emacspeak-alsaplayer)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

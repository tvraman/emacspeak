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

(defvar emacspeak-alsaplayer-session "alsaplayer-0"
  "Alsaplayer session name associated with this buffer.")
(make-variable-buffer-local 'emacspeak-alsaplayer-session)

(defvar emacspeak-alsaplayer-session-id "0"
  "Alsaplayer session id associated with this buffer.")
(make-variable-buffer-local 'emacspeak-alsaplayer-session-id)
 
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
  "Turn this on if you want spoken feedback and auditory icons
from alsaplayer."
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



(defun emacspeak-alsaplayer-get-session ()
  "Return session id from alsaplayer output."
  (substring
   (second
    (split-string
     (buffer-string)))
   1
   -1))

;;;###autoload
(defun emacspeak-alsaplayer-launch ()
  "Launch Alsaplayer.
user is placed in a buffer associated with the newly created
Alsaplayer session."
  (interactive)
  (declare (special emacspeak-alsaplayer-session
                    emacspeak-alsaplayer-session-id))
  (let ((process-connection-type t)
        (process nil)
        (buffer (get-buffer-create "alsaplayer")))
    (save-excursion
      (set-buffer buffer)
      (emacspeak-alsaplayer-mode)
      (setq process
            (start-process
             "alsaplayer"
             (current-buffer)
             emacspeak-alsaplayer-program
             "-i" "daemon" ))
      (accept-process-output process)
      (setq emacspeak-alsaplayer-session
            (emacspeak-alsaplayer-get-session))
      (put 'emacspeak-alsaplayer-session 'buffer (current-buffer))
      (setq emacspeak-alsaplayer-session-id
             (second
             (split-string emacspeak-alsaplayer-session "-")))
      (erase-buffer)
      (setq process
            (start-process
             "alsaplayer" (current-buffer) emacspeak-alsaplayer-program
             "-n"
             (or emacspeak-alsaplayer-session-id
                 "0")
             "--status")))
    (switch-to-buffer buffer)
    (rename-buffer emacspeak-alsaplayer-session 'unique))
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{  Invoke commands:

(defun emacspeak-alsaplayer-send-command(command-list &optional no-refresh)
  "Send command to Alsaplayer.
Optional second arg no-refresh is used to avoid getting status twice."
  (declare (special emacspeak-alsaplayer-session))
  (save-excursion
    (set-buffer (get 'emacspeak-alsaplayer-session 'buffer))
    (erase-buffer)
  (let ((process nil))
    (setq process
          (apply 'start-process
                 "alsaplayer"
                 (current-buffer) emacspeak-alsaplayer-program
                 "-n" emacspeak-alsaplayer-session-id
                 command-list))
    (unless no-refresh
    (setq process
          (start-process
                 "alsaplayer" (current-buffer)   emacspeak-alsaplayer-program
                 "-n" emacspeak-alsaplayer-session-id
                 "--status"))))))

(defun emacspeak-alsaplayer-add-to-queue (resource)
  "Add specified resource to queue."
  (interactive
   (list
    (read-file-name "Media Resource: "
                    emacspeak-alsaplayer-media-directory)))
  (emacspeak-alsaplayer-send-command
   (cond
    ((file-directory-p resource)
     (nconc
      (list "-e")
      (directory-files
       (expand-file-name resource)
       'full
       "\\(mp3\\)\\|\\(ogg\\)$")))
    (t
     (list "-e"
           (expand-file-name resource)))))
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'select-object)))

(defun emacspeak-alsaplayer-replace-queue (resource)
  "Add specified resource to queue."
  (interactive
   (list
    (read-file-name "MP3 Resource: "
                    emacspeak-alsaplayer-media-directory)))
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
           (expand-file-name resource)))))
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'select-object)))

(defun emacspeak-alsaplayer-status ()
  "Show alsaplayer status"
  (interactive)
  (emacspeak-alsaplayer-send-command
   (list "--status")
   'no-refresh)
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'open-object)))

    
(defun emacspeak-alsaplayer-pause ()
  "Pause or resume alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command
   (list "--pause"))
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'button)))

(defun emacspeak-alsaplayer-next ()
  "Next  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command
   (list "--next"))
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'select-object)))

(defun emacspeak-alsaplayer-previous ()
  "Previous  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command
   (list "--prev"))
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'select-object)))

    
(defun emacspeak-alsaplayer-start ()
  "Start  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command
   (list "--start"))
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'open-object)))

(defun emacspeak-alsaplayer-stop ()
  "Stop  alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command
   (list "--stop"))
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'close-object)))

(defun emacspeak-alsaplayer-relative (offset)
  "Relative seek  alsaplayer"
  (interactive "sOffset")
  (emacspeak-alsaplayer-send-command
   (list "--relative"
         offset))
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'large-movement)))

(defun emacspeak-alsaplayer-speed (setting)
  "Set speed in alsaplayer."
  (interactive "sSpeed")
  (emacspeak-alsaplayer-send-command
   (list "--speed"
         setting))
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'select-object)))

(defun emacspeak-alsaplayer-volume (setting)
  "Set volume."
  (interactive "sVolume")
  (emacspeak-alsaplayer-send-command
   (list "--volume"
         setting))
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'select-object)))

(defun emacspeak-alsaplayer-seek (offset)
  "Absolute seek  alsaplayer"
  (interactive "sOffset")
  (emacspeak-alsaplayer-send-command
   (list "--seek"
         offset))
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'large-movement)))

(defun emacspeak-alsaplayer-jump (track)
  "Jump to specified track."
  (interactive "sTrack Number:")
  (emacspeak-alsaplayer-send-command
   (list "--jump"
         track))
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'large-movement)))

(defun emacspeak-alsaplayer-clear ()
  "Clear or resume alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command
   (list "--clear"))
  (when (and emacspeak-alsaplayer-auditory-feedback (interactive-p))
    (emacspeak-auditory-icon 'delete-object)))

(defun emacspeak-alsaplayer-quit ()
  "Quit or resume alsaplayer"
  (interactive)
  (emacspeak-alsaplayer-send-command
   (list "--quit"))
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
   (list
    "--relative"
    (format "%s"
            (or seconds 1)))))

(defun emacspeak-alsaplayer-backward-second ( seconds)
  "Skip backward by  seconds."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (list
    "--relative"
    (format "-%s"
            (or seconds 1)))))

(defun emacspeak-alsaplayer-forward-minute ( minutes)
  "Skip forward by  minutes."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (list
    "--relative"
    (format "%s"
            (* 60 (or minutes 1))))))

(defun emacspeak-alsaplayer-backward-minute ( minutes)
  "Skip backwards by  minutes."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (list
    "--relative"
    (format "-%s"
            (* 60 (or minutes 1))))))


(defun emacspeak-alsaplayer-forward-ten-minutes ( minutes)
  "Skip forward by  chunks of ten minutes."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (list
    "--relative"
    (format "%s"
            (* 600 (or minutes 1))))))

(defun emacspeak-alsaplayer-backward-ten-minutes ( minutes)
  "Skip backwards by  chunks of minutes."
  (interactive "p")
  (emacspeak-alsaplayer-send-command
   (list
    "--relative"
    (format "-%s"
            (* 600 (or minutes 1))))))



(define-key emacspeak-alsaplayer-mode-map "."
  'emacspeak-alsaplayer-forward-second)
(define-key emacspeak-alsaplayer-mode-map ","
  'emacspeak-alsaplayer-backward-second)
(define-key emacspeak-alsaplayer-mode-map ">"
  'emacspeak-alsaplayer-forward-minute)
(define-key emacspeak-alsaplayer-mode-map "<"
  'emacspeak-alsaplayer-backward-minute)
(define-key emacspeak-alsaplayer-mode-map "]"
  'emacspeak-alsaplayer-forward-ten-minutes)
(define-key emacspeak-alsaplayer-mode-map "["
  'emacspeak-alsaplayer-backward-ten-minutes)

;;}}}
;;{{{ bind keys

(declaim (special emacspeak-alsaplayer-mode-map))
(define-key emacspeak-alsaplayer-mode-map "a"
  'emacspeak-alsaplayer-add-to-queue)
(define-key emacspeak-alsaplayer-mode-map "A"
  'emacspeak-alsaplayer-replace-queue)
(define-key emacspeak-alsaplayer-mode-map "c"
  'emacspeak-alsaplayer-clear)
(define-key emacspeak-alsaplayer-mode-map "g"
  'emacspeak-alsaplayer-seek)
(define-key emacspeak-alsaplayer-mode-map "j" 'emacspeak-alsaplayer-jump)
(define-key emacspeak-alsaplayer-mode-map "l"
  'emacspeak-alsaplayer-launch)
(define-key emacspeak-alsaplayer-mode-map " "
  'emacspeak-alsaplayer-pause)
(define-key emacspeak-alsaplayer-mode-map "n"
  'emacspeak-alsaplayer-next)
(define-key emacspeak-alsaplayer-mode-map "p"
  'emacspeak-alsaplayer-previous)
(define-key emacspeak-alsaplayer-mode-map "q"
  'emacspeak-alsaplayer-quit)
(define-key emacspeak-alsaplayer-mode-map "r" 'emacspeak-alsaplayer-relative)
(define-key emacspeak-alsaplayer-mode-map "s"
  'emacspeak-alsaplayer-start)
(define-key emacspeak-alsaplayer-mode-map "S"
  'emacspeak-alsaplayer-stop)
(define-key emacspeak-alsaplayer-mode-map "/" 'emacspeak-alsaplayer-speed)
(define-key emacspeak-alsaplayer-mode-map "?"
  'emacspeak-alsaplayer-status)
(define-key emacspeak-alsaplayer-mode-map "v" 'emacspeak-alsaplayer-volume)

;;}}}
(provide 'emacspeak-alsaplayer)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

;;; amixer.el --- Set Audio Volume from Emacs  -*- lexical-binding: t; -*-
;;
;; Emacs front-end to AMixer
;;{{{  Copyright:

;; Copyright (C) 1995 -- 2022, T. V. Raman<tv.raman.tv@gmail.com>
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

;;}}}
;;{{{ introduction

;;; Commentary:
;; Provide an emacs front-end to amixer,
;; the sound mixer in ALSA that is used to configure the audio device.
;;
;; The main entry point is command @code{emacspeak-audio-setup} bound
;; to @kbd{C-e)}. When called for the first time, this command
;; builds up a database of available controls on the default audio
;; device. These control names are then available for completion in
;; the minibuffer. Pick a desired control, e.g., "master playback
;; volume", and this displays a prompt with the current value. Enter
;; the new value and press <RETURN>. To reset all controls to their
;; default values, Press @kbd{C-j}.

;;; Code:

;;}}}
;;{{{ required packages

(require 'cl-lib)
(require 'emacspeak-preamble)
(cl-declaim  (optimize  (safety 0) (speed 3)))

;;}}}
;;{{{ Decls:

;; forward decl:
(defvar emacspeak-speak-messages)

;;}}}
;;{{{ Custom:

(defcustom amixer-device "default"
  "ALSA Control Device."
  :type 'string
  :group 'amixer)

(defvar amixer-program  (executable-find "amixer")
  "Amixer program")

(defvar alsactl-program  (executable-find "alsactl")
  "AlsaCtl program")

(defvar amixer-db nil
  "Holds cached values.")

(cl-defstruct amixer-control
  numid iface name setting)

(cl-defstruct amixer-control-setting
  type access values
  min max step
  current)

;;}}}
;;{{{ Manage amixer db:

(defun amixer-populate-settings (control)
  "Populate control with its settings information."
  (cl-declare (special  amixer-device))
  (let ((fields nil)
        (emacspeak-speak-messages nil)
        (slots nil)
        (current nil))
    (with-temp-buffer
      (shell-command
       (format "amixer --device %s cget numid=%s"
               amixer-device
               (amixer-control-numid (cdr control)))
       (current-buffer))
      (goto-char (point-min))
      (forward-line 1)
      (setq fields
            (split-string
             (buffer-substring-no-properties
              (1+ (line-beginning-position))
              (line-end-position))
             ","))
      (setq slots
            (cl-loop for f in fields
                     collect
                     (cl-second (split-string f "="))))
      (while (and (not (eobp))
                  (looking-at "^ *;"))
        (forward-line 1))
      (setq current
            (cl-second
             (split-string
              (buffer-substring-no-properties
               (line-beginning-position)
               (line-end-position))
              "=")))
      (setf (amixer-control-setting (cdr control))
            (make-amixer-control-setting
             :type (nth 0 slots)
             :access (nth 1 slots)
             :values (nth 2 slots)
             :min (nth 3 slots)
             :max (nth 4 slots)
             :step (nth 5 slots)
             :current current))))
  control)

(defun amixer-build-db ()
  "Create a database of amixer controls and their settings."
  (cl-declare (special amixer-db amixer-device amixer-program))
  (unless amixer-program (error "You dont have a standard amixer."))
  (let (
        (message-log-max nil)
        (controls nil)
        (fields nil)
        (slots nil)
        (emacspeak-speak-messages nil))
    (with-temp-buffer
      (shell-command
       (format
        "amixer --device %s controls | sed -e s/\\'//g"
        amixer-device)
       (current-buffer))
      (goto-char (point-min))
      (while (not (eobp))
        (setq fields
              (split-string
               (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))
               ","))
        ;; only need 3 fields:
        (setq fields
              (list
               (nth 0 fields)
               (nth 1 fields)
               (mapconcat #'identity (nthcdr 2 fields) " ")))
        (setq slots
              (cl-loop for f in fields
                       collect
                       (cl-second (split-string f "="))))
        (push
         (cons
          (cl-third slots)
          (make-amixer-control
           :numid (cl-first slots)
           :iface (cl-second slots)
           :name (cl-third slots)))
         controls)
        (forward-line 1))               ; done collecting controls
      (mapc #'amixer-populate-settings controls)
      (setq amixer-db controls))))

;;}}}
;;{{{ Amixer:

(defun amixer-get-enumerated-values(control)
  "Return list of enumerated values."
  (cl-declare (special amixer-device))
  (let ((values nil)
        (emacspeak-speak-messages nil))
    (with-temp-buffer
      (shell-command
       (format
        "amixer -devicec %s   cget numid=%s | grep Item | sed -e s/\\'//g"
        amixer-device
        (amixer-control-numid control))
       (current-buffer))
      (goto-char (point-min))
      (while (not   (eobp))
        (beginning-of-line)
        (when (looking-at "^ *;")
          (search-forward "Item #" nil t)
          (push
           (buffer-substring-no-properties
            (point)
            (line-end-position))
           values))
        (forward-line 1))
      (nreverse values))))

(defvar amixer-alsactl-config-file
  (cond
   ((file-exists-p (expand-file-name "asound.state"
                                     user-emacs-directory))
    (expand-file-name "asound.state" user-emacs-directory))
   ((file-exists-p "/var/lib/alsa/asound.state")
    "/var/lib/alsa/asound.state"))
  "Personal sound card settings. Copied from /var/lib/alsa/asound.state
to  ~/.emacs.d ")

(defun amixer-alsactl-setup ()
  "Set up alsactl sound state."
  (cl-declare (special amixer-alsactl-config-file))
  (setq
   amixer-alsactl-config-file
   (let ((sys-alsa "/var/lib/alsa/asound.state")
         (f (expand-file-name "asound.state" user-emacs-directory)))
     (unless (file-exists-p sys-alsa)
       (error "Alsa not setup correctly.")
       (message "Perhaps run \"sudo alsactl store the first time.\""))
     (unless  (file-exists-p f) (copy-file sys-alsa user-emacs-directory))
     f)))

;;;###autoload
(defun amixer-restore (&optional conf-file)
  "Reset Alsa."
  (cl-declare (special alsactl-program))
  (if conf-file
      (start-process
       "AlsaCtl" nil alsactl-program
       "-f" conf-file
       "restore")
    (start-process
     "AlsaCtl" nil alsactl-program
     "restore"))
  (dtk-stop)
  (message "Resetting  sound to default")
  (amixer-build-db))

;;;###autoload
(defun amixer (&optional refresh)
  "ALSA settings.
Interactive prefix arg refreshes cache."
  (interactive "P")
  (cl-declare (special amixer-db amixer-alsactl-config-file amixer-program))
  (unless amixer-alsactl-config-file (amixer-alsactl-setup))
  (when (or refresh (null amixer-db))
    (amixer-build-db))
  (let ((control
         (cdr
          (assoc
           (let ((completion-ignore-case t))
             (completing-read
              "Control:" amixer-db
              nil 'must-match))
           amixer-db)))
        (update nil)
        (choices nil))
    (cond
     ((null control)
      (amixer-restore  amixer-alsactl-config-file))
     (t
      (when (string=
             "ENUMERATED"
             (amixer-control-setting-type (amixer-control-setting control)))
        (setq choices
              (amixer-get-enumerated-values control)))
      (setq update
            (read-from-minibuffer
             (format
              "Change %s from %s %s:"
              (amixer-control-name control)
              (amixer-control-setting-current
               (amixer-control-setting control))
              (or choices ""))))
      (setf
       (amixer-control-setting-current
        (amixer-control-setting control))
       update)
      (start-process
       "AMixer" "*Debug*"  amixer-program
       "--device" amixer-device
       "cset"
       (format "numid=%s" (amixer-control-numid control))
       update)
      (message
       "updated %s to %s"
       (amixer-control-name control)
       update)))))

;;;###autoload
(defun amixer-store()
  "Persist  amixer."
  (interactive)
  (cl-declare (special  amixer-alsactl-config-file alsactl-program))
  (unless amixer-alsactl-config-file (amixer-alsactl-setup))
  (when amixer-alsactl-config-file
    (start-process
     "AlsaCtl" nil alsactl-program
     "-f"amixer-alsactl-config-file
     "store")
    (emacspeak-auditory-icon 'task-done)
    (message "Persisted amixer state to %s."
             amixer-alsactl-config-file)))

;;}}}
;;{{{Raise/Lower Volume Using pactl:
(defcustom amixer-volume-step 2
  "Step-size for volume change."
  :type 'integer
  :group 'emacspeak)

;;;###autoload
(defun amixer-volume-up (prompt)
  "Raise Master volume by amixer-volume-step.
Interactive prefix arg `PROMPT' reads percentage as a number"
  (interactive "P")
  (cl-declare (special amixer-volume-step))
  (let ((emacspeak-speak-messages nil)
        (inhibit-message t))
    (shell-command
     (format "%s set 'Master'+ %d%%"
             amixer-program
             (if prompt
                 (read-number "Volume Step:")
               amixer-volume-step)))
    (emacspeak-auditory-icon 'right)))

;;;###autoload
(defun amixer-volume-down (prompt)
  "Lower Master volume by amixer-volume-step.
Interactive prefix arg `PROMPT' reads percentage as a number"
  (interactive "P")
  (cl-declare (special amixer-volume-step))
  (let ((emacspeak-speak-messages nil)
        (inhibit-message t))
    (shell-command
     (format "%s set 'Master' -%d%%-"
             amixer-program
             (if prompt
                 (read-number "Volume Step:")
               amixer-volume-step)))
    (emacspeak-auditory-icon 'left)))

;;}}}
(provide 'amixer)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}

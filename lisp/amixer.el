;;; amixer.el --- Control AMixer from Emacs  -*- lexical-binding: t; -*-
;;;$Id$
;;;Emacs front-end to AMixer
;;{{{  Copyright:

;;; Copyright (C) 1995 -- 2015, T. V. Raman<raman@cs.cornell.edu>
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
;;{{{ introduction

;;; Commentary:
;;; Provide an emacs front-end to amixer.
;;;amixer is part of ALSA.

;;; Code:

;;}}}
;;{{{ required packages

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))

;;}}}
;;{{{ Customizations:

;;}}}
;;{{{ Definitions

(defcustom amixer-device "default"
  "ALSA Control Device."
  :type 'string
  :group 'amixer)

(defvar amixer-program  (executable-find "amixer")
  "Amixer program")


(defvar amixer-db nil
  "Holds cached values.")

(cl-defstruct amixer-control
  numid iface name setting)

(declare-function amixer-control-name  "amixer.el" (amixer))
(declare-function amixer-control-numid  "amixer.el" (amixer))
(declare-function amixer-control-iface  "amixer.el" (amixer))
(cl-defstruct amixer-control-setting
  type access values
  min max step
  current)

;;}}}
;;{{{ Manage amixer db:

(defun amixer-populate-settings (control)
  "Populate control with its settings information."
  (declare (special amixer-card amixer-device))
  (let ((fields nil)
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
                     (second (split-string f "="))))
      (while (and (not (eobp))
                  (looking-at "^ *;"))
        (forward-line 1))
      (setq current
            (second
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
  (declare (special amixer-db amixer-device))
  (unless amixer-program
    (error "You dont have a standard amixer."))
  (let ((controls nil)
        (fields nil)
        (slots nil))
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
;;; only need 3 fields:
        (setq fields
              (list
               (nth 0 fields)
               (nth 1 fields)
               (mapconcat #'identity (nthcdr 2 fields) " ")))
        (setq slots
              (cl-loop for f in fields
                       collect
                       (second (split-string f "="))))
        (push
         (cons
          (third slots)
          (make-amixer-control
           :numid (first slots)
           :iface (second slots)
           :name (third slots)))
         controls)
        (forward-line 1))               ; done collecting controls
      (mapc #'amixer-populate-settings controls)
      (setq amixer-db controls))))

;;}}}
;;{{{ Amixer:

(defun amixer-get-enumerated-values(control)
  "Return list of enumerated values."
  (declare (special amixer-device))
  (let ((values nil))
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
  nil
  "Personal sound card settings. Copied from /var/lib/alsa/asound.state
to your ~/.emacs.d to avoid needing to run alsactl as root on first
use."
  )

(defun amixer-alsactl-setup ()
  "Set up alsactl sound state."
  (declare (special amixer-alsactl-config-file))
  (setq
   amixer-alsactl-config-file
   (let ((sys-alsa "/var/lib/alsa/asound.state")
         (f (expand-file-name "asound.state" user-emacs-directory)))
     (unless (file-exists-p sys-alsa) (error "Alsa not setup correctly."))
     (unless  (file-exists-p f) (copy-file sys-alsa user-emacs-directory))
     f)))

;;;###autoload
(defun amixer-restore (&optional conf-file)
  "Restore alsa settings."
  (if conf-file
      (start-process
       "AlsaCtl" nil "alsactl"
       "-f" conf-file
       "restore")
    (start-process
     "AlsaCtl" nil "alsactl"
     "restore"))
  (dtk-stop)
  (message "Resetting  sound to default")
  (amixer-build-db))

;;;###autoload
(defun amixer (&optional refresh)
  "Interactively manipulate ALSA settings.
Interactive prefix arg refreshes cache."
  (interactive "P")
  (declare (special amixer-db amixer-alsactl-config-file))
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
      (amixer-restore  amixer-alsactl-config-file)
      (amixer-reset-equalizer))
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
(defun amixer-equalize()
  "Set equalizer. Only affects device `equal'."
  (interactive)
  (declare (special amixer-device))
  (let ((amixer-device "equal"))
    (amixer 'refresh)
    ;;; mark db dirty.
    (setq amixer-db nil)))

(defun amixer-reset-equalizer ()
  "Reset equalizer to default values -- 66% for all 10 bands."
  (interactive)
  (let ((amixer amixer-program))
    (cl-loop
     for  i from 1 to 10 do
     (start-process
      "AMixer" nil amixer
      "-Dequal"
      "cset"
      (format "numid=%s" i)
      "66,66" )))
  (message "Reset equalizer"))

;;;###autoload
(defun amixer-store()
  "Persist current amixer settings."
  (interactive)
  (declare (special  amixer-alsactl-config-file))
  (unless amixer-alsactl-config-file (amixer-alsactl-setup))
  (when amixer-alsactl-config-file
    (start-process
     "AlsaCtl" nil (executable-find "alsactl")
     "-f"amixer-alsactl-config-file
     "store" )
    (emacspeak-auditory-icon 'task-done)
    (message "Persisted amixer state.")))
;;}}}
(provide 'amixer)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}

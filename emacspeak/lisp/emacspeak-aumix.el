;;; emacspeak-aumix.el --- Setting Audio Mixer
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak extension to conveniently set audio display
;;; Keywords: Emacspeak, Audio Desktop
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

;;; Copyright (C) 1995 -- 2004, T. V. Raman<raman@cs.cornell.edu>
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

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-speak)
(require 'emacspeak-forms)
(require 'emacspeak-sounds)
;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; Provides an AUI to setting up the auditory display via AUMIX
;;; This module is presently Linux specific
;;; Code:

;;}}}
;;{{{ Variables

(defvar emacspeak-aumix-program "aumix"
  "Program that sets up the mixer.")

(defvar emacspeak-aumix-channel-table (make-hash-table)
  "Hash table holding mapping from  channel options to descriptions.")

(defsubst emacspeak-aumix-set-channel  (channel description)
  (declare (special emacspeak-aumix-channel-table))
  (setf (gethash channel emacspeak-aumix-channel-table) description))

(defsubst emacspeak-aumix-get-channel (channel)
  (declare (special emacspeak-aumix-channel-table))
  (gethash channel emacspeak-aumix-channel-table))
(emacspeak-aumix-set-channel ?r "reset")
(emacspeak-aumix-set-channel ?e "edit")
(emacspeak-aumix-set-channel ?b "bass")
(emacspeak-aumix-set-channel ?c "CD Audio")
(emacspeak-aumix-set-channel ?i "input gain")
(emacspeak-aumix-set-channel ?l "line")
(emacspeak-aumix-set-channel ?m "microphone")
(emacspeak-aumix-set-channel ?p "pc speaker")
(emacspeak-aumix-set-channel ?s "midi synthesizer")
(emacspeak-aumix-set-channel ?t "treble")
(emacspeak-aumix-set-channel ?w "wave audio")
(emacspeak-aumix-set-channel ?x "mix monitor")
(emacspeak-aumix-set-channel ?1 "1")
(emacspeak-aumix-set-channel ?2 "2")
(emacspeak-aumix-set-channel ?3 "3")
(emacspeak-aumix-set-channel ?o "output gain")
(emacspeak-aumix-set-channel ?v "volume")

;;;###autoload
(defgroup emacspeak-aumix nil
  "Customization group for setting the Emacspeak auditory
display."
  :group 'emacspeak)

(defcustom emacspeak-aumix-multichannel-capable-p nil
  "*Set to T if the sound card is capable of mixing multiple channels of audio."
  :group 'emacspeak-aumix
  :type 'boolean)

(defcustom emacspeak-aumix-full-duplex-p nil
  "*Set to T if the sound card is truly full duplex."
  :group 'emacspeak-aumix
  :type 'boolean)

(defcustom emacspeak-aumix-midi-available-p nil
  "*Set to T if   midi is available."
  :group 'emacspeak-aumix
  :type 'boolean)

(setq emacspeak-aumix-midi-available-p
      (file-exists-p "/usr/bin/stdiosynth"))
;;}}}
;;{{{ emacspeak-aumix

;;;###autoload
(defcustom emacspeak-aumix-settings-file
  (when
      (file-exists-p (expand-file-name ".aumixrc"
                                       emacspeak-resource-directory))
    (expand-file-name ".aumixrc" emacspeak-resource-directory))
  "*Name of file containing personal aumix settings."
  :group 'emacspeak-aumix
  :type 'string)

;;;###autoload
(defcustom emacspeak-aumix-reset-options
  (format 
   "-f %s -L 2>&1 >/dev/null"
   emacspeak-aumix-settings-file)
  "*Option to pass to aumix for resetting to default values."
  :group 'emacspeak-aumix
  :type 'string)
;;;###autoload
(defun emacspeak-aumix-reset ()
  "Reset to default audio settings."
  (interactive)
  (declare (special emacspeak-aumix-program
                    emacspeak-aumix-reset-options))
  (shell-command
   (format "%s %s"
           emacspeak-aumix-program
           emacspeak-aumix-reset-options))
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)))
(eval-when-compile (require 'emacspeak-forms))
(defun emacspeak-aumix-edit ()
  "Edit aumix settings interactively. 
Run command \\[emacspeak-aumix-reset]
after saving the settings to have them take effect."
  (interactive)
  (declare (special emacspeak-etc-directory))
  (let ((emacspeak-speak-messages nil)
        (dtk-stop-immediately nil))
    (emacspeak-forms-find-file
     (expand-file-name "forms/aumix-rc.el" emacspeak-etc-directory))
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-forms-speak-field)))

(defun emacspeak-aumix ()
  "Setup output parameters of the auditory display.
 Launch this tool while you have auditory output on
multiple channels playing so you can
adjust the settings to your preference.  Hit q to quit when
you are done."
  (interactive)
  (declare (special emacspeak-aumix-program))
  (let ((channel nil)
        (description nil)
        (setting nil)
        (done nil))
    (while (not done)
      (setq channel (read-char "Set channel: "))
      (setq description (emacspeak-aumix-get-channel channel))
      (cond
       ((and description
             (string-equal "edit" description))
        (emacspeak-aumix-edit)
        (setq done t))
       ((and description
             (string-equal "reset" description))
        (emacspeak-aumix-reset)
        (when (interactive-p)
          (emacspeak-auditory-icon 'close-object))
        (setq done t))
       (description
        (setq setting
              (read-from-minibuffer
               (format "Set %s to:" description)))
        (shell-command
         (format "%s -%c %s"
                 emacspeak-aumix-program channel setting))
        (emacspeak-auditory-icon 'select-object))
       ((= channel ?q)
        (setq done t)
        (emacspeak-auditory-icon 'close-object))
       (t (message "Invalid channel %c" channel)
          (emacspeak-auditory-icon 'warn-user))))))

;;;###autoload

(defun emacspeak-aumix-wave-increase (&optional gain)
  "Increase volume of wave output. "
  (interactive "P")
  (unless (numberp gain) (setq gain 1 ))
  (let ((emacspeak-speak-messages nil))
    (shell-command
     (format "%s -w +%s"
	     emacspeak-aumix-program  gain))
    (emacspeak-auditory-icon 'select-object) ))

;;;###autoload
(defun emacspeak-aumix-wave-decrease  (&optional gain)
  "Decrease volume of wave output. "
  (interactive "P")
  (unless (numberp gain) (setq gain 1 ))
  (let ((emacspeak-speak-messages nil))
    (shell-command
     (format "%s -w -%s"
	     emacspeak-aumix-program  gain))
    (emacspeak-auditory-icon 'select-object)))

;;;###autoload
(defun emacspeak-aumix-volume-increase (&optional gain)
  "Increase overall volume. "
  (interactive "P")
  (unless (numberp gain) (setq gain 1 ))
  (let ((emacspeak-speak-messages nil))
    (shell-command
     (format "%s -v +%s"
	     emacspeak-aumix-program  gain))
    (emacspeak-auditory-icon 'select-object)))

;;;###autoload
(defun emacspeak-aumix-volume-decrease  (&optional gain)
  "Decrease overall volume. "
  (interactive "P")
  (unless (numberp gain) (setq gain 1 ))
  (let ((emacspeak-speak-messages nil))
    (shell-command
     (format "%s -v -%s"
	     emacspeak-aumix-program  gain))
    (emacspeak-auditory-icon 'select-object)))

;;}}}
(provide 'emacspeak-aumix)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

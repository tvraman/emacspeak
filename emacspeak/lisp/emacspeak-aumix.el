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

;;; Copyright (C) 1999 T. V. Raman <raman@cs.cornell.edu>
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

;;}}}
;;{{{ emacspeak-aumix

(defun emacspeak-aumix ()
  "Setup output parameters of the auditory display.
Luanch this tool while you have auditory output on
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
       (description
        (setq setting
              (read-from-minibuffer
               (format "Set %s to:" description)))
        (shell-command
         (format "%s -%c %s"
                 emacspeak-aumix-program channel setting)))
       ((= channel ?q)
        (setq done t)
        (emacspeak-auditory-icon 'close-object))
       (t (message "Invalid channel %c" channel))))))

;;}}}
(provide 'emacspeak-aumix)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

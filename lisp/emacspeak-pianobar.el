;;; emacspeak-pianobar.el --- Pandora Radio: Speech-enable PIANOBAR
;;; $Id: emacspeak-pianobar.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable PIANOBAR An Emacs Interface to pianobar
;;; Keywords: Emacspeak,  Audio Desktop pianobar
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
;;;Copyright (C) 1995 -- 2007, T. V. Raman
;;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
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
;;; MERCHANTABILITY or FITNPIANOBAR FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction
;;; pianobar git://github.com/PromyLOPh/pianobar.git
;;; Is a stand-alone client for Pandora Radio.
;;; pianobar.el available on the Emacs Wiki at
;;; http://www.emacswiki.org/emacs/pianobar.el
;;; Provides access to Pandora Radio via pianobar from the comfort of Emacs.
;;; This module speech-enables Pianobar and enhances it for the Complete Audio Desktop.
;;; Commentary:
;;; PIANOBAR == 

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Advice Interactive Commands:

(defadvice pianobar (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'open-object)))

;;; Advice all actions to play a pre-auditory icon

(loop for  f in
      '(pianobar-pause-song pianobar-love-current-song
pianobar-ban-current-song pianobar-bookmark-song
pianobar-create-station pianobar-delete-current-station
pianobar-explain-song
pianobar-add-shared-station pianobar-song-history
pianobar-currently-playing pianobar-add-shared-station
pianobar-move-song-different-station pianobar-next-song
 pianobar-rename-current-station
pianobar-change-station
pianobar-tired-of-song
pianobar-upcoming-songs
pianobar-select-quickmix-stations pianobar-next-song)
      do
      (eval
       `(defadvice ,f (before emacspeak pre act comp)
          "Play auditory icon."
          (when (interactive-p)
            (emacspeak-auditory-icon 'select-object)))))
(defadvice pianobar-window-toggle (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (let ((state (get-buffer-window pianobar-buffer)))
      (cond
       (state
        (emacspeak-auditory-icon'open-object)
        (dtk-speak "Displayed pianobar"))
       (t
        (emacspeak-auditory-icon'close-object)
        (dtk-speak "Hid Pianobar "))))))


(defadvice pianobar-quit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)))


;;}}}
;;{{{ Simplified PianoBar interaction:

;;{{{ Customizations And Variables

(defgroup emacspeak-pianobar nil
  "Control Pandora CLI client Pianobar from Emacs.")


(defcustom emacspeak-pianobar-program
  (executable-find "pianobar")
  "Pianobar --- Command Line Pandora player."
  :group 'emacspeak-pianobar)



(defvar emacspeak-pianobar-buffer "*PianoBar*")

;;}}}
;;{{{ define a derived mode for m-player interaction 


(defvar emacspeak-pianobar-process nil
  "Process handle to pianobar." )

(defsubst emacspeak-pianobar-dispatch (command)
  "Dispatch command to pianobar."
  (declare (special emacspeak-pianobar-process))
  (with-current-buffer emacspeak-pianobar-buffer
    (erase-buffer)
    (process-send-string
     emacspeak-pianobar-process
     (format "%s\n" command))
    (accept-process-output emacspeak-pianobar-process 0.1)
    (unless (zerop (buffer-size))
      (buffer-substring-no-properties (point-min) (1-  (point-max))))))

(define-derived-mode emacspeak-pianobar-mode comint-mode 
  "Pianobar Interaction"
  "Major mode for pianobar interaction. \n\n
\\{emacspeak-pianobar-mode-map}"
  (progn
    (setq buffer-undo-list t)
    (ansi-color-for-comint-mode-on)
    (setq emacspeak-pianobar-process (get-buffer-process (current-buffer)))))

;;}}}
;;{{{ emacspeak-pianobar

;;;###autoload
(defun emacspeak-pianobar  ()
  "Start or control Emacspeak Pianobar player."
  (interactive )
  (declare (special emacspeak-pianobar-process))
  (cond
   ((and emacspeak-pianobar-process
         (eq 'run (process-status emacspeak-pianobar-process)))
    (call-interactively 'emacspeak-pianobar-command))
   (t  (call-interactively 'emacspeak-pianobar-start))))

(defun emacspeak-pianobar-command (key)
  "Invoke Pianobar  commands."
  (interactive (list (read-key-sequence "PianoBar Key: ")))
  (declare (special emacspeak-pianobar-process))
  (cond
   ((and (stringp key) (string= ":" key))
    (pop-to-buffer (process-buffer emacspeak-pianobar-process))
    (emacspeak-speak-mode-line))
   (t (call-interactively (lookup-key emacspeak-pianobar-mode-map key)))))





(defun emacspeak-pianobar-start ()
  "Start up PianoBar process."
  (interactive)
  (declare (special emacspeak-pianobar-process
                    emacspeak-pianobar-program ))
  (when (and emacspeak-pianobar-process
             (eq 'run (process-status emacspeak-pianobar-process))
             (y-or-n-p "Stop currently playing music? "))
    (kill-process emacspeak-pianobar-process)
    (setq emacspeak-pianobar-process nil))
  (let ((process-connection-type nil))
    (with-current-buffer (get-buffer-create emacspeak-pianobar-buffer)
      (setq emacspeak-pianobar-buffer
            (make-comint "PianoBar" emacspeak-pianobar-program))
      (setq emacspeak-pianobar-process (get-buffer-process emacspeak-pianobar-buffer))
      (emacspeak-pianobar-mode)))
  (switch-to-buffer emacspeak-pianobar-buffer))

;;;###autoload
(defun emacspeak-pianobar-command ()
  "Dispatch PianoBar command read from minibuffer."
  (interactive)
  (let ((command (read-from-minibuffer "PianoBar Command:")))
    (message 
     (emacspeak-pianobar-dispatch command))))

;;}}}

;;}}}
(provide 'emacspeak-pianobar)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

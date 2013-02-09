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
;;;Copyright (C) 1995 -- 2011, T. V. Raman
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

;;; Commentary:

;;; PIANOBAR ==  Pandora Client for Emacs

;;; pianobar git://github.com/PromyLOPh/pianobar.git
;;; Is a stand-alone client for Pandora Radio.
;;; pianobar.el available on the Emacs Wiki at
;;; http://www.emacswiki.org/emacs/pianobar.el
;;; Provides access to Pandora Radio via pianobar from the comfort of Emacs.
;;; This module speech-enables Pianobar and enhances it for the Complete Audio Desktop.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'ansi-color)

;;}}}
;;{{{ Pianobar Fixups:

(defun emacspeak-pianobar-current-song  ()
  "Return current song."
  (declare (special pianobar-current-song))
  (ansi-color-apply
   (substring pianobar-current-song
              (+ 2 (string-match "|>" pianobar-current-song)))))

(defadvice pianobar-currently-playing (around emacspeak pre act comp)
  "Override with our own notifier."
  (message  (emacspeak-pianobar-current-song)))

;;}}}
;;{{{ Advice Interactive Commands:

(defadvice pianobar (after emacspeak pre act comp)
  "Provide auditory feedback."
  (define-key pianobar-key-map "t" 'emacspeak-pianobar-electric-mode-toggle)
  (dotimes (i 10)
    (define-key pianobar-key-map    (format "%s" i )   'emacspeak-pianobar-switch-to-preset ))
  (dotimes (i 25)
    (define-key pianobar-key-map
      (format "%c" (+ i 65))
      'emacspeak-pianobar-switch-to-preset ))
  (define-key pianobar-key-map "(" #'(lambda () (pianobar-send-string "(\n")))
  (define-key pianobar-key-map ")" #'(lambda () (pianobar-send-string ")\n")))  
  (emacspeak-speak-mode-line)  
  (emacspeak-auditory-icon 'open-object))

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
          (when (ems-interactive-p )
            (emacspeak-auditory-icon 'select-object)))))
(defadvice pianobar-window-toggle (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
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
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'close-object)))

;;}}}
;;; Simplified Pianobar Interaction

;;{{{ Customizations And Variables

;;}}}
;;{{{ emacspeak-pianobar
(defvar emacspeak-pianobar-electric-mode nil
  "Records if electric mode is on.")

;;;###autoload
(defun emacspeak-pianobar-electric-mode-toggle ()
  "Toggle electric mode in pianobar buffer.
If electric mode is on, keystrokes invoke pianobar commands directly."
  (interactive)
  (declare (special emacspeak-pianobar-electric-mode
                    pianobar-key-map pianobar-buffer))
  
  (save-excursion
    (set-buffer pianobar-buffer)
    pianobar-buffer
    (cond
     (emacspeak-pianobar-electric-mode  ; turn it off
      (use-local-map nil)
      (setq emacspeak-pianobar-electric-mode nil)
      (emacspeak-auditory-icon 'off))
     (t                                 ;turn it on
      (use-local-map pianobar-key-map)
      (setq emacspeak-pianobar-electric-mode t)
      (emacspeak-auditory-icon 'on)))
    (message "Turned %s pianobar electric mode."
             (if emacspeak-pianobar-electric-mode 'on 'off))))

;;;###autoload
(defun emacspeak-pianobar  ()
  "Start or control Emacspeak Pianobar player."
  (interactive )
  (declare (special pianobar-buffer))
  (condition-case nil
      (unless (featurep 'pianobar) (require 'pianobar))
    (error "Pianobar not installed."))
  (cond
   ((and  (buffer-live-p (get-buffer pianobar-buffer))
          (processp (get-buffer-process pianobar-buffer))
          (eq 'run (process-status (get-buffer-process  pianobar-buffer))))
    (call-interactively 'emacspeak-pianobar-command))
   (t (pianobar))))

(defun emacspeak-pianobar-hide-or-show ()
  "Hide or show pianobar."
  (cond
   ((eq (current-buffer) (get-buffer pianobar-buffer))
    (bury-buffer)
    (emacspeak-auditory-icon 'close-object))
   ((get-buffer-window pianobar-buffer)
    (bury-buffer pianobar-buffer)
    (delete-windows-on pianobar-buffer)
    (emacspeak-auditory-icon 'close-object))
   (t (switch-to-buffer pianobar-buffer)
      (emacspeak-auditory-icon 'open-object))))

(defun emacspeak-pianobar-command (key)
  "Invoke Pianobar  commands."
  (interactive (list (read-key-sequence "Pianobar Key: ")))
  (declare (special pianobar-key-map))
  (cond
   ((and (stringp key)
         (string= "'" key))
    (emacspeak-pianobar-hide-or-show)
    (emacspeak-speak-mode-line))
   ((lookup-key pianobar-key-map key)
    (call-interactively (lookup-key pianobar-key-map key)))
   (t (pianobar-send-string  key))))

;;; Station Presets
(defun emacspeak-pianobar-switch-to-preset ()
  "Switch to one of the  presets."
  (interactive)
  (declare (special last-input-event))
  (let ((preset last-input-event))
    (setq preset 
          (cond
           ((and (<= 48 preset)
                 (<= preset 57))
            (- preset 48))
           ((and (<= 65 preset)
                 (<= preset 90))
            (- preset 55))               ;A == 10
           (t
            (setq preset (read-string "Preset: ")))))
    (pianobar-send-string (format "s%s\n" preset))))
;;}}}

(provide 'emacspeak-pianobar)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}

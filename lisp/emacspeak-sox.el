;;; emacspeak-sox.el --- Speech-enable SOX
;;; $Id: emacspeak-sox.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable SOX An Emacs Interface to sox
;;; Keywords: Emacspeak,  Audio Desktop sox
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
;;;Copyright (C) 1995 -- 2007, 2011, T. V. Raman
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
;;; MERCHANTABILITY or FITNSOX FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary: This module defines a convenient speech-enabled
;;; interface for editting mp3 and wav files using SoX.
;;; Launching this module creates a special interaction buffer 
;;; that provides single keystroke commands for editing and applying effects to a selected sound file.

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'derived)

;;}}}
;;{{{ Define Special Mode

(defun emacspeak-sox-draw-effect (effect)
  "Insert a representation of specified effect at point."
  (let ((name (emacspeak-sox-effect-name effect))
        (params (emacspeak-sox-effect-params effect)))
    (insert (propertize  name 'face  'font-lock-keyword-face))
    (loop
     for p in params do
     (insert (propertize (first p) 'face 'font-lock-string-face))
     (insert " ")
             (insert (propertize (second p) 'face 'bold)))
    (insert "\n")))
     
     


(defun emacspeak-sox-redraw (context)
  "Redraws sox buffer."
  (let ((inhibit-read-only t)
        (orig (point-min))
        (file (emacspeak-sox-context-file context))
        (effects (emacspeak-sox-context-effects context)))
    (goto-char orig)
    (erase-buffer)    
    (insert (propertize "Audio File:  " 'face font-lock-doc-face))
    (when  file (insert  (propertize file 'face font-lock-keyword-face)))
    (insert "\n")
    (when effects (mapc #'emacspeak-sox-draw-effect effects))))

(define-derived-mode emacspeak-sox-mode special-mode
                     "Interactively manipulate audio files."
  "An audio workbench for the Emacspeak desktop."
  (declare (special emacspeak-sox-context))
  (unless emacspeak-sox-context
    (setq emacspeak-sox-context (make-emacspeak-sox-context)))
  (emacspeak-sox-redraw emacspeak-sox-context)
  (setq buffer-read-only t)
  (setq header-line-format "Audio Workbench"))

(defvar emacspeak-sox-buffer "Audio WorkBench"
  "Buffer name of workbench.")

;;;###autoload
(defun emacspeak-sox ()
  "Create a new Audio Workbench or switch to an existing workbench."
  (interactive)
  (declare (special emacspeak-sox-buffer))
  (unless (get-buffer emacspeak-sox-buffer)
    (let ((buffer (get-buffer-create emacspeak-sox-buffer)))
    (with-current-buffer buffer
      (emacspeak-sox-mode)
      (emacspeak-sox-setup-keys))))
  (switch-to-buffer emacspeak-sox-buffer)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-header-line))
    
    
(defgroup emacspeak-sox nil
  "Audio workbench for the Emacspeak Audio Desktop."
  :group 'emacspeak)

(defun emacspeak-sox-setup-keys ()
  "Set up sox keymap."
  (declare (special emacspeak-sox-mode-map))
  (loop
 for k in
 '(
   ("e" emacspeak-sox-set-effect)
   ("f" emacspeak-sox-file)
   ("p" emacspeak-sox-play)
   )
        do
        (emacspeak-keymap-update  emacspeak-sox-mode-map k)))

;;}}}
;;{{{ Top-level Context:

(defstruct emacspeak-sox-effect
  name ; effect name
  params ; list of effect param/value pairs 
  )

(defstruct emacspeak-sox-context
  file ; file being manipulated 
  effects ; list of effects with params
)

(defvar emacspeak-sox-context
  nil
  "Buffer-local handle to sox context.")

(make-variable-buffer-local 'emacspeak-sox-context)

(defcustom emacspeak-sox-wave
  (executable-find "sox")
  "Location of SoX utility."
  :type 'file)

(defcustom emacspeak-sox-play 
  (executable-find "play")
  "Location of play from SoX utility."
  :type 'file)





;;}}}
;;{{{ Common Commands 

(defsubst emacspeak-sox-sound-p (snd-file)
  "Predicate to test if we can edit this file."
  (let ((case-fold-search t))
    (or 
     (string-match  "\\.mp3$" snd-file)
     (string-match "\\.au$" snd-file)
     (string-match "\\.wav$" snd-file))))
     

(defun emacspeak-sox-file (snd-file)
  "Open specified snd-file on the Audio Workbench."
  (interactive "fSound File: ")
  (declare (special emacspeak-sox-context))
  (unless emacspeak-sox-context
    (error "Audio Workbench not initialized."))
  (let ((inhibit-read-only t)
        (type (emacspeak-sox-sound-p snd-file)))
    (unless type (error "%s does not look like a sound file." snd-file))
    (setf (emacspeak-sox-context-file emacspeak-sox-context) snd-file))
  (emacspeak-sox-redraw emacspeak-sox-context)
  (message "Selected file %s" snd-file)
  (emacspeak-auditory-icon 'select-object))

(defun emacspeak-sox-read-timestamp (prompt)
  "Read timestamp hh:mm:ss.sss."
  (declare (special emacspeak-sox-context))
  (let ((timestamp (read-from-minibuffer  prompt)))
    (unless emacspeak-sox-context
      (error "Audio Workbench not initialized."))
    (unless (string-match "[0-9:.]+" timestamp)
      (error "%s does not look like a valid timestamp." timestamp))
    timestamp))






     

(defun emacspeak-sox-play ()
  "Play wave file from specified context."
  (interactive )
  (declare (special emacspeak-sox-context
            emacspeak-sox-play))
  (let* ((c emacspeak-sox-context)
         (file (emacspeak-sox-context-file c))
        (effects (emacspeak-sox-context-effects c))
        (command nil)
        (options nil))
    (loop
     for e in effects  do
     (push 
      (funcall
       (intern (format "emacspeak-sox-get-%s-options" (emacspeak-sox-effect-name e)))
       e)
      options))
    (setq options (mapconcat #'identity  options " "))
    (setq command
          (format "%s %s %s &"
                          emacspeak-sox-play file options))
    (call-process shell-file-name nil nil nil shell-command-switch command)
    command))
    
  
  
;;}}}
;;{{{  SOX for Wave files :





;;}}}
;;{{{ SoX Commands:

(defconst emacspeak-sox-effects
  '("trim"
    "echo"
    )
  "Table of implemented effects.")


(defun emacspeak-sox-set-effect (name)
  "Set effect."
  (interactive
   (list (completing-read "SoX Effect: " emacspeak-sox-effects)))
  (declare (special emacspeak-sox-context  emacspeak-sox-effects))
      (setf (emacspeak-sox-context-effects emacspeak-sox-context)
            (list  (funcall (intern (format  "emacspeak-sox-get-%s-effect"  name)))))
    (emacspeak-sox-redraw emacspeak-sox-context)
  (message "Set effect  %s" name))

(defun emacspeak-sox-get-trim-effect ()
  "Read needed params for effect trim,
and return a suitable effect structure."
  (make-emacspeak-sox-effect
   :name "trim"
   :params
   (let ((s (read-from-minibuffer "Time Offset: "))
         (params nil))
     (while (string-match "[0-9:.]+" s)
       (push  (list "|" s) params)
       (setq s (read-from-minibuffer "Offset Time: ")))
     (nreverse params))))
     
   

(defun emacspeak-sox-get-trim-options   (effect)
  "Construct options  portion of commandline for this trim effect."
  (let ((params (emacspeak-sox-effect-params effect)))
    (format "trim %s"
            (mapconcat #'second params " "))))
    
          

;;}}}

(provide 'emacspeak-sox)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

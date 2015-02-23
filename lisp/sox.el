;;; $Id: sox.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
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
;;;
;;; Launching this module creates a special interaction buffer
;;; that provides single keystroke commands for editing and
;;; applying effects to a selected sound file. For adding mp3
;;; support to sox,
;;;
 ;;; sudo apt-get libsox-fmt-mp3 install
;;;
;;; This module can be used independent of Emacspeak.

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'derived)

;;}}}
;;{{{ Customizations:

(defgroup sox nil
  "Audio workbench for the Emacspeak Audio Desktop."
  :group 'emacspeak
  :group 'applications)

(defcustom sox-edit
  (executable-find "sox")
  "Location of SoX utility."
  :type 'file)

(defcustom sox-play (executable-find "play")
  "Location of play from SoX utility."
  :type 'file)

;;}}}
;;{{{ Define Special Mode
(defsubst sox-effect-at-point (&optional pos)
  "Return effect at  point."
  (get-text-property (or pos (point)) 'sox-effect))

(defun sox-draw-effect (effect)
  "Insert a representation of specified effect at point."
  (let ((name (sox-effect-name effect))
        (params (sox-effect-params effect))
        (orig (point)))
    (insert (propertize  name 'face  'fixed-pitch))
    (insert ":\t")
    (loop
     for p in params do
     (when (second p) (insert (propertize (first p) 'face 'italic ))
           (insert "\t")
           (insert (propertize (second p) 'face 'bold))
           (insert "\t")))
    (put-text-property orig (point) 'sox-effect effect)
    )
  (insert "\n"))

(defun sox-redraw (context)
  "Redraws sox buffer."
  (let ((inhibit-read-only t)
        (orig (point-min))
        (file  (sox-context-file context))
        (effects (sox-context-effects context)))
    (goto-char orig)
    (when file (setq file (abbreviate-file-name file )))
    (erase-buffer)
    (insert (propertize "Audio File:  " 'face font-lock-doc-face))
    (when  file (insert  (propertize file 'face font-lock-keyword-face)))
    (insert "\n")
    (when effects (mapc #'sox-draw-effect  effects))
    (goto-char (point))))

(defun sox-refresh ()
  "Redraw Audio Workbench."
  (interactive)
  (declare (special sox-context))
  (sox-redraw sox-context))
(defconst sox-header-line-format
  '((:eval
     (format
      "%s: %s"
      (propertize "Audacious" 'face 'bold)
      (propertize
       (abbreviate-file-name
        (or (and sox-context (sox-context-file sox-context)) "")))
      'face 'font-lock-keyword-face)))
  "Header line format for SoX buffers.")

(define-derived-mode sox-mode special-mode
                     "Interactively manipulate audio files."
  "An audio workbench for the Emacspeak desktop."
  (declare (special sox-context))
  (setq sox-context (make-sox-context))
  (sox-redraw sox-context)
  (setq buffer-read-only t)
  (setq tab-width 8)
  (setq header-line-format sox-header-line-format))

(defvar sox-buffer "Audio WorkBench"
  "Buffer name of workbench.")

;;;###autoload
(defun sox ()
  "Create a new Audio Workbench or switch to an existing workbench."
  (interactive)
  (declare (special sox-buffer))
  (unless (get-buffer sox-buffer)
    (let ((buffer (get-buffer-create sox-buffer)))
      (with-current-buffer buffer
        (sox-mode)
        (sox-setup-keys))))
  (funcall-interactively #'switch-to-buffer sox-buffer))

(defun sox-setup-keys ()
  "Set up sox keymap."
  (declare (special sox-mode-map))
  (loop
   for k in
   '(
     ("E" sox-add-effect)
     ("e" sox-set-effect)
     ("f" sox-open-file)
     ("g" sox-refresh)
     ("p" sox-play)
     ("s" sox-save)
     ("k" sox-stop)
     ("\C-k" sox-delete-effect-at-point)
     ((kbd "RET") sox-edit-effect-at-point)
     )
   do
   (define-key sox-mode-map (first k) (second k))))

;;}}}
;;{{{ Top-level Context:

(defstruct sox-effect
  name ; effect name
  params ; list of effect name/value pairs
  )

(defstruct sox-context
  file ; file being manipulated
  effects ; list of effects with params
  start-time ; play start time
  stop-time ; play stop time
  play ; play process handle
  )

(defvar sox-context nil
  "Buffer-local handle to sox context.")

(make-variable-buffer-local 'sox-context)

;;}}}
;;{{{ Commands:

(defvar sox-sound-regexp
  (regexp-opt  '(".mp3" ".wav" ".au"))
  "Regexp matching sound files.")

(defsubst sox-sound-p (snd-file)
  "Predicate to test if we can edit this file."
  (declare (special sox-sound-regexp))
  (let ((case-fold-search t))
    (string-match  sox-sound-regexp snd-file)))

(defun sox-open-file (snd-file)
  "Open specified snd-file on the Audio Workbench."
  (interactive "fSound File: ")
  (declare (special sox-context))
  (unless sox-context (error "Audio Workbench not initialized."))
  (let ((inhibit-read-only t)
        (type (sox-sound-p snd-file)))
    (unless type (error "%s does not look like a sound file." snd-file))
    (setf (sox-context-file sox-context)
          snd-file))
  (cd(file-name-directory snd-file))
  (sox-redraw sox-context)
  (message "Selected file %s" snd-file))

(defun sox-action (context action &optional save-file)
  "Apply action to    current context."
  (let ((file (sox-context-file context))
        (effects (sox-context-effects context))
        (command nil)
        (options nil))
    (loop
     for e in effects  do
     (push (sox-effect-name e) options)
     (loop
      for  p in (sox-effect-params e) do
      (when (second p)(push (second p)  options))))
    (setq options (nreverse  options))
    (when (string= action sox-edit) (push save-file options))
    (apply #'start-process
           sox-play "*SOX*" action file options)))

(defun sox-play ()
  "Play sound from current context."
  (interactive)
  (declare (special sox-context sox-play))
  (setf (sox-context-start-time sox-context) (current-time))
  (setf (sox-context-play sox-context)(sox-action sox-context
                                                  sox-play)))

(defun sox-stop ()
  "Stop currently playing  sound from current context."
  (interactive)
  (declare (special sox-context))
  (setf (sox-context-stop-time sox-context) (current-time))
  (delete-process (sox-context-play sox-context))
  (message
   "%s"
   (time-to-seconds
    (time-subtract
     (sox-context-stop-time sox-context)
     (sox-context-start-time sox-context)))))

(defun sox-save(save-file)
  "Save context to  file after prompting."
  (interactive "FSave File: ")
  (declare (special sox-context sox-edit))
  (sox-action sox-context sox-edit save-file))
(defun sox-edit-effect-at-point ()
  "Edit effect at point."
  (interactive)
  (let ((inhibit-read-only  t)
        (effect (get-text-property (point) 'sox-effect))
        (desc nil)
        (repeat nil))
    (unless effect (error "No effect at point."))
    (setq desc (intern (format "sox-%s-params" (sox-effect-name effect))))
    (setq repeat (get desc 'repeat))
    (setf (sox-effect-params effect)
          (sox-read-effect-params (eval desc) repeat ))
    (delete-region (line-beginning-position) (line-end-position))
    (sox-draw-effect effect)
    (flush-lines "^ *$" (point-min) (point-max))))

(defun sox-delete-effect-at-point ()
  "Delete effect at point."
  (interactive)
  (declare (special sox-context))
  (let ((inhibit-read-only  t)
        (e (sox-effect-at-point)))
    (unless e (error "No effect at point."))
    (setf  (sox-context-effects sox-context) (remove e (sox-context-effects sox-context)))
    (message "Deleted effect %s at point. " (sox-effect-name e ))
    (sox-redraw sox-context)))

(defun sox-set-effect (name)
  "Set effect."
  (interactive
   (list (completing-read "SoX Effect: " sox-effects nil t)))
  (declare (special sox-context  sox-effects))
  (setf (sox-context-effects sox-context)
        (list
         (funcall (intern (format  "sox-get-%s-effect"  name)))))
  (sox-redraw sox-context)
  (message "Set effect  %s" name))

(defun sox-add-effect (name)
  "Adds  effect at the end of the effect list"
  (interactive
   (list (completing-read "Add SoX Effect: "  sox-effects nil t)))
  (declare (special sox-context  sox-effects))
  (setf (sox-context-effects sox-context)
        (append
         (sox-context-effects sox-context)
         (list
          (funcall (intern (format  "sox-get-%s-effect"  name))))))
  (sox-redraw sox-context)
  (message "Set effect  %s" name))

(defsubst sox-read-effect-params-per-desk (p)
  "Read sox effect param per spec."
  (let ((result (read-from-minibuffer (capitalize p))))
    (when (>  (length result) 0) (list p result ))))

(defun sox-read-effect-params (param-desc &optional repeat)
  "Read list of effect  params."
  (let ((r (delq nil (mapcar #'sox-read-effect-params-per-desk param-desc))))
    ;;; Now handle repeat
    (cond
     ((null repeat ) r) ; base case
     ((null r) r) ; all done
     (t ; recur till done
      (append r (sox-read-effect-params param-desc 'repeat))))))

;;}}}
;;{{{  Effects Infrastructure:

;;; To define support for an effect,:
;;; 1. Add it to the effect table below.
;;; 2. Clone the code from one of the previously implemented effects,
;;; And update per the SoX man page.

(defconst sox-effects
  '(
    "bass"
    "chorus"
    "reverb"
    "treble"
    "trim")
  "Table of implemented effects.")

;;}}}
;;{{{ Trim:

(defvar sox-trim-params '("|")
  "Parameter spec for effect trim.")
(put 'sox-trim-params 'repeat t)

(defun sox-get-trim-effect ()
  "Read needed params for effect trim,
and return a suitable effect structure."
  (make-sox-effect
   :name "trim"
   :params
   (sox-read-effect-params sox-trim-params 'repeat)))

;;}}}
;;{{{ Bass:

;;; bass|treble gain [frequency[k] [width[s|h|k|o|q]]]
(defvar sox-bass-params
  '("gain" "frequency" "width")
  "Params accepted by bass.")

(defun sox-get-bass-effect ()
  "Read needed params for effect bass,
and return a suitable effect structure."
  (declare (special sox-bass-params))
  (make-sox-effect
   :name "bass"
   :params (sox-read-effect-params sox-bass-params)))

;;}}}
;;{{{ Treble:

;;; bass|treble gain [frequency[k] [width[s|h|k|o|q]]]
(defvar sox-treble-params
  '("gain" "frequency" "width")
  "Params accepted by treble.")

(defun sox-get-treble-effect ()
  "Read needed params for effect treble,
and return a suitable effect structure."
  (declare (special sox-treble-params))
  (make-sox-effect
   :name "treble"
   :params (sox-read-effect-params sox-treble-params) ))

;;}}}
;;{{{ Chorus:

;;;  chorus gain-in gain-out <delay decay speed depth -s|-t>
(defvar sox-chorus-params
  '("gain-in" "gain-out" "delay" "decay" "speed" "step" "shape" )
  "Parameters for effect chorus.")
(put 'sox-chorus-params 'repeat t)
(defun sox-get-chorus-effect  ()
  "Read needed params for effect chorus
and return a suitable effect structure."
  (declare (special sox-chorus-params))
  (make-sox-effect
   :name "chorus"
   :params (sox-read-effect-params sox-chorus-params)))

;;}}}
;;{{{ Reverb:

;;;reverb [-w|--wet-only] [reverberance (50%) [HF-damping (50%)
;;; [room-scale (100%) [stereo-depth (100%)
;;; [pre-delay (0ms) [wet-gain (0dB)]]]]]]

;; reverb [-w|--wet-only] [reverberance (50%) [HF-damping (50%)
;;               [room-scale (100%) [stereo-depth (100%)
;;               [pre-delay (0ms) [wet-gain (0dB)]]]]]]

(defconst sox-reverb-params
  '("-w"  "reverb" "hf-damp"
    "room-scale" "stereo-depth"
    "pre-delay"  "wet-gain")
  "Parameters for effect reverb.")

(defun sox-get-reverb-effect  ()
  "Read needed params for effect reverb
and return a suitable effect structure."
  (declare (special sox-reverb-params))
  (make-sox-effect
   :name "reverb"
   :params (sox-read-effect-params sox-reverb-params)))

;;}}}
(provide 'sox)
;;{{{ Add Emacspeak Support

;;; Code here can be factored out to emacspeak-sox.el
(require 'emacspeak-preamble)

(defadvice sox (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-speak-header-line)))

(defadvice sox-open-file(after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)))

(provide 'emacspeak-sox)
(defadvice sox-refresh (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice sox-delete-effect-at-point (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)))

;;}}}
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

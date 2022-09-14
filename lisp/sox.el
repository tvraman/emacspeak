;;; sox.el --- An Audio Work-Bench -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable SOX An Emacs Interface to sox
;; Keywords: Emacspeak,  Audio Desktop sox
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; 
;;  $Revision: 4532 $ |
;; Location undetermined
;; 

;;}}}
;;{{{  Copyright:
;; Copyright (C) 1995 -- 2022, T. V. Raman
;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
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
;; MERCHANTABILITY or FITNSOX FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;; This module defines a convenient speech-enabled
;; interface for editing mp3 and wav files using SoX.
;; 
;; Launching M-x sox  creates a special interaction buffer
;; that provides single keystroke commands for editing and
;; applying effects to a selected sound file. For adding mp3
;; support to sox, do
;; 
;; sudo apt-get libsox-fmt-mp3 install
;; 
;; This module provides support for ladspa effects using module ladspa.el.
;; To use ladspa effects with SoX, you need a relatively new build of Sox;
;; The stock SoX that is package for Debian/Ubuntu  does not always work.
;; This module can be used independent of Emacspeak.
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(eval-when-compile (require 'derived))
(require 'ladspa)

;;}}}
;;{{{ Customizations:

(defgroup sox nil
  "Audio workbench for the Emacspeak Audio Desktop."
  :group 'emacspeak
  :group 'applications)

(defvar sox-edit
  (executable-find "sox")
  "Location of SoX utility.")

(defvar sox-play (executable-find "play")
  "Location of play from SoX utility.")

;;}}}
;;{{{ Define Special Mode

(defun sox-effect-at-point (&optional pos)
  "Return effect at  point."
  (get-text-property (or pos (point)) 'sox-effect))

(defun sox-draw-effect (effect)
  "Insert a representation of specified effect at point."
  (let ((name (sox-effect-name effect))
        (type (sox-effect-type effect))
        (params (sox-effect-params effect))
        (orig (point)))
    (insert (propertize  name 'face  'fixed-pitch))
    (insert ":\t")
    (cond
     ((eq 'ladspa type)
      (insert
       (mapconcat
        #'ladspa-control-value
        (ladspa-plugin-controls (sox-effect-params effect)) " ")))
     (t
      (cl-loop
       for p in params do
       (when (cl-second p) (insert (propertize (cl-first p) 'face 'italic))
             (insert "\t")
             (insert (propertize (cl-second p) 'face 'bold))
             (insert "\t")))))
    (put-text-property orig (point) 'sox-effect effect))
  (insert "\n"))

(defun sox-redraw (context)
  "Redraws sox buffer."
  (let ((inhibit-read-only t)
        (orig (point-min))
        (file  (sox-context-file context))
        (effects (sox-context-effects context)))
    (goto-char orig)
    (when file (setq file (abbreviate-file-name file)))
    (erase-buffer)
    (insert (propertize "Audio File:  " 'face font-lock-doc-face))
    (when  file (insert  (propertize file 'face font-lock-keyword-face)))
    (insert "\n")
    (when effects (mapc #'sox-draw-effect  effects))
    (goto-char (point))))

(defun sox-refresh ()
  "Redraw Audio Workbench."
  (interactive)
  (cl-declare (special sox-context))
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
  (cl-declare (special sox-context))
  (setq sox-context (make-sox-context))
  (sox-redraw sox-context)
  (setq buffer-read-only t)
  (setq tab-width 8)
  (setq header-line-format sox-header-line-format))

(defvar sox-buffer "Audio WorkBench"
  "Buffer name of workbench.")

;;;###autoload
(defun sox ()
  "Create a new Audio Workbench or switch to one."
  (interactive)
  (cl-declare (special sox-buffer))
  (unless (get-buffer sox-buffer)
    (let ((buffer (get-buffer-create sox-buffer)))
      (with-current-buffer buffer
        (sox-mode)
        (sox-setup-keys))))
  (funcall-interactively #'pop-to-buffer sox-buffer))

(defun sox-setup-keys ()
  "Set up sox keymap."
  (cl-declare (special sox-mode-map))
  (cl-loop
   for k in
   '(
     ("." sox-show-timestamp)
     ("C-k" sox-delete-effect-at-point)
     ("e" sox-add-effect)
     ("RET" sox-edit-effect-at-point)
     ("E" sox-set-effect)
     ("f" sox-open-file)
     ("g" sox-refresh)
     ("k" sox-stop)
     ("p" sox-play)
     ("s" sox-save)
     )
   do
   (define-key sox-mode-map (kbd (cl-first k)) (cl-second k))))

;;}}}
;;{{{ Top-level Context:

(cl-defstruct sox-effect
  type ; native: nil ladspa: 'ladspa
  name ; effect name
  params ; list of effect name/value pairs
  )

(cl-defstruct sox-context
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
  (regexp-opt  '(".mp3" ".wav" ".au" ".aiff"))
  "Regexp matching sound files.")

(defun sox-sound-p (snd-file)
  "Predicate to test if we can edit this file."
  (cl-declare (special sox-sound-regexp))
  (let ((case-fold-search t))
    (string-match  sox-sound-regexp snd-file)))

(defun sox-open-file (snd-file)
  "Open specified snd-file on the Audio Workbench."
  (interactive "fSound File: ")
  (cl-declare (special sox-context))
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
        (options nil))
    (cl-loop
     for e in effects  do
     (cond
      ((eq 'ladspa (sox-effect-type e))
       (mapc #'(lambda(o)  (push o    options))
             (sox-ladspa-cmd (sox-effect-params e))))
      (t
       (push (sox-effect-name e) options)
       (cl-loop
        for  p in (sox-effect-params e) do
        (when (cl-second p)(push (cl-second p)  options))))))
    (setq options (nreverse  options))
    (when (string= action sox-edit) (push save-file options))
    (apply #'start-process
           "player" "*SOX*"
           action file options)))
;;;###autoload
(defun sox-play ()
  "Play sound ."
  (interactive)
  (cl-declare (special sox-context sox-play))
  (when (process-live-p (sox-context-play sox-context))
    (error "Already playing stream."))
  (setf (sox-context-start-time sox-context) (current-time))
  (setf (sox-context-play sox-context)
        (sox-action sox-context sox-play)))

(defun sox-stop ()
  "Stop currently playing  sound from current context."
  (interactive)
  (cl-declare (special sox-context))
  (unless (process-live-p (sox-context-play sox-context))
    (error "Not playing stream."))
  (setf (sox-context-stop-time sox-context) (current-time))
  (delete-process (sox-context-play sox-context))
  (message
   "%.2f"
   (float-time
    (time-subtract
     (sox-context-stop-time sox-context)
     (sox-context-start-time sox-context)))))

(defun sox-show-timestamp ()
  "Show timestamp   in stream."
  (interactive)
  (unless (process-live-p (sox-context-play sox-context))
    (error "Not playing stream."))
  (cl-declare (special sox-context))
  (message
   "%.2f"
   (float-time
    (time-subtract (current-time) (sox-context-start-time sox-context)))))

(defun sox-save(save-file)
  "Save context to  file after prompting."
  (interactive "FSave File: ")
  (cl-declare (special sox-context sox-edit))
  (sox-action sox-context sox-edit save-file))
(defun sox-edit-effect-at-point ()
  "Edit effect at point."
  (interactive)
  (let ((inhibit-read-only  t)
        (effect (get-text-property (point) 'sox-effect))
        (desc nil)
        (repeat nil))
    (unless effect (error "No effect at point."))
    (cond
     ((eq 'ladspa (sox-effect-type   effect))
      (ladspa-create (sox-effect-params effect)))
     (t
      (setq desc (intern (format "sox-%s-params" (sox-effect-name effect))))
      (setq repeat (get desc 'repeat))
      (setf (sox-effect-params effect)
            (sox-read-effect-params (eval desc) repeat))
      (delete-region (line-beginning-position) (line-end-position))
      (sox-draw-effect effect)
      (flush-lines "^ *$" (point-min) (point-max))))))

(defun sox-delete-effect-at-point ()
  "Delete effect at point."
  (interactive)
  (cl-declare (special sox-context))
  (let ((inhibit-read-only  t)
        (e (sox-effect-at-point)))
    (unless e (error "No effect at point."))
    (setf  (sox-context-effects sox-context)
           (remove e (sox-context-effects sox-context)))
    (message "Deleted effect %s at point. " (sox-effect-name e))
    (sox-redraw sox-context)))

(defun sox-set-effect (name)
  "Set effect."
  (interactive
   (list (completing-read "SoX Effect: " sox-effects nil t)))
  (cl-declare (special sox-context  sox-effects))
  (setf (sox-context-effects sox-context)
        (list
         (funcall (intern (format  "sox-get-%s-effect"  name)))))
  (sox-redraw sox-context)
  (message "Set effect  %s" name))

(defun sox-add-effect (name)
  "Adds  effect at the end of the effect list"
  (interactive
   (list (completing-read "Add SoX Effect: "  sox-effects nil t)))
  (cl-declare (special sox-context  sox-effects))
  (setf (sox-context-effects sox-context)
        (append
         (sox-context-effects sox-context)
         (list
          (funcall (intern (format  "sox-get-%s-effect"  name))))))
  (sox-redraw sox-context)
  (message "Set effect  %s" name))

(defun sox-read-effect-params-per-desk (p)
  "Read sox effect param per spec."
  (let ((result (read-from-minibuffer (capitalize p))))
    (when (>  (length result) 0) (list p result))))

(defun sox-read-effect-params (param-desc &optional repeat)
  "Read list of effect  params."
  (let ((r (delq nil (mapcar #'sox-read-effect-params-per-desk param-desc))))
    ;;; Now handle repeat
    (cond
     ((null repeat) r) ; base case
     ((null r) r) ; all done
     (t ; recur till done
      (append r (sox-read-effect-params param-desc 'repeat))))))

;;}}}
;;{{{  Effects Infrastructure:
(defvar sox-effects nil
  "Table of implemented effects.")

(defun
    sox-register-effect (name)
  "Register effect."
  (cl-pushnew name sox-effects :test #'string=))

;; To define support for an effect,:
;; 1. Add it to the effect table below.
;; 2. Clone the code from one of the previously implemented effects,
;; And update per the SoX man page.

;;}}}
;;{{{ Ladspa Effects:

;; Heavy lifting done by Ladspa module.

(defvar sox-ladspa-params nil
  "Generic spec for ladspa effect.")

(put 'sox-ladspa-params 'create #'ladspa-create)
(sox-register-effect "ladspa")

(defun sox-get-ladspa-effect ()
  "Read needed params for effect ladspa,
and return a suitable effect structure."
  (ladspa-plugins)
  (let ((plugin (ladspa-create (ladspa-read "Ladspa effect: "))))
    (make-sox-effect
     :type 'ladspa
     :name (ladspa-plugin-label plugin)
     :params plugin)))

;;}}}
;;{{{ Apply Ladspa to SoX:

(defun sox-ladspa-cmd (plugin)
  "Convert Ladspa Plugin to SoX args."
  `("ladspa"
    ,(ladspa-plugin-library plugin) ,(ladspa-plugin-label plugin)
    ,@(mapcar #'ladspa-control-value  (ladspa-plugin-controls plugin))))

;;}}}
;;{{{ Define SoX Effect: Macro

(defun sox-def-effect (name params repeat)
  "Defines needed functions and variables for manipulating effect name."
  (let ((p-sym (intern (format "sox-%s-params" name)))
        (getter (intern (format "sox-get-%s-effect" name))))
    ;;; Register effect
    (sox-register-effect name)
    ;; Parameter template used for prompting:
    (eval
     `(defconst ,p-sym ',params
        ,(format "Parameters for effect %s" name)))

    ;; Set up  repeat
    (when repeat
      (eval `(put ',p-sym 'repeat t)))

    ;; Function  for generating effect structure:
    (eval
     `(defun ,getter ()
        ,(format "Read needed params for effect %s
and return a suitable effect structure." name)
        (cl-declare (special ,p-sym))
        (make-sox-effect
         :name ,name
         :params (sox-read-effect-params ,p-sym ,repeat))))))

;;}}}
;;{{{ Use: sox-def-effect

(sox-def-effect "echo" '("gain-in" "gain-out" "delay" "decay") t)

(sox-def-effect "echos" '("gain-in" "gain-out" "delay" "decay") t)

(sox-def-effect "channels" '("count") nil)

(sox-def-effect "remix" '("out-spec") t)

(sox-def-effect "trim" '("position") t)

(sox-def-effect "bass" '("gain" "frequency" "width") nil)

(sox-def-effect "treble" '("gain" "frequency" "width") nil)

(sox-def-effect
 "chorus"
 '("gain-in" "gain-out" "delay" "decay" "speed" "step" "shape")
 t)

(sox-def-effect "fade" '("shape"  "fade-in" "stop" "fade-out") nil)

;; reverb:
;; reverb [-w|--wet-only] [reverberance (50%) [HF-damping (50%)
;; [room-scale (100%) [stereo-depth (100%)
;; [pre-delay (0ms) [wet-gain (0dB)]]]]]]
(sox-def-effect
 "reverb"
 '("-w"  "reverb" "hf-damp"
   "room-scale" "stereo-depth"
   "pre-delay"  "wet-gain")
 nil)

;;}}}
(provide 'sox)
;;{{{ Add Emacspeak Support

;; Code here can be factored out to emacspeak-sox.el
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

(defadvice sox-open-file(after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)))

(defadvice sox-refresh (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice sox-delete-effect-at-point (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)))
(provide 'emacspeak-sox)

;;}}}
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}

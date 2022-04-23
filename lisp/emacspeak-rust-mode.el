;;; emacspeak-rust-mode.el --- Speech-enable RUST-MODE  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable RUST-MODE An Emacs Interface to rust-mode
;; Keywords: Emacspeak,  Audio Desktop rust-mode
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
;; Copyright (C) 1995 -- 2007, 2019, T. V. Raman
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
;; MERCHANTABILITY or FITNRUST-MODE FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;; Speech-enable rust-mode

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map Faces:

(voice-setup-add-map
 '(
   (rust-builtin-formatting-macro-face voice-lighten)
   (rust-question-mark-face voice-smoothen)
   (rust-string-interpolation-face voice-lighten-medium)
   (rust-unsafe-face voice-animate)))

;;}}}
;;{{{ Interactive Commands: (rust-mode

(cl-loop
 for f in 
 '(rust-compile rust-run rust-test rust-run-clippy rust-promote-module-into-dir)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'task-done))))) 



(defadvice rust-dbg-wrap-or-unwrap (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (emacspeak-speak-line)))

(defadvice rust-format-buffer (after emacspeak pre act comp)
  "speak."
  (cond
   ((buffer-live-p (get-buffer rust-rustfmt-buffername))
    (emacspeak-auditory-icon 'open-object))
   
   (t (emacspeak-auditory-icon 'task-done))))

(defadvice rust-goto-format-problem (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'large-movement))))

(defadvice rust-enable-format-on-save (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'on)
    (message "Enabled format on save")))

(defadvice rust-disable-format-on-save (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'off)
    (message "Disabled format on save")))

(cl-loop
 for f in
 '(rust-beginning-of-defun rust-end-of-defun)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))


(defun emacspeak-rust-mode-setup ()
  "Setup additional keys etc."
  (cl-declare (special rust-mode-map))
  (when (and (bound-and-true-p rust-mode-map)
             (keymapp rust-mode-map))
    (define-key rust-mode-map (ems-kbd "C-c C-c")'rust-compile)
    (define-key rust-mode-map (ems-kbd "C-c C-r")'rust-run)
    (define-key rust-mode-map (ems-kbd "C-c C-t")'rust-test)))

(emacspeak-rust-mode-setup)

;;}}}
;;{{{Interactive Commands: rustic

(cl-loop
 for f in 
 '(
   rustic-beginning-of-defun rustic-end-of-defun
                             rustic-beginning-of-function rustic-end-of-string)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (let ((emacspeak-show-point t))
         (emacspeak-auditory-icon 'large-movement)
         (emacspeak-speak-line))))))

;;}}}
(provide 'emacspeak-rust-mode)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}

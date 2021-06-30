;;; emacspeak-geiser.el --- Speech-enable GEISER  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable GEISER An Emacs Interface to geiser
;;; Keywords: Emacspeak,  Audio Desktop geiser (Scheme IDE)
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
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
;;; MERCHANTABILITY or FITNGEISER FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA..

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; geiser.el --- GNU Emacs and Scheme talk to each other
;;; This module speech-enables all interactive aspects of geiser,
;;; including the geiser->scheme REPL.
;;; This is used by racket-mode for racket interaction,
;;; And also for interacting with Guile.

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
   (geiser-font-lock-autodoc-current-arg  voice-bolden)
   (geiser-font-lock-autodoc-identifier voice-animate)
   (geiser-font-lock-doc-button voice-bolden-extra)
   (geiser-font-lock-doc-link voice-bolden)
   (geiser-font-lock-doc-title voice-smoothen)
   (geiser-font-lock-error-link voice-annotate)
   (geiser-font-lock-image-button voice-bolden-medium)
   (geiser-font-lock-repl-input voice-lighten)
   (geiser-font-lock-repl-prompt voice-lighten)
   (geiser-font-lock-xref-header voice-smoothen)
   (geiser-font-lock-xref-link voice-bolden)))

;;}}}
;;{{{ Interactive Commands:

(cl-loop
 for f in
 '(
   geiser run-geiser
   geiser--switch-to-repl
   geiser-mode-switch-to-repl geiser-doc-switch-to-repl
   geiser-mode-switch-to-repl-and-enter geiser-show-logs)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-speak-mode-line)
       (emacspeak-auditory-icon 'open-object)))))

(cl-loop
 for f in
 '(
   geiser-compile-current-buffer geiser-compile-definition
   geiser-compile-definition-and-go geiser-compile-file geiser-eval-buffer
   geiser-eval-buffer-and-go geiser-eval-definition geiser-eval-definition-and-go
   geiser-eval-last-sexp geiser-eval-region geiser-eval-region-and-go
   geiser-expand-definition geiser-expand-last-sexp geiser-expand-region
   geiser-load-current-buffer geiser-load-file
   geiser-log-clear geiser-repl-clear-buffer
   geiser-squarify geiser-pop-symbol-stack geiser-insert-lambda)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'task-done)))))

(cl-loop
 for f in
 '(
   geiser-doc-edit-symbol-at-point
   geiser-edit-symbol-at-point geiser-doc-symbol-at-point
   geiser-doc-refresh geiser-doc-previous-section
   geiser-doc-previous geiser-doc-next-section geiser-doc-next
   geiser-doc-module geiser-doc-look-up-manual
   geiser-edit--open-next geiser-edit-module
   geiser-edit-module-at-point geiser-edit-symbol)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'open-object)))))

(cl-loop
 for f in
 '(geiser-repl--bol geiser-repl--newline-and-indent)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'select-object)
       (emacspeak-speak-line)))))

(cl-loop
 for f in
 '(
   geiser-repl-previous-prompt geiser-repl-next-prompt
   geiser-repl--previous-error  geiser-repl--next-error
   )
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))

(defadvice geiser-repl-exit (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))

(defadvice geiser-repl-import-module(around emacspeak pre act comp)
  "speak."
  (cond
   ((ems-interactive-p)
    (let ((start (point)))
      ad-do-it
      (emacspeak-auditory-icon 'task-done)
      (emacspeak-speak-region start (point))))
   (t ad-do-it))
  ad-return-value)

(defadvice geiser-repl--maybe-send(around emacspeak pre act comp)
  "speak."
  (cond
   ((ems-interactive-p)
    (let ((start (point)))
      ad-do-it
      (emacspeak-auditory-icon 'close-object)
      (emacspeak-speak-region start (point))))
   (t ad-do-it))
  ad-return-value)

(defadvice geiser-repl--doc-module (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (with-current-buffer (window-buffer (selected-window))
      (emacspeak-auditory-icon 'open-object)
      (emacspeak-speak-buffer))))

(cl-loop
 for f in
 '(
   geiser-xref-callees geiser-xref-callers geiser-xref-generic-methods
   )
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))

;;}}}
(provide 'emacspeak-geiser)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

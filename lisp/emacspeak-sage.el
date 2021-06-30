;;; emacspeak-sage.el --- Speech-enable SAGE  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable SAGE An Emacs Interface to sage
;;; Keywords: Emacspeak,  Audio Desktop sage
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
;;; MERCHANTABILITY or FITNSAGE FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA..

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; Speech-enable @code{sage-shell-mode}.
;;; This is a major mode for interacting with @code{sage},
;;;  @url{http://www.sagemath.org/}
;;; An Open-source  Mathematical Software System.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)


;;}}}
;;{{{ Forward Decls:
(declare-function sage-shell:delete-output "sage-shell-mode" nil)
(declare-function sage-shell:-send-input-one-line "sage-shell-mode" (line))
(declare-function  sage-shell-help:describe-symbol "emacspeak-sage" t)
(declare-function sage-shell-edit:process-alist "sage-shell-mode" nil)
(declare-function sage-shell:last-output-beg-end "sage-shell-mode" nil)

;;}}}
;;{{{ Helpers:

(defun emacspeak-sage-get-output ()
  "Return most recent Sage output"
  (interactive)
  (with-current-buffer
      (process-buffer (car (cl-first  (sage-shell-edit:process-alist))))
    (apply #'buffer-substring (sage-shell:last-output-beg-end))))

(defun emacspeak-sage-speak-output ()
  "Speak last output from Sage."
  (interactive)
  (cl-assert
   (memq  major-mode '(sage-shell-mode sage-shell:sage-mode))
   t "Not in a Sage buffer")
  (cl-flet
      ((say-it ()
               (dtk-speak
                (apply #'buffer-substring (sage-shell:last-output-beg-end)))))
    (cond
     ((eq major-mode 'sage-shell-mode) (say-it))
     ((eq major-mode 'sage-shell:sage-mode)
      (cl-assert   (sage-shell-edit:process-alist) t "No running Sage.")
;;; Take the first one for now:
      (with-current-buffer
          (process-buffer (car (cl-first  (sage-shell-edit:process-alist))))
        (say-it))))))

(defun emacspeak-sage-get-output-as-latex ()
  "Return most recent Sage output as LaTeX markup."
  (interactive)
  (cl-assert (eq major-mode 'sage-shell:sage-mode) t "Not in a sage buffer")
  (cl-assert   (sage-shell-edit:process-alist) t "No running Sage.")
  (let ((orig (emacspeak-sage-get-output))
        (result nil))
    (with-current-buffer
        (process-buffer (car (cl-first  (sage-shell-edit:process-alist))))
      (sage-shell:-send-input-one-line (format "latex(%s)" orig))
      (sit-for .1)
      (setq result (emacspeak-sage-get-output))
      (sage-shell:delete-output)
      result)))

;;}}}
;;{{{ Advice Help:
(defadvice sage-shell-help:describe-symbol (after emacspeak pre act comp)
  "speak."
  (with-current-buffer (window-buffer (selected-window))
    (emacspeak-auditory-icon 'help)
    (emacspeak-speak-buffer)))

(cl-loop
 for f in
 '(
   sage-shell-help:forward-history sage-shell-help:backward-history
   sage-shell:help)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'help)
       (emacspeak-speak-buffer)))))

(emacspeak-auditory-icon 'help)

;;}}}
;;{{{ Advice sage-edit:

(cl-loop
 for f in
 '(
   sage-shell-blocks:send-current
   sage-shell-edit:load-current-file
   sage-shell-edit:load-current-file-and-go
   sage-shell-edit:load-file
   sage-shell-edit:load-file-and-go
   sage-shell-edit:pop-to-process-buffer
   sage-shell-edit:send--buffer
   sage-shell-edit:send--buffer-and-go
   sage-shell-edit:send-buffer
   sage-shell-edit:send-buffer-and-go
   sage-shell-edit:send-defun
   sage-shell-edit:send-defun-and-go
   sage-shell-edit:send-line-and-go
   sage-shell-edit:send-region
   sage-shell-edit:send-region-and-go)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'task-done)))))

(cl-loop
 for f in
 '(sage-shell-edit:send-line sage-shell-edit:send-line*)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'task-done))
     (sit-for 0.1)
     (emacspeak-sage-speak-output))))

;;}}}
;;{{{ sage-mode navigation:

(cl-loop
 for f in
 '(sage-shell-blocks:forward sage-shell-blocks:backward)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))

;;}}}
;;{{{ sage comint interaction:
(defadvice sage-shell:list-outputs (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (with-current-buffer (window-buffer (selected-window))
      (emacspeak-auditory-icon 'open-object)
      (emacspeak-speak-line))))

(defadvice sage-shell:delchar-or-maybe-eof (around emacspeak pre act comp)
  "Speak character you're deleting."
  (cond
   ((ems-interactive-p)
    (cond
     ((= (point) (point-max))
      (message "Sending EOF to comint process"))
     (t (dtk-tone-deletion)
        (emacspeak-speak-char t)))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice sage-shell:delete-output (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-line)))

(cl-loop
 for f in
 '(sage-shell:run-new-sage sage-shell:run-sage)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'task-done)
       (emacspeak-speak-mode-line)))))

(defadvice sage-shell:copy-previous-output-to-kill-ring
    (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (call-interactively #'emacspeak-speak-current-kill)))

(defadvice sage-shell:send-input (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (sit-for .01)
    (accept-process-output)
    (emacspeak-sage-speak-output)
    (emacspeak-auditory-icon 'close-object)))

;;}}}
;;{{{ sage sagetext:

(cl-loop
 for f in
 '(sage-shell-sagetex:compile-current-file
   sage-shell-sagetex:compile-file
   sage-shell-sagetex:error-mode
   sage-shell-sagetex:load-current-file
   sage-shell-sagetex:load-file
   sage-shell-sagetex:run-latex-and-load-current-file
   sage-shell-sagetex:run-latex-and-load-file
   sage-shell-sagetex:send-environment)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'task-done)
       (emacspeak-speak-mode-line)))))

;;}}}
;;{{{ Additional Interactive Commands:

(defun emacspeak-sage-describe-symbol (s)
  "Describe Sage symbol at point."
  (interactive
   (list
    (read-from-minibuffer
     "Sage Symbol: "
     (format "%s" (symbol-at-point)))))
  (cl-assert (eq  major-mode  'sage-shell:sage-mode) t "Not in a Sage buffer")
  (cl-assert   (sage-shell-edit:process-alist) t "No running Sage.")
;;; Take the first one for now:
  (with-current-buffer
      (process-buffer (car (cl-first  (sage-shell-edit:process-alist))))
    (sage-shell-help:describe-symbol s)))

;;}}}
;;{{{ Keybindings:
(cl-declaim (special sage-shell:sage-mode-map))
(when (and (bound-and-true-p sage-shell:sage-mode-map))
  (cl-loop
   for b in
   '(
     ("C-c h" emacspeak-sage-describe-symbol)
     ("C-C SPC" emacspeak-sage-speak-output)
     ("C-C m" emacspeak-maths-enter-guess))
   do
   (emacspeak-keymap-update sage-shell:sage-mode-map b)))

;;}}}
(provide 'emacspeak-sage)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

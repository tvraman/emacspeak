;;; emacspeak-python.el --- Speech enable Python development environment  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description: Auditory interface to python mode
;;; Keywords: Emacspeak, Speak, Spoken Output, python
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (c) 1995 -- 2021, T. V. Raman
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ Introduction

;;; Commentary:

;;; This speech-enables python-mode bundled with Emacs

;;; Code:

;;}}}
;;{{{  Required modules
(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

(require 'python "python" 'no-error)
;;}}}
;;{{{ interactive programming

(defadvice python-check (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)))
(cl-loop
 for f in
 '(
   python-shell-send-region python-shell-send-defun
   python-shell-send-file   python-shell-send-buffer
   python-shell-send-string python-shell-send-string-no-output)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak"
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'task-done)))))

;;}}}
;;{{{  whitespace management and indentation
(defadvice python-indent-dedent-line (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'right)))

(defadvice  python-indent-dedent-line-backspace (around emacspeak pre act comp)
  "Speak character you're deleting."
  (cond
   ((ems-interactive-p)
    (let ((ws (= 32 (char-syntax (preceding-char)))))
      (dtk-tone 500 100 'force)
      (unless ws (emacspeak-speak-this-char (preceding-char)))
      ad-do-it
      (when ws (dtk-notify-speak (format "Indent %s " (current-column))))))
   (t ad-do-it))
  ad-return-value)

(defadvice python-fill-paragraph (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'fill-object)))

(defadvice python-indent-shift-left (after emacspeak pre act comp)
  "Speak number of lines that were shifted"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'left)
    (dtk-speak
     (format "Left shifted block  containing %s lines"
             (count-lines  (region-beginning)
                           (region-end))))))
(defadvice python-indent-shift-right (after emacspeak pre act comp)
  "Speak number of lines that were shifted"
  (when (ems-interactive-p)
    (dtk-speak
     (format "Right shifted block  containing %s lines"
             (count-lines  (region-beginning)
                           (region-end))))))
(defadvice python-indent-region (after emacspeak pre act comp)
  "Speak number of lines that were shifted"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'right)
    (dtk-speak
     (format "Indented region   containing %s lines"
             (count-lines  (region-beginning)
                           (region-end))))))

;;}}}
;;{{{  buffer navigation
(defadvice python-mark-defun (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (message "Marked function containing %s lines"
             (count-lines (point) (mark 'force)))))

(cl-loop
 for f in
 '(
   python-nav-up-list python-nav-if-name-main python-nav-forward-statement
   python-nav-forward-sexp-safe python-nav-forward-sexp python-nav-forward-defun
   python-nav-forward-block python-nav-end-of-statement python-nav-end-of-defun
   python-nav-end-of-block python-nav-beginning-of-statement python-nav-beginning-of-block
   python-nav-backward-up-list python-nav-backward-statement python-nav-backward-sexp-safe
   python-nav-backward-sexp python-nav-backward-defun python-nav-backward-block
   )
 do
 (eval
  `(defadvice  ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'paragraph)))))

;;}}}
(provide 'emacspeak-python)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

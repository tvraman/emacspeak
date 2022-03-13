;;; emacspeak-metapost.el --- speech-enable metapost mode  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak module for speech-enabling
;;; metapost mode
;;; Keywords: Emacspeak, metapost
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
;;;  $Revision: 4074 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1995 -- 2021, T. V. Raman
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

;;{{{ required modules

(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction:

;;; Commentary:
;;; Speech-enables metapost mode.
;;; metapost is a powerful drawing package
;;; typically installed as mpost by modern TeX
;;; installations.

;;}}}
;;{{{  completion 

(defadvice meta-complete-symbol (around emacspeak pre act comp)
  "Say what you completed."
  (let ((prior (save-excursion (skip-syntax-backward "^ >") (point)))
        (dtk-stop-immediately dtk-stop-immediately))
    (when dtk-stop-immediately (dtk-stop))
    ad-do-it
    (when (> (point) prior)
      (setq dtk-stop-immediately nil)
      (tts-with-punctuations 'all
                             (dtk-speak (buffer-substring prior (point)))))
    ad-return-value))

;;}}}
;;{{{ indentation

(defadvice meta-indent-line (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)))

(defadvice meta-fill-paragraph (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'fill-object)
    (message "Filled current paragraph")))

;;}}}
;;{{{  navigation 
(defadvice  meta-beginning-of-defun (after emacspeak pre act comp)
  "Speak the line."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice  meta-end-of-defun (after emacspeak pre act comp)
  "Speak the line."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

;;}}}
;;{{{  commenting etc

(defadvice meta-comment-region (after emacspeak pre act comp)
  "Speak."
  (when (ems-interactive-p)
    (let ((prefix-arg (ad-get-arg 2)))
      (message "%s region containing %s lines"
               (if (and prefix-arg
                        (< prefix-arg 0))
                   "Uncommented"
                 "Commented")
               (count-lines (point) (mark 'force))))))

(defadvice meta-comment-defun (after emacspeak pre act comp)
  "Speak."
  (when (ems-interactive-p)
    (let ((prefix-arg (ad-get-arg 2)))
      (message "%s environment containing %s lines"
               (if  prefix-arg
                   "Uncommented"
                 "Commented")
               (count-lines (point) (mark 'force))))))

(defadvice meta-uncomment-defun (after emacspeak pre act comp)
  "Speak."
  (when (ems-interactive-p)
    (message "Uncommented environment containing %s lines"
             (count-lines (point) (mark 'force)))))

(defadvice meta-uncomment-region (after emacspeak pre act comp)
  "Speak."
  (when (ems-interactive-p)
    (message "Uncommented  region containing %s lines"
             (count-lines (point) (mark 'force)))))

(defadvice meta-indent-region (after emacspeak pre act comp)
  "Speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'fill-object)
    (message "Indented  region containing %s lines"
             (count-lines (point) (mark 'force)))))

(defadvice meta-indent-buffer (after emacspeak pre act comp)
  "Speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'fill-object)
    (message "Indented  buffer containing %s lines"
             (count-lines (point-min) (point-max 'force)))))

(defadvice meta-mark-defun (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (message "Marked function containing %s lines"
             (count-lines (point)
                          (mark 'force)))))

(defadvice meta-indent-defun (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'fill-object)
    (message "Indented current defun. ")))

;;}}}
(provide 'emacspeak-metapost)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

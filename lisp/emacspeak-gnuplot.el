;;; emacspeak-gnuplot.el --- speech-enable gnuplot mode  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak extension to speech-enable
;;; gnuplot mode
;;; Keywords: Emacspeak, WWW interaction
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

;;; Copyright (C) 1995 -- 2021, T. V. Raman<tv.raman.tv@gmail.com>
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

;;; This module speech-enables gnuplot-mode
;;; an Emacs add-on that enables fluent interaction with
;;; gnuplot.
;;; Use gnuplot to generate plots of mathematical functions
;;; for inclusion in documents.

;;; Code:

;;}}}

;;{{{ advice interactive commands

(defadvice gnuplot-send-region-to-gnuplot (after emacspeak
                                                 pre act
                                                 comp)
  "Speak status."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-other-window 1)))

(defadvice gnuplot-send-line-to-gnuplot (after emacspeak
                                               pre act
                                               comp)
  "Speak status."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-other-window 1)))

(defadvice gnuplot-send-line-and-forward (after emacspeak
                                                pre act
                                                comp)
  "Speak status."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-other-window 1)))

(defadvice gnuplot-send-buffer-to-gnuplot (after emacspeak
                                                 pre act
                                                 comp)
  "Speak status."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-other-window 1)))
(defadvice gnuplot-send-file-to-gnuplot (after emacspeak
                                               pre act
                                               comp)
  "Speak status."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-other-window 1)))

(defadvice gnuplot-delchar-or-maybe-eof (around emacspeak pre act comp)
  "Speak character you're deleting."
  (cond
   ((ems-interactive-p)
    (cond
     ((= (point) (point-max))
      (message "Sending EOF to comint process"))
     (t (dtk-tone 500 100 'force)
        (emacspeak-speak-char t)))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice gnuplot-kill-gnuplot-buffer (after emacspeak pre
                                              act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice gnuplot-show-gnuplot-buffer (after emacspeak pre
                                              act comp)
  "Speak status."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defadvice gnuplot-complete-keyword (around emacspeak pre act comp)
  "Say what you completed."
  (let ((prior (save-excursion (skip-syntax-backward "^ >") (point)))
        (dtk-stop-immediately dtk-stop-immediately))
    (when dtk-stop-immediately (dtk-stop))
    ad-do-it
    (when (> (point) prior)
      (setq dtk-stop-immediately nil)
      (dtk-speak (buffer-substring prior (point))))
    ad-return-value))

(defadvice gnuplot-indent-line (after emacspeak pre act
                                      comp)
  "Speak line we idnented."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice gnuplot-negate-option (after emacspeak pre act comp)
  "Speak line we negated."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(add-hook 'gnuplot-mode-hook
           #'(lambda nil
             (dtk-set-punctuations 'all)))

;;}}}
(provide 'emacspeak-gnuplot)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

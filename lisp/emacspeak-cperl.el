;;; emacspeak-cperl.el --- Speech enable CPerl Mode  -*- lexical-binding: t; -*- 
;;; $Id$
;;; $Author: tv.raman.tv $ 
;;; DescriptionEmacspeak extensions for CPerl mode
;;; Keywords:emacspeak, audio interface to emacs CPerl
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com 
;;; A speech interface to Emacs |
;;; $Date: 2007-09-01 15:30:13 -0700 (Sat, 01 Sep 2007) $ |
;;;  $Revision: 4532 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2021, T. V. Raman 
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}

;;{{{ required modules 

(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; Provide additional advice to CPerl mode 

;;; Code:

;;}}}
;;{{{ voice locking:

;;; first pull in emacspeak-perl for voice lock definitions 
(require 'emacspeak-perl)

;;}}}
;;{{{  Advice electric insertion to talk:

(defadvice cperl-electric-backspace (around emacspeak pre act comp)
  "Speak character you're deleting."
  (cond
   ((ems-interactive-p)
    (dtk-tone 500 100 'force)
    (emacspeak-speak-this-char (preceding-char))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice cperl-linefeed (around emacspeak pre act comp)
  "Speak the previous line if line echo is on. 
  See command \\[emacspeak-toggle-line-echo].
Otherwise cue user to the line just created. "
  (cl-declare (special emacspeak-line-echo))
  (cond
   ((ems-interactive-p)
    (cond
     (emacspeak-line-echo (emacspeak-speak-line))
     (t
      (dtk-speak-using-voice voice-annotate
                             (format
                              "indent %s"
                              (current-column)))
      (dtk-force)))))
  ad-do-it
  ad-return-value)

(defadvice cperl-indent-exp  (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'fill-object)
    (message "Indented current s expression ")))

;;}}}
;;{{{ Advice info to talk:

(defadvice cperl-info-on-current-command (after emacspeak pre act comp)
  "Speak the displayed info"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'help)
    (message "Displayed info in other window")))

(defadvice cperl-info-on-command (after emacspeak
                                        pre act
                                        comp)
  "Speak the displayed info"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'help)
    (message "Displayed help in other window.")))

;;}}}
;;{{{ structured editing

(defadvice cperl-invert-if-unless (after emacspeak pre act
                                         comp)
  "Speak updated line"
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice cperl-comment-region (after emacspeak pre act comp)
  "Speak."
  (when (ems-interactive-p)
    (let ((prefix-arg (ad-get-arg 2)))
      (message "%s region containing %s lines"
               (if (and prefix-arg
                        (< prefix-arg 0))
                   "Uncommented"
                 "Commented")
               (count-lines (point) (mark))))))

(defadvice cperl-uncomment-region (after emacspeak pre act comp)
  "Speak."
  (when (ems-interactive-p)
    (let ((prefix-arg (ad-get-arg 2)))
      (message "%s region containing %s lines"
               (if (and prefix-arg
                        (< prefix-arg 0))
                   "Commented"
                 "Uncommented")
               (count-lines (point) (mark))))))

(defadvice cperl-indent-command (after emacspeak pre act
                                       comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice cperl-indent-region (after emacspeak pre act
                                      comp)
  "speak when done"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'fill-object)
    (message "Filled region containing %s lines"
             (count-lines (region-beginning)
                          (region-end)))))
(defadvice cperl-fill-paragraph (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'fill-object)
    (message "Filled current paragraph")))
;;}}}
;;{{{  misc

(defadvice cperl-switch-to-doc-buffer (after emacspeak pre
                                             act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'open-object)))

(defadvice cperl-find-bad-style (after emacspeak pre act
                                       comp)
  "speak when done."
  (when (ems-interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'task-done)))
;;}}}
;;{{{ set up hooks 

(add-hook 'cperl-mode-hook
          #'(lambda ()
              (dtk-set-punctuations 'all)
              (or dtk-split-caps
                  (dtk-toggle-split-caps))
              (or emacspeak-audio-indentation
                  (emacspeak-toggle-audio-indentation))))

;;}}}
(provide  'emacspeak-cperl)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}

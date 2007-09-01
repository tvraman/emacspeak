;;; emacspeak-cperl.el --- Speech enable CPerl Mode 
;;; $Id$
;;; $Author$ 
;;; DescriptionEmacspeak extensions for CPerl mode
;;; Keywords:emacspeak, audio interface to emacs CPerl
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision: 4532 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, T. V. Raman 
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
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}

;;{{{ required modules 

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

(defvar emacspeak-cperl-electric-insertion-commands-to-advice
  '(cperl-electric-lbrace
    cperl-electric-paren
    cperl-electric-rparen
    cperl-electric-semi
    cperl-electric-terminator)
  "Electric commands from CPerl to advice")

(loop for e in emacspeak-cperl-electric-insertion-commands-to-advice
      do
      (eval
       `(defadvice ,e (after emacspeak pre act comp)
          "Speak what you inserted.
Cue electric insertion with a tone."
          (when (interactive-p)
            (let ((emacspeak-speak-messages nil))
              (emacspeak-speak-this-char last-input-char)
              (dtk-tone 800 50 t))))))

(defadvice cperl-electric-backspace (around emacspeak pre act)
  "Speak character you're deleting."
  (cond
   ((interactive-p )
    (dtk-tone 500 30 'force)
    (emacspeak-speak-this-char (preceding-char ))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice cperl-linefeed (around emacspeak pre act)
  "Speak the previous line if line echo is on. 
  See command \\[emacspeak-toggle-line-echo].
Otherwise cue user to the line just created. "
  (declare (special emacspeak-line-echo ))
  (cond
   ((interactive-p)
    (cond
     (emacspeak-line-echo 
      (emacspeak-speak-line )
      ad-do-it)
     (t ad-do-it       
        (dtk-speak-using-voice voice-annotate
                               (format
                                "indent %s"
                                (current-column)))
        (dtk-force))))
   (t ad-do-it))
  ad-return-value)

(defadvice cperl-indent-exp  (after emacspeak pre act)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'fill-object )
    (message "Indented current s expression ")))

;;}}}
;;{{{ Advice info to talk:

(defadvice cperl-info-on-current-command (after emacspeak pre act comp)
  "Speak the displayed info"
  (when (interactive-p)
    (emacspeak-auditory-icon 'help)
    (message "Displayed info in other window")))

(defadvice cperl-info-on-command (after emacspeak
                                        pre act
                                        comp)
  "Speak the displayed info"
  (when (interactive-p)
    (emacspeak-auditory-icon 'help)
    (message "Displayed help in other window.")))

;;}}}
;;{{{ structured editing

(defadvice cperl-invert-if-unless (after emacspeak pre act
                                         comp)
  "Speak updated line"
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice cperl-comment-region (after emacspeak pre act )
  "Provide spoken feedback."
  (when (interactive-p)
    (let ((prefix-arg (ad-get-arg 2)))
      (message "%s region containing %s lines"
               (if (and prefix-arg
                        (< prefix-arg 0))
                   "Uncommented"
                 "Commented")
               (count-lines (point) (mark))))))

(defadvice cperl-uncomment-region (after emacspeak pre act )
  "Provide spoken feedback."
  (when (interactive-p)
    (let ((prefix-arg (ad-get-arg 2)))
      (message "%s region containing %s lines"
               (if (and prefix-arg
                        (< prefix-arg 0))
                   "Commented"
                 "Uncommented")
               (count-lines (point) (mark))))))

(defadvice cperl-indent-command (after emacspeak pre act
                                       comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice cperl-indent-region (after emacspeak pre act
                                      comp)
  "Provide auditory feedback when done"
  (when (interactive-p)
    (emacspeak-auditory-icon 'fill-object)
    (message "Filled region containing %s lines"
             (count-lines (region-beginning)
                          (region-end)))))
(defadvice cperl-fill-paragraph (after emacspeak pre act)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'fill-object )
    (message "Filled current paragraph")))
;;}}}
;;{{{  misc

(defadvice cperl-switch-to-doc-buffer (after emacspeak pre
                                             act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'open-object)))

(defadvice cperl-find-bad-style (after emacspeak pre act
                                       comp)
  "Provide auditory feedback when done."
  (when (interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'task-done)))
;;}}}
;;{{{ set up hooks 

(add-hook 'cperl-mode-hook
          (function (lambda ()
                      (voice-lock-mode 1)
                      (dtk-set-punctuations 'all)
                      (or dtk-split-caps
                          (dtk-toggle-split-caps))
                      (or emacspeak-audio-indentation
                          (emacspeak-toggle-audio-indentation))
                      (emacspeak-dtk-sync))))

;;}}}
(provide  'emacspeak-cperl)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

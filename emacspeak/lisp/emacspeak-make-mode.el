;;; emacspeak-make-mode.el --- Speech enable make-mode
;;; $Id$
;;; $Author$ 
;;; Description:  Emacspeak extension to speech enable make-mode
;;; Keywords: Emacspeak, Make
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision$ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2004, T. V. Raman 
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  required modules

(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; This module speech enables make-mode

;;; Code:

;;}}}
;;{{{ advice

(defadvice makefile-next-dependency (after emacspeak pre act
                                           comp)
  "Speak line we moved to"
  (when (interactive-p)
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'large-movement))))

(defadvice makefile-browser-next-line (after emacspeak pre act
					     comp)
  "Speak line we moved to"
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice makefile-browser-previous-line (after emacspeak pre act
						 comp)
  "Speak line we moved to"
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice makefile-previous-dependency (after emacspeak pre act comp)
  "Speak line we moved to"
  (when (interactive-p)
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'large-movement))))

(defadvice makefile-complete (around emacspeak pre act comp)
  "Speak what we completed"
  (cond
   ((interactive-p)
    (let ((orig (point)))
      ad-do-it
      (emacspeak-speak-region orig (point))))
   (t ad-do-it))
  ad-return-value)

(defadvice makefile-backslash-region (after emacspeak pre
                                            act comp)
  "Speak how many lines we backslashed"
  (when (interactive-p)
    (message "Backslashed region containing %s lines"
	     (count-lines (region-beginning)
			  (region-end)))
    (emacspeak-auditory-icon 'select-object)))

(defadvice makefile-browser-quit (after emacspeak pre act
                                        comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'close-object)))

(defadvice makefile-switch-to-browser (after emacspeak pre
                                             act comp)
  "Provide status information"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice makefile-browser-toggle (around emacspeak pre act comp)
  "Speak what happened"
  (cond
   ((interactive-p)
    (let  ((this-line (max
                       (count-lines (point-min) (point))
                       1))
           (state nil))
      ad-do-it
      (setq state
            (makefile-browser-get-state-for-line this-line))
      (emacspeak-auditory-icon
       (if state 'on 'off))
      (emacspeak-speak-line)))
   (t ad-do-it))
  ad-return-value)
    
(defadvice makefile-browser-insert-selection (after
                                              emacspeak
                                              pre act comp)
  "Provide status message"
  (when (interactive-p)
    (message
     "Inserted selections into client  %s"
     (buffer-name makefile-browser-client))))

;;}}}
;;{{{ setup mode hook:

(add-hook 'makefile-mode-hook
          (function (lambda ()
                      (declare (special dtk-split-caps))
                      (voice-lock-mode 1)
                      (dtk-set-punctuations 'all)
                      (or dtk-split-caps
                          (dtk-toggle-split-caps))
                      (or emacspeak-audio-indentation
                          (emacspeak-toggle-audio-indentation))
                      (emacspeak-dtk-sync))))

;;}}}
(provide 'emacspeak-make-mode)

;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

;;; emacspeak-compile.el --- Speech enable  navigation of  compile errors, grep matches  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $ 
;;; Description:  Emacspeak extensions to  the compile package 
;;; Keywords: Emacspeak compile
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;{{{  Introduction:

;;; Commentary:

;;; This module makes compiling code from inside Emacs speech friendly.
;;; It is an example of how a little amount of code can make Emacspeak even better.
;;; Code:

;;}}}
;;{{{ Required modules
(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Personalities  
(voice-setup-add-map
 '(
   (compilation-line-number voice-smoothen)
   (compilation-column-number voice-smoothen)
   (compilation-info voice-lighten)
   (compilation-error voice-animate-extra)
   (compilation-warning voice-animate)
   (compilation-mode-line-exit voice-animate)
   (compilation-mode-line-fail voice-brighten)
   (compilation-mode-line-run voice-annotate)))

;;}}}
;;{{{  functions

(defun emacspeak-compilation-speak-error ()
  "Speech feedback about the compilation error. "
  (interactive)
  (let ((dtk-stop-immediately nil)
        (emacspeak-show-point t))
    (emacspeak-speak-line)))

;;}}}
;;{{{  advice  interactive commands
(cl-loop for f in 
         '(
           next-error previous-error
           compilation-next-file compilation-previous-file
           compile-goto-error compile-mouse-goto-error
           )
         do
         (eval
          `(defadvice ,f (after  emacspeak pre act comp)
             "Speak the line containing the error. "
             (when (ems-interactive-p)
               (dtk-stop)
               (emacspeak-auditory-icon 'large-movement)
               (emacspeak-compilation-speak-error)))))

(cl-loop for f in 
         '(
           compilation-next-error
           compilation-previous-error
           next-error-no-select
           previous-error-no-select)
         do
         (eval
          `(defadvice ,f (after emacspeak pre act comp)
             "Speak."
             (when (ems-interactive-p)
               (emacspeak-speak-line)
               (emacspeak-auditory-icon 'select-object)))))
;;}}}
;;{{{ advise process filter and sentinels

(defadvice compile (after emacspeak pre act comp)
  "provide auditory confirmation"
  (when (ems-interactive-p)
    (message "Launched compilation")
    (emacspeak-auditory-icon 'task-done)))

(defadvice  compilation-sentinel (after emacspeak pre act comp)
  "speak"
  (emacspeak-auditory-icon 'task-done)
  (message "process %s %s"
           (process-name  (ad-get-arg 0))
           (ad-get-arg 1)))

;;}}}
(provide 'emacspeak-compile)

;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}

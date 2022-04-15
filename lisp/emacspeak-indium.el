;;; emacspeak-indium.el --- Speech-enable INDIUM, A Javascript IDE -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable INDIUM An Emacs Interface to indium
;;; Keywords: Emacspeak,  Audio Desktop indium
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
;;; Copyright (C) 1995 -- 2007, 2011, T. V. Raman
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
;;; MERCHANTABILITY or FITNINDIUM FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; INDIUM ==  Javascript IDE 
;;; This module speech-enables Indium.

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
   (indium-keyword-face voice-animate)
   (indium-button-face  voice-bolden-medium)
   (indium-header-face voice-bolden)
   (indium-repl-prompt-face  voice-annotate)
   (indium-repl-stdout-face voice-monotone-extra)
   (indium-repl-error-face  voice-animate-extra)
   (indium-link-face voice-bolden)
   (indium-highlight-face voice-animate)
   (indium-breakpoint-face voice-lighten)
   (indium-frame-url-face  voice-animate)
   (indium-litable-face voice-lighten)))

;;}}}
;;{{{ Advice indium-backend.el:

(defadvice indium-quit (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(cl-loop
 for f in
 '(indium-reload indium-reconnect)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'task-done)))))

;;}}}
;;{{{ Advice indium-chrome.el

(defadvice indium-connect-to-chrome (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)))

;;}}}
;;{{{ Advice indium-debugger.el

'(indium-debugger-eval-last-node
  indium-debugger-evaluate
  indium-debugger-here
  indium-debugger-inspect-last-node
  indium-debugger-locals
  indium-debugger-locals-maybe-refresh
  
  
  indium-debugger-resume
  indium-debugger-step-into
  indium-debugger-step-out
  indium-debugger-step-over)

;;}}}
;;{{{ Advice indium-inspector.el

;;}}}
;;{{{ Advice indium-interaction.el

;;}}}
;;{{{ Advice indium-nodejs.el

;;}}}
;;{{{ Advice indium-render.el

;;}}}
;;{{{ Advice indium-repl.el

(defadvice indium-repl-return (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (save-excursion
      (forward-line -1)
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'close-object))))

(cl-loop
 for f in 
 '(indium-repl-next-input indium-repl-previous-input)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))

;;}}}
;;{{{ Advice indium-scratch.el

;;}}}
;;{{{ Advice indium-webkit.el

;;}}}

(provide 'emacspeak-indium)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

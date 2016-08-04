;;; emacspeak-jade.el --- Speech-enable JADE
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable JADE An Emacs Interface to jade
;;; Keywords: Emacspeak,  Audio Desktop jade
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
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
;;; MERCHANTABILITY or FITNJADE FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; JADE ==  Javascript IDE 
;;; This module speech-enables Jade.


;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map Faces:
(voice-setup-add-map
 '(
 (jade-keyword-face voice-animate)
 (jade-button-face  voice-bolden-medium)
 (jade-header-face voice-smoothen)
 (jade-repl-prompt-face  voice-annotate)
 (jade-repl-stdout-face voice-monotone)
 (jade-repl-error-face  voice-animate-extra)
 (jade-link-face voice-bolden)
 (jade-highlight-face voice-animate)))


;;}}}
;;{{{ Advice jade-backend.el:

(defadvice jade-quit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
(emacspeak-speak-mode-line)))

(loop
 for f in
 '(jade-reload jade-reconnect)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'task-done)))))

;;}}}
;;{{{ Advice jade-chrome.el

;;}}}
;;{{{ Advice jade-debugger.el

;;}}}
;;{{{ Advice jade-inspector.el

;;}}}
;;{{{ Advice jade-interaction.el

;;}}}
;;{{{ Advice jade-nodejs.el

;;}}}
;;{{{ Advice jade-render.el

;;}}}
;;{{{ Advice jade-repl.el

;;}}}
;;{{{ Advice jade-scratch.el

;;}}}
;;{{{ Advice jade-webkit.el

;;}}}

(provide 'emacspeak-jade)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

;;; emacspeak-jawbreaker.el --- Talk to Firefox/JawBreaker  via MozRepl
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Play JawBreaker game from Emacs in Firefox
;;; Keywords: Emacspeak,  Audio Desktop Firefox, Piglets 
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2008-03-19 07:02:19 -0700 (Wed, 19 Mar 2008) $ |
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:

;;; MozRepl provides a read-eval-print loop into Firefox
;;;  Module emacspeak-moz provides convenient functions for driving MozRepl
;;; See http://repo.hyperstruct.net/mozlab
;;; Using that module, you can connect two large pigs ---
;;; Emacs and Firefox  via a socket ---
;;; the result as you can expect is to produce piglets.
;;; emacspeak-jawbreaker is a piglet that enables one to play 
;;; The JawBreaker game from within Emacspeak.
;;; I run Firefox headless using the etc/firebox script
;;; And I have Fire Vox installed to provide the Firefox side of the spoken output.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'derived)
(require 'emacspeak-piglets)

;;}}}
;;{{{ Constants

(defvar emacspeak-jawbreaker-url
  "http://www.minijuegosgratis.com/juegos/jawbreaker/jawbreaker.htm"
  "URL for game page.")

;;}}}
;;{{{ Define our mode:

(define-derived-mode emacspeak-jawbreaker-mode emacspeak-piglets-mode
  "JawBreaker Interaction"
  "Major mode for JawBreaker interaction.
Launches the game, and sends keypresses from the special buffer 
to the running game. ")

;;}}}
;;{{{ Interactive Commands 

(defvar emacspeak-jawbreaker-buffer "*Jaw Breaker Interaction*"
  "Buffer where we play JawBreaker.")
;;;###autoload
(defun emacspeak-jawbreaker ()
  "Opens JawBreaker game in Firefox."
  (interactive)
  (declare (special emacspeak-jawbreaker-url
                    emacspeak-jawbreaker-buffer))
  (comint-send-string
   (inferior-moz-process)
   (format "window.location.href='%s'\n"
           emacspeak-jawbreaker-url))
  (save-excursion
    (set-buffer (get-buffer-create emacspeak-jawbreaker-buffer))
    (erase-buffer)
    (setq buffer-undo-list t)
    (emacspeak-jawbreaker-mode)
    (switch-to-buffer emacspeak-jawbreaker-buffer)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'open-object)))

;;}}}
(provide 'emacspeak-jawbreaker)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

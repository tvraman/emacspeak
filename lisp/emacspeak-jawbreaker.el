;;; emacspeak-jawbreaker.el.el --- Talk to Firefox/JawBreaker  via MozRepl
;;; $Id: emacspeak-moz.el 5290 2007-09-16 21:52:56Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Play JawBreaker game from Emacs in Firefox
;;; Keywords: Emacspeak,  Audio Desktop Firefox, Piglets 
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-09-16 14:52:56 -0700 (Sun, 16 Sep 2007) $ |
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
(require 'emacspeak-moz)

;;}}}
;;{{{ Constants

(defvar emacspeak-jawbreaker-url
  "http://www.minijuegosgratis.com/juegos/jawbreaker/jawbreaker.htm"
  "URL for game page.")

;;}}}
;;{{{ Define our mode:

(define-derived-mode emacspeak-jawbreaker-mode inferior-moz-mode
  "JawBreaker Interaction"
  "Major mode for JawBreaker interaction.
Launches the game, and sends keypresses from the special buffer 
to the running game. ")

;;}}}
;;{{{ Interactive Commands And Keybindings:

(defun emacspeak-jawbreaker-keypress (c)
  "Send keypress to jawbreaker."
  (interactive "%c")
  (comint-send-string (inferior-moz-process) 
   (format
    "b=repl.adom.body(); repl.adom.keyPress(b,'%c')" c)))


(defun emacspeak-jawbreaker-key ()
  "Send keypress to jawbreaker."
  (interactive)
  (emacspeak-jawbreaker-keypress last-input-char))
   

(defun emacspeak-jawbreaker-silence()
  "Stop speech."
  (interactive)
  (comint-send-string (inferior-moz-process)
                      "CLC_SR_StopSpeaking()"))

(loop for key in
      '("a" "b" "e" "t"
        "j" "k" "h" "l"
        "r" "c" " " "s" "n" "?"
           )
      do
      (define-key emacspeak-jawbreaker-mode-map key 'emacspeak-jawbreaker-key))
(define-key emacspeak-jawbreaker-mode-map "q" 'emacspeak-jawbreaker-silence)

;;}}}
(provide 'emacspeak-jawbreaker)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

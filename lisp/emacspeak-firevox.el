;;; emacspeak-firevox.el.el --- FireVox Piglet
;;; $Id$
;;; $Author$
;;; Description:  Play Firevox game from Emacs in Firefox
;;; Keywords: Emacspeak,  Audio Desktop Firefox, Piglets 
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:

;;; MozRepl provides a read-eval-print loop into Firefox
;;;  Module emacspeak-moz provides convenient functions for driving MozRepl
;;; See http://repo.hyperstruct.net/mozlab
;;; Using that module, you can connect two large pigs ---
;;; Emacs and Firefox  via a socket ---
;;; the result as you can expect is to produce piglets.
;;; emacspeak-firevox is a piglet that enables one to play 
;;; The Firevox game from within Emacspeak.
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

(defvar emacspeak-firevox-url
  "http://www.minijuegosgratis.com/juegos/firevox/firevox.htm"
  "URL for game page.")

;;}}}
;;{{{ Define our mode:

(define-derived-mode emacspeak-firevox-mode emacspeak-piglets-mode
  "Firevox Interaction"
  "Major mode for Firevox interaction.
Invokes FireVox commands from Emacs.")

;;}}}
;;{{{ Interactive Commands And Keybindings:

(defvar emacspeak-firevox-buffer "*Fire Vox Interaction*"
  "Buffer where we talk to  Firevox.")


;;; Interactive commands:

;;;###autoload
(defun emacspeak-firevox-read-next ()
  "Read next item on page."
  (interactive)
  (comint-send-string
   (inferior-moz-process)
   "CLC_SR_ReadContent(1)\n"))

;;;###autoload
(defun emacspeak-firevox-read-previous ()
  "Read next item on page."
  (interactive)
  (comint-send-string
   (inferior-moz-process)
   "CLC_SR_ReadContent(-1)\n"))

             
          
           ;;; think mute
(defun emacspeak-firevox-read-current ()
"Read current node."
(comint-send-string
 (inferior-moz-process)
 "CLC_SR_ReadCurrentAtomicObject()\n"))

;;;###autoload
(defun emacspeak-firevox-read-parent ()
          "Read parent node."
          (interactive)
          (comint-send-string
           (inferior-moz-process)
           "CLC_SR_SayParentTextContent()\n"))

(defun emacspeak-firevox-setup-keys ()
  "Set up FireVox keybindings."
  (declare (special emacspeak-firevox-mode-map))
  (loop for k in
        '(
          ("\C-n" emacspeak-firevox-read-next)
          ("\C-p" emacspeak-firevox-read-previous)
          ("\C-m" emacspeak-piglets-silence) ;;; think mute
          ("\C-@" emacspeak-firevox-read-current)
          ("\C-^" emacspeak-firevox-read-parent)
          )
        do
        (emacspeak-keymap-update  emacspeak-firevox-mode-map  k))
  )
(defun emacspeak-firevox ()
  "Creates FireVox interaction."
  (interactive)
  (declare (special emacspeak-firevox-buffer))
  
  (save-excursion
    (set-buffer (get-buffer-create emacspeak-firevox-buffer))
    (erase-buffer)
    (setq buffer-undo-list t)
    (emacspeak-firevox-mode)
    (switch-to-buffer emacspeak-firevox-buffer)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'open-object)))

;;}}}
(provide 'emacspeak-firevox)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

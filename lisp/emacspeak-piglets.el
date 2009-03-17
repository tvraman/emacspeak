;;; emacspeak-piglets.el.el --- Result of large pigs connecting over a socket
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Result of connecting Emacs and Firefox
;;; Keywords: Emacspeak,  Audio Desktop Firefox, Piglets 
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2008-07-02 16:44:27 -0700 (Wed, 02 Jul 2008) $ |
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
;;; Consequently, Emacs and Firefox connect over  a socket 4242
;;; See http://repo.hyperstruct.net/mozlab
;;; Using that module, you can connect two large pigs ---
;;; Emacs and Firefox  via a socket ---
;;; the result as you can expect is to produce piglets.

;;; This module provides the needed Emacs plumbing 
;;; To drive Firefox from Emacs.
;;; Install Firefox extension Fire-Vox to provide spoken output from the Firefox side.
;;; Use commands provided by emacspeak-moz.el
;;; to render the Firefox DOM using Emacs/W3 or Emacs/W3M

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

;;}}}
;;{{{ Define our mode:
(defcustom emacspeak-piglets-tts-rate 75
  "Speech rate for HTTP  TTS server."
  :type 'number
  :group 'emacspeak-piglets)

;;;###autoload
(defun emacspeak-piglets-tts-init ()
  "Start TTS  engine for Piglets."
  (interactive)
  (declare (special emacspeak-piglets-tts-rate
                    dtk-program emacspeak-servers-directory))
  (start-process
   "HTTP-TTS"
   "* HTTP TTS *"
   "python"
   (expand-file-name
    "python/HTTPSpeaker.py"
    emacspeak-servers-directory)
   dtk-program
   "2222"
   (format "%d" emacspeak-piglets-tts-rate)))
(define-derived-mode emacspeak-piglets-mode inferior-moz-mode
  "Piglets Interaction"
  "Major mode for Piglets interaction.
Keystrokes are sent to a connected Firefox."
  (progn
    (emacspeak-piglets-tts-init)
    (comint-send-string
     (inferior-moz-process)
     ";\n;\nrepl.setenv('printPrompt', false)\n")
    (emacspeak-piglets-forward-keys)))

;;}}}
;;{{{ Interactive Commands And Keybindings:

(defvar emacspeak-piglets-edit-commands
  (list 'emacspeak-self-insert-command
        'completion-separator-self-insert-autofilling
        'completion-separator-self-insert-command
        'self-insert-command
        'delete-char
        'backward-delete-char
        'backward-delete-char-untabify
        'completion-kill-region)
  "Editting commands that emacspeak should rebind in Piglets mode")

(defun emacspeak-piglets-forward-keys ()
  "Set up Piglets mode to forward keys to Firefox."
  (declare (special emacspeak-piglets-edit-commands
                    emacspeak-piglets-mode-map))
  (loop for edit-command in emacspeak-piglets-edit-commands
        do
        (let ((edit-keys (where-is-internal edit-command  emacspeak-piglets-mode-map)))
          (loop for key in edit-keys 
                do
                (define-key emacspeak-piglets-mode-map  key 'emacspeak-piglets-key)))))
;;;###autoload
(defun emacspeak-piglets-tab ()
  "Send TAB to Firefox."
  (interactive)
  (comint-send-string
   (inferior-moz-process) 
   (format "CLC_SR_StopSpeaking();repl.adom.keyPress(repl.adom.root(),'TAB')\n" )))

;;;###autoload
(defun emacspeak-piglets-enter ()
  "Send enter to Firefox."
  (interactive)
  (comint-send-string
   (inferior-moz-process) 
   (format "repl.adom.keyPress(repl.adom.target(),'ENTER')\n" )))
;;;###autoload
(defun emacspeak-piglets-keypress (c)
  "Send keypress to Firefox."
  (interactive "%c")
  (comint-send-string (inferior-moz-process) 
                      (format "CLC_SR_StopSpeaking();repl.adom.keyPress(repl.adom.target(),'%c', false, false, %s)\n"
                              c
                              (if (and (<= 65 c)
                                       (<= c 90))
                                  "true"
                                "false"))))

;;;###autoload
(defun emacspeak-piglets-key ()
  "Send last keypress to Firefox."
  (interactive)
  (declare (special last-input-char))
  (when (interactive-p)
    (emacspeak-piglets-silence))
  (emacspeak-piglets-keypress last-input-char))

;;;###autoload
(defun emacspeak-piglets-silence()
  "Stop speech output from FireVox."
  (interactive)
  (comint-send-string
   (inferior-moz-process)
   "CLC_SR_StopSpeaking() \n"))

;;}}}
(provide 'emacspeak-piglets)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

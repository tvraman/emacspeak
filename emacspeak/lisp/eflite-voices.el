;;; tts-eflite.el --- Emacspeak EFLITE 
;;; $Id$
;;; $Author$
;;; Description:  EFLITE Wizard for the emacspeak desktop
;;; Keywords: Emacspeak,  Audio Desktop EFLITE
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
;;;Copyright (C) 1995 -- 2003, T. V. Raman 
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

;;; defines interactive command tts-eflite that launches eflite 
;;; http://sf.net/projects/eflite
;;; as the tts server.
;;; When and if eflite becomes capable of voice locking,
;;; this file should provide code similar to  outloud-voices.el

;;}}}
;;{{{  Required modules

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'custom)

;;}}}
;;;###autoload
;;{{{  launch eflite 

(defun tts-eflite ()
  "Use eflite TTS server."
  (interactive)
  (let ((dtk-tcl "eflite"))
    (tts-restart)
    (message "This is Emacspeak! ")))

;;}}}

(provide 'tts-eflite)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}

;;; emacspeak-dictation.el --- Speech enable dictation -- Dictation Interface
;;; $Id$
;;; $Author$ 
;;; Description: Auditory interface to dictation
;;; Keywords: Emacspeak, Speak, Spoken Output, dictation
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

;;; Copyright (c) 1995 -- 2003, T. V. Raman
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

;;{{{  Required modules

(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction

;;; Commentary: 

;;; dictation.el is an emacs lisp package that interfaces
;;; emacs to a dictation system like IBM ViaVoice.
;;; This module provides auditory feedback to the emacspeak
;;; user while using dictation.
;;; Make sure to use a headphone for the output if you use
;;; emacspeak with dictation.

;;}}}
;;{{{  advice interactive commands 
(defadvice dictation-toggle (after emacspeak pre act comp)
  "Provide an auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'button)))

(defadvice dictation-filter (around emacspeak pre act comp)
  "Speak what you heard. "
  (let ((start nil)
        (dtk-stop-immediately nil))
    (set-buffer dictation-buffer)
    (setq start (point))
    ad-do-it
    (emacspeak-speak-region start (point))))
        
;;}}}
(provide 'emacspeak-dictation )
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

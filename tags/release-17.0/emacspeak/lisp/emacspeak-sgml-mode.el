;;; emacspeak-sgml-mode.el --- Speech enable SGML mode
;;; $Id$
;;; $Author$ 
;;; Description: Emacspeak extension for sgml mode
;;; Keywords:emacspeak, audio interface to emacs sgml 
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

;;;Copyright (C) 1995 -- 2002, T. V. Raman 
;;; Copyright (c) 1995 by T. V. Raman  
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

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-sounds)
(require 'emacspeak-speak)
(require 'emacspeak-fix-interactive)
(require 'voice-lock)
;;{{{  Introduction

;;; emacspeak extensions to sgml mode

;;}}}
;;{{{ advice interactive commands 

(defadvice sgml-skip-tag-forward (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice sgml-skip-tag-backward (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice sgml-slash (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-speak-this-char (preceding-char))))

(defadvice sgml-delete-tag (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)))

(defadvice sgml-name-char (around emacspeak pre act comp)
  "Speak the character you typed"
  (cond
   ((interactive-p)
    (let ((start (point)))
      (message "Type the char: ")
      ad-do-it
      (emacspeak-speak-region start (point))))
   (t ad-do-it))
  ad-return-value)

(defadvice sgml-tags-invisible (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'button)
    (dtk-speak  "Toggled display of tags")))

;;}}}
;;{{{ simple voice locking 

(voice-lock-set-major-mode-keywords 'sgml-mode
				    'sgml-voice-lock-keywords)

(defconst sgml-voice-lock-keywords-1
  '(("<\\([!?][a-z][-.a-z0-9]*\\)" 1 voice-lock-keyword-personality)
    ("<\\(/?[a-z][-.a-z0-9]*\\)" 1 voice-lock-function-name-personality)
    ("[&%][a-z][-.a-z0-9]*;?" . voice-lock-variable-name-personality)
    ("<! *--.*-- *>" . voice-lock-comment-personality)))

;; for voice-lock, but must be defvar'ed after
;; sgml-voice-lock-keywords-1  above
(defvar sgml-voice-lock-keywords sgml-voice-lock-keywords-1
  "*Rules for highlighting SGML code.  ")

;;}}}
(provide  'emacspeak-sgml-mode)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

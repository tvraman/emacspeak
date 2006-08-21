;;; emacspeak-perl.el --- Speech enable Perl Mode 
;;; $Id$
;;; $Author$ 
;;; DescriptionEmacspeak extensions for perl-mode
;;; Keywords:emacspeak, audio interface to emacs perl
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

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
(require 'voice-lock)

;;{{{  Introduction:

;;; Provide additional advice to perl-mode 

;;}}}
;;{{{ voice locking:
(defvar perl-voice-lock-keywords
  (list
   (cons (concat "[ \n\t{]*\\("
		 (mapconcat 'identity
			    '("if" "until" "while" "elsif" "else" "unless" "for"
			      "foreach" "continue" "exit" "die" "last" "goto" "next"
			      "redo" "return" "local" "my"  "exec")
			    "\\|")
		 "\\)[ \n\t;(]") 1)
   (mapconcat 'identity
	      '("#endif" "#else" "#ifdef" "#ifndef" "#if" "#include"
		"#define" "#undef")
	      "\\|")
   '("^[ \n\t]*sub[ \t]+\\([^ \t{]+\\)[ \t]*[{]" 1 voice-lock-function-name-personality)
   '("[ \n\t{]*\\(eval\\)[ \n\t(;]" 1 voice-lock-function-name-personality)
   '("\\(--- .* ---\\|=== .* ===\\)" . voice-lock-doc-string-personality)
   )
  "Additional expressions to highlight in Perl mode.")

(voice-lock-set-major-mode-keywords 'perl-mode 'perl-voice-lock-keywords)

;;}}}
;;{{{  Advice electric insertion to talk:

(defadvice electric-perl-terminator  (after emacspeak pre act comp )
  "Speak what you inserted."
  (when (interactive-p)
    (emacspeak-speak-this-char last-input-char)))

;;}}}
;;{{{  Program structure:

(defadvice mark-perl-function (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (message "Marked procedure")))

(defadvice perl-beginning-of-function (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice perl-end-of-function (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)))

;;}}}

(provide  'emacspeak-perl)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

;;; emacspeak-gnuplot.el --- speech-enable gnuplot mode
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak extension to speech-enable
;;; gnuplot mode
;;; Keywords: Emacspeak, WWW interaction
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

;;; Copyright (C) 1995 -- 2001, T. V. Raman<raman@cs.cornell.edu>
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

;;{{{ required modules

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'advice)
(require 'emacspeak-speak)
(require 'voice-lock)
(require 'emacspeak-fix-interactive)
(require 'emacspeak-sounds)

;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; This module speech-enables gnuplot-mode
;;; an Emacs add-on that enables fluent interaction with
;;; gnuplot.
;;; Use gnuplot to generate plots of mathematical functions
;;; for inclusion in documents.

;;; Code:

;;}}}

;;{{{ set up voice locking 

(voice-lock-set-major-mode-keywords 'gnuplot-mode
                                                      'gnuplot-voice-lock-keywords)

(defvar gnuplot-voice-lock-keywords nil
  "Voice lock keywords for gnuplot mode.")

(setq gnuplot-voice-lock-keywords
      (list
					; comments
	   '("#.*$" . voice-lock-comment-personality)
					; quoted things
	   ;'("['\"]\\([^'\"\n]*\\)['\"]"
	   ;  1 voice-lock-string-personality)
	   '("'[^'\n]*'?" . voice-lock-string-personality)
					; stuff in brackets, sugg. by <LB>
	   '("\\[\\([^]]+\\)\\]"
	     1 voice-lock-reference-personality)
					; variable/function definitions
	   '("\\(\\<[a-z]+[a-z_0-9()]*\\)[ \t]*="
	     1 voice-lock-variable-name-personality)
					; built-in function names
	   (cons
            (concat
		  "\\<\\("
		  "a\\(bs\\|cosh\?\\|rg\\|sinh\?\\|"
		  "tan\\(\\|\[2h\]\\)\\)\\|"
		  "bes\\(j\[01\]\\|y\[01\]\\)\\|"
		  "c\\(eil\\|o\\(lumn\\|sh\?\\)\\)\\|"
		  "e\\(rfc\?\\|xp\\)\\|floor\\|gamma\\|"
		  "i\\(beta\\|gamma\\|mag\\|"
		  "n\\(t\\|v\\(erf\\|norm\\)\\)\\)\\|"
		  "l\\(gamma\\|og\\(\\|10\\)\\)\\|"
		  "norm\\|r\\(and\\|eal\\)\\|"
		  "s\\(gn\\|inh\?\\|qrt\\)\\|"
		  "t\\(anh\?\\|m_\\(hour\\|m\\(day\\|in\\|on\\)\\|"
		  "sec\\|wday\\|y\\(day\\|ear\\)\\)\\)\\|"
		  "valid"
		  "\\)\\>")
		 'voice-lock-function-name-personality)
					; (s)plot -- also thing (s)plotted
	   '("\\<s?plot\\>" . voice-lock-keyword-personality)
	   '("\\<s?plot\\s-+\\([^'\" ]+\\)[) \n,\\\\]"
	     1 voice-lock-variable-name-personality)
					; other common commands
					; miscellaneous commands
	   (cons
            (concat "\\<\\("
			 "c\\(d\\|lear\\)\\|exit\\|fit\\|help\\|load\\|"
			 "p\\(ause\\|rint\\|wd\\)\\|quit\\|replot\\|"
			 "s\\(ave\\|et\\|how\\)"
			 "\\)\\>\\|!.*$")
		 'voice-lock-reference-personality))
      )

;;}}}
;;{{{ advice interactive commands

(defadvice gnuplot-send-region-to-gnuplot (after emacspeak
                                                 pre act
                                                 comp)
  "Speak status."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-other-window 1)))

(defadvice gnuplot-send-line-to-gnuplot (after emacspeak
                                                 pre act
                                                 comp)
  "Speak status."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-other-window 1)))

(defadvice gnuplot-send-line-and-forward (after emacspeak
                                                 pre act
                                                 comp)
  "Speak status."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-other-window 1)))

(defadvice gnuplot-send-buffer-to-gnuplot (after emacspeak
                                                 pre act
                                                 comp)
  "Speak status."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-other-window 1)))
(defadvice gnuplot-send-file-to-gnuplot (after emacspeak
                                                 pre act
                                                 comp)
  "Speak status."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-other-window 1)))

(defadvice gnuplot-delchar-or-maybe-eof (around emacspeak pre act)
  "Speak character you're deleting."
  (cond
   ((interactive-p )
    (cond
     ((= (point) (point-max))
    (message "Sending EOF to comint process"))
    (t (dtk-tone 500 30 'force)
    (and emacspeak-delete-char-speak-deleted-char
         (emacspeak-speak-char t))))
    ad-do-it
    (and emacspeak-delete-char-speak-current-char
         (emacspeak-speak-char t)))
   (t ad-do-it))
  ad-return-value)


(defadvice gnuplot-kill-gnuplot-buffer (after emacspeak pre
                                              act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice gnuplot-show-gnuplot-buffer (after emacspeak pre
                                              act comp)
  "Speak status."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defadvice gnuplot-complete-keyword (around emacspeak pre act)
  "Say what you completed."
  (let ((prior (point ))
        (dtk-stop-immediately dtk-stop-immediately))
    (when dtk-stop-immediately (dtk-stop))
    ad-do-it
    (when (> (point) prior)
      (setq dtk-stop-immediately nil)
      (dtk-speak (buffer-substring prior (point ))))
    ad-return-value))



(defadvice gnuplot-indent-line (after emacspeak pre act
                                      comp)
  "Speak line we idnented."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice gnuplot-negate-option (after emacspeak pre act comp)
  "Speak line we negated."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))


(add-hook 'gnuplot-mode-hook
          (function
           (lambda nil
             (dtk-set-punctuations "all")
             (voice-lock-mode t))))

;;}}}
(provide 'emacspeak-gnuplot)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

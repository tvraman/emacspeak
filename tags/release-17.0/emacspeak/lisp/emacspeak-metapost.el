;;; emacspeak-metapost.el --- speech-enable metapost mode
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak module for speech-enabling
;;; metapost mode
;;; Keywords: Emacspeak, metapost
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

;;; Copyright (C) 1999 T. V. Raman <raman@cs.cornell.edu>
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
(require 'voice-settings)
(require 'emacspeak-sounds)

;;}}}
;;{{{  Introduction:

;;; Commentary:
;;; Speech-enables metapost mode.
;;; metapost is a powerful drawing package
;;; typically installed as mpost by modern TeX
;;; installations.

;;}}}
;;{{{  voice locking 

(defvar emacspeak-metapost-voice-lock-keywords
  (let ((input-keywords
	 "\\(input\\|generate\\)")
	(begin-keywords
	 (concat "\\(begin\\(char\\|fig\\|graph\\|logochar\\)\\|"
		 "\\cmchar\\|dcchar\\|ecchar\\)"))
	(end-keywords
	 "\\(end\\(char\\|fig\\|graph\\)\\)")
	(macro-keywords-1
	 "\\(def\\|let\\|mode_def\\|vardef\\)")
	(macro-keywords-2
	 "\\(primarydef\\|secondarydef\\|tertiarydef\\)")
					;(make-regexp
					; '("expr" "suffix" "text" "primary" "secondary" "tertiary") t)
	(args-keywords
	 (concat "\\(expr\\|primary\\|s\\(econdary\\|uffix\\)\\|"
		 "te\\(rtiary\\|xt\\)\\)"))
					;(make-regexp
					; '("boolean" "color" "numeric" "pair" "path" "pen" "picture"
					;   "string" "transform" "newinternal") t)
	(type-keywords
	 (concat "\\(boolean\\|color\\|n\\(ewinternal\\|umeric\\)\\|"
		 "p\\(a\\(ir\\|th\\)\\|en\\|icture\\)\\|string\\|"
		 "transform\\)"))
					;(make-regexp
					; '("for" "forever" "forsuffixes" "endfor"
					;   "step" "until" "upto" "downto" "thru" "within"
					;   "iff" "if" "elseif" "else" "fi" "exitif" "exitunless"
					;   "let" "def" "vardef" "enddef" "mode_def"
					;   "true" "false" "known" "unknown" "and" "or" "not"
					;   "save" "interim" "inner" "outer" "relax"
					;   "begingroup" "endgroup" "expandafter" "scantokens"
					;   "generate" "input" "endinput" "end" "bye"
					;   "message" "errmessage" "errhelp" "special" "numspecial"
					;   "readstring" "readfrom" "write") t)
	(syntactic-keywords
	 (concat "\\(and\\|b\\(egingroup\\|ye\\)\\|"
		 "d\\(ef\\|ownto\\)\\|e\\(lse\\(\\|if\\)"
		 "\\|nd\\(\\|def\\|for\\|group\\|input\\)"
		 "\\|rr\\(help\\|message\\)"
		 "\\|x\\(it\\(if\\|unless\\)\\|pandafter\\)\\)\\|"
		 "f\\(alse\\|i\\|or\\(\\|ever\\|suffixes\\)\\)\\|"
		 "generate\\|i\\(ff?\\|n\\(ner\\|put\\|terim\\)\\)\\|"
		 "known\\|let\\|m\\(essage\\|ode_def\\)\\|"
		 "n\\(ot\\|umspecial\\)\\|o\\(r\\|uter\\)\\|"
		 "re\\(ad\\(from\\|string\\)\\|lax\\)\\|"
		 "s\\(ave\\|cantokens\\|pecial\\|tep\\)\\|"
		 "t\\(hru\\|rue\\)\\|"
		 "u\\(n\\(known\\|til\\)\\|pto\\)\\|"
		 "vardef\\|w\\(ithin\\|rite\\)\\)"))
	)
    (list
     ;; embedded TeX code in btex ... etex
     (cons (concat "\\(btex\\|verbatimtex\\)"
		   "[ \t]+\\(.*\\)[ \t]+"
		   "\\(etex\\)")
	   '((1 voice-lock-keyword-personality)
	     (2 voice-lock-string-personality)
	     (3 voice-lock-keyword-personality)))
     ;; unary macro definitions: def, vardef, let
     (cons (concat "\\<" macro-keywords-1 "\\>"
		   "[ \t]+\\(\\sw+\\|\\s_+\\|\\s.+\\)")
	   '((1 voice-lock-keyword-personality)
	     (2 voice-lock-function-name-personality)))
     ;; binary macro definitions: <leveldef> x operator y
     (cons (concat "\\<" macro-keywords-2 "\\>"
		   "[ \t]+\\(\\sw+\\)"
		   "[ \t]*\\(\\sw+\\|\\s.+\\)"
		   "[ \t]*\\(\\sw+\\)")
	   '((1 voice-lock-keyword-personality)
	     (2 voice-lock-variable-name-personality nil t)
	     (3 voice-lock-function-name-personality nil t)
	     (4 voice-lock-variable-name-personality nil t)))
     ;; variable declarations: numeric, pair, color, ...
     (cons (concat "\\<" type-keywords "\\>"
		   "\\([ \t]+\\(\\sw+\\)\\)*")
	   '((1 voice-lock-type-personality)
	     (voice-lock-match-meta-declaration-item-and-skip-to-next
	      (goto-char (match-end 1)) nil
	      (1 voice-lock-variable-name-personality nil t))))
     ;; argument declarations: expr, suffix, text, ...
     (cons (concat "\\<" args-keywords "\\>"
		   "\\([ \t]+\\(\\sw+\\|\\s_+\\)\\)*")
	   '((1 voice-lock-type-personality)
	     (voice-lock-match-meta-declaration-item-and-skip-to-next
	      (goto-char (match-end 1)) nil
	      (1 voice-lock-variable-name-personality nil t))))
     ;; special case of arguments: expr x of y
     (cons (concat "\\(expr\\)[ \t]+\\(\\sw+\\)"
		   "[ \t]+\\(of\\)[ \t]+\\(\\sw+\\)")
	   '((1 voice-lock-type-personality)
	     (2 voice-lock-variable-name-personality)
	     (3 voice-lock-keyword-personality nil t)
	     (4 voice-lock-variable-name-personality nil t)))
     ;; syntactic keywords
     (cons (concat "\\<" syntactic-keywords "\\>")
	   'voice-lock-keyword-personality)
     ;; beginchar, beginfig
     (cons (concat "\\<" begin-keywords "\\>")
	   'voice-lock-keyword-personality)
     ;; endchar, endfig
     (cons (concat "\\<" end-keywords "\\>")
	   'voice-lock-keyword-personality)
     ;; input, generate
     (cons (concat "\\<" input-keywords "\\>"
		   "[ \t]+\\(\\sw+\\)")
	   '((1 voice-lock-keyword-personality)
	     (2 'voice-lock-constant-personality)))
     ;; embedded Metafont/MetaPost code in comments
     (cons "|\\([^|]+\\)|" 
	   '(1 'voice-lock-constant-personality t))
     ))
  "Default expressions to highlight in Metafont or MetaPost mode.")

(defun voice-lock-match-meta-declaration-item-and-skip-to-next (limit)
  ;; Match and move over Metafont/MetaPost declaration item after point.
  ;;
  ;; The expected syntax of an item is either "word" or "symbol",
  ;; possibly ending with optional whitespace.  Everything following
  ;; the item (but belonging to it) is expected to by skipable by
  ;; `forward-sexp'.  The list of items is expected to be separated
  ;; by commas and terminated by semicolons or equals signs.
  ;;
  (if (looking-at "[ \t]*\\(\\sw+\\|\\s_+\\)")
      (save-match-data
        (condition-case nil
            (save-restriction
              ;; Restrict to end of line, currently guaranteed to be LIMIT.
              (narrow-to-region (point-min) limit)
              (goto-char (match-end 1))
              ;; Move over any item value, etc., to the next item.
              (while (not (looking-at "[ \t]*\\(\\(,\\)\\|;\\|=\\|$\\)"))
                (goto-char (or (scan-sexps (point) 1) (point-max))))
              (goto-char (match-end 2)))
          (error t)))))

;;; setup the keywords 
(voice-lock-set-major-mode-keywords 'metapost-mode
				    'emacspeak-metapost-voice-lock-keywords)

;;}}}
;;{{{  completion 

(defadvice meta-complete-symbol (around emacspeak pre act)
  "Say what you completed."
  (let ((prior (point ))
        (dtk-stop-immediately dtk-stop-immediately))
    (when dtk-stop-immediately (dtk-stop))
    ad-do-it
    (when (> (point) prior)
      (setq dtk-stop-immediately nil)
      (tts-with-punctuations "all"
                             (dtk-speak (buffer-substring prior (point )))))
    ad-return-value))

;;}}}
;;{{{ indentation

(defadvice meta-indent-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-line )))

(defadvice meta-fill-paragraph (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'fill-object )
    (message "Filled current paragraph")))

;;}}}
;;{{{  navigation 
(defadvice  meta-beginning-of-defun (after emacspeak pre act)
  "Speak the line."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice  meta-end-of-defun (after emacspeak pre act)
  "Speak the line."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

;;}}}
;;{{{  commenting etc

(defadvice meta-comment-region (after emacspeak pre act )
  "Provide spoken feedback."
  (when (interactive-p)
    (let ((prefix-arg (ad-get-arg 2)))
      (message "%s region containing %s lines"
               (if (and prefix-arg
                        (< prefix-arg 0))
                   "Uncommented"
                 "Commented")
               (count-lines (point) (mark 'force))))))

(defadvice meta-comment-defun (after emacspeak pre act )
  "Provide spoken feedback."
  (when (interactive-p)
    (let ((prefix-arg (ad-get-arg 2)))
      (message "%s environment containing %s lines"
               (if  prefix-arg
                   "Uncommented"
                 "Commented")
               (count-lines (point) (mark 'force))))))

(defadvice meta-uncomment-defun (after emacspeak pre act )
  "Provide spoken feedback."
  (when (interactive-p)
    (message "Uncommented environment containing %s lines"
	     (count-lines (point) (mark 'force)))))

(defadvice meta-uncomment-region (after emacspeak pre act )
  "Provide spoken feedback."
  (when (interactive-p)
    (message "Uncommented  region containing %s lines"
	     (count-lines (point) (mark 'force)))))

(defadvice meta-indent-region (after emacspeak pre act )
  "Provide spoken feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'fill-object)
    (message "Indented  region containing %s lines"
	     (count-lines (point) (mark 'force)))))

(defadvice meta-indent-buffer (after emacspeak pre act )
  "Provide spoken feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'fill-object)
    (message "Indented  buffer containing %s lines"
	     (count-lines (point-min) (point-max 'force)))))

(defadvice meta-mark-defun (after emacspeak pre act)
  "Produce an auditory icon if possible."
  (when (interactive-p )
    (emacspeak-auditory-icon 'mark-object)
    (message "Marked function containing %s lines"
             (count-lines (point)
                          (mark 'force)))))

(defadvice meta-indent-defun (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'fill-object)
    (message "Indented current defun. ")))

;;}}}
(provide 'emacspeak-metapost)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

;;; emacspeak-tcl.el --- Speech enable TCL development environment
;;; $Id$
;;; $Author$ 
;;; DescriptionEmacspeak extensions for tcl-mode
;;; Keywords:emacspeak, audio interface to emacs tcl
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

;;{{{  Introduction:

;;; Provide additional advice to tcl-mode 

;;}}}
;;{{{ requires
(require 'emacspeak-preamble)

;;}}}
;;{{{ voice locking:

;;;  Snarfed from tcl.el /usr/local/lib/emacs/site-lisp/tcl.el

(defvar tcl-proc-list
  '("proc" "method" "itcl_class" "public" "protected")
  "List of commands whose first argument defines something.
This exists because some people (eg, me) use \"defvar\" et al. ")

(defvar tcl-proc-regexp
  (concat "^\\("
          (mapconcat 'identity tcl-proc-list "\\|")
          "\\)[ \t]+")
  "Regexp to use when matching proc headers.")

(defvar tcl-typeword-list
  '("global" "upvar")
  "List of Tcl keywords denoting \"type\".  Used only for highlighting. ")

;; Generally I've picked control operators to be keywords.
(defvar tcl-keyword-list
  '("if" "then" "else" "elseif" "for" "foreach" "break" "continue" "while"
    "set" "eval" "case" "in" "switch" "default" "exit" "error" "proc" "return"
    "uplevel" "loop" "for_array_keys" "for_recursive_glob" "for_file"
    "unwind_protect" 
    ;; itcl
    "method" "itcl_class" )
  "List of Tcl keywords.  Used only for highlighting.
Default list includes some TclX keywords. ")

;; FIXME need some way to recognize variables because array refs look
;; like 2 sexps.
(defvar tcl-type-alist
  '(
    ("expr" tcl-expr)
    ("catch" tcl-commands)
    ("set" tcl-expr)
    ("if" tcl-expr "then" tcl-commands)
    ("elseif" tcl-expr "then" tcl-commands)
    ("elseif" tcl-expr tcl-commands)
    ("if" tcl-expr tcl-commands)
    ("while" tcl-expr tcl-commands)
    ("for" tcl-commands tcl-expr tcl-commands tcl-commands)
    ("foreach" nil nil tcl-commands)
    ("for_file" nil nil tcl-commands)
    ("for_array_keys" nil nil tcl-commands)
    ("for_recursive_glob" nil nil nil tcl-commands)
    ;; Loop handling is not perfect, because the third argument can be
    ;; either a command or an expr, and there is no real way to look
    ;; forward.
    ("loop" nil tcl-expr tcl-expr tcl-commands)
    ("loop" nil tcl-expr tcl-commands)
    )
  "Alist that controls indentation.
\(Actually, this really only controls what happens on continuation lines).
Each entry looks like `(KEYWORD TYPE ...)'.
Each type entry describes a sexp after the keyword, and can be one of:
* nil, meaning that this sexp has no particular type.
* tcl-expr, meaning that this sexp is an arithmetic expression.
* tcl-commands, meaning that this sexp holds Tcl commands.
* a string, which must exactly match the string at the corresponding
  position for a match to be made.

For example, the entry for the \"loop\" command is:

   (\"loop\" nil tcl-expr tcl-commands)

This means that the \"loop\" command has three arguments.  The first
argument is ignored (for indentation purposes).  The second argument
is a Tcl expression, and the last argument is Tcl commands.")

;;}}}
;;{{{  Advice electric insertion to talk:

(defadvice tcl-electric-hash (after emacspeak pre act comp )
  "Speak what you inserted."
  (when (interactive-p)
    (emacspeak-speak-this-char last-input-char)))

(defadvice tcl-electric-char (after emacspeak pre act comp )
  "Speak what you inserted."
  (when (interactive-p)
    (emacspeak-speak-this-char last-input-char)))

(defadvice tcl-electric-brace (after emacspeak pre act comp )
  "Speak what you inserted."
  (when (interactive-p)
    (emacspeak-speak-this-char last-input-char)))

;;}}}
;;{{{  Actions in the tcl mode buffer:

(defadvice switch-to-tcl (before emacspeak pre act comp)
  "Announce yourself."
  (when (interactive-p)
    (message "Switching to the Inferior TCL buffer")))

(defadvice tcl-eval-region (after emacspeak  pre act comp)
  "Announce what you did."
  (when (interactive-p)
    (message "Evaluating contents of region")))

(defadvice tcl-eval-defun (after emacspeak pre act comp )
  "Announce what you did"
  (when (interactive-p)
    (let* ((start nil)
           (proc-line
            (save-excursion
              (tcl-beginning-of-defun)
              (setq start (point))
              (end-of-line)
              (buffer-substring start (point)))))
      (message "Evaluated  %s" proc-line ))))

(defadvice tcl-help-on-word (after emacspeak pre act comp)
  "Speak  the help."
  (when (interactive-p)
    (emacspeak-auditory-icon 'help)
    (save-excursion
      (set-buffer "*Tcl help*")
      (emacspeak-speak-buffer ))))

;;}}}
;;{{{  Program structure:

(defadvice tcl-mark-defun (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (message "Marked procedure")))

(defadvice tcl-beginning-of-defun (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice tcl-end-of-defun (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice indent-tcl-exp (after emacspeak pre act )
  "Produce an auditory icon"
  (when (interactive-p)
    (emacspeak-auditory-icon 'fill-object)))

(defadvice tcl-indent-line (after emacspeak pre act)
  "Speak the line"
  (when (interactive-p)
    (emacspeak-speak-line)))
;;}}}

(provide  'emacspeak-tcl)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

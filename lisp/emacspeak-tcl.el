;;; emacspeak-tcl.el --- Speech enable TCL -*- lexical-binding: t; -*-
;;
;; $Author: tv.raman.tv $ 
;; DescriptionEmacspeak extensions for tcl-mode
;; Keywords:emacspeak, audio interface to emacs tcl
;;{{{  LCD Archive entry: 

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com 
;; A speech interface to Emacs |
;; 
;;  $Revision: 4532 $ | 
;; Location undetermined
;; 

;;}}}
;;{{{  Copyright:
;; Copyright (C) 1995 -- 2022, T. V. Raman 
;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
;; All Rights Reserved. 
;; 
;; This file is not part of GNU Emacs, but the same permissions apply.
;; 
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;}}}

;;{{{  Introduction:
;;; Commentary:
;; Provide additional advice to tcl-mode 
;;; Code:
;;}}}
;;{{{ requires
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ voice locking:

;;  Snarfed from tcl.el /usr/local/lib/emacs/site-lisp/tcl.el

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
    "uplevel" "cl-loop" "for_array_keys" "for_recursive_glob" "for_file"
    "unwind_protect" 
    ;; itcl
    "method" "itcl_class")
  "List of Tcl keywords.  Used only for highlighting.
Default list includes some TclX keywords. ")

;;}}}
;;{{{  Advice electric insertion to talk:

(defadvice tcl-electric-hash (after emacspeak pre act comp)
  "Speak what you inserted."
  (when (ems-interactive-p)
    (emacspeak-speak-this-char last-input-event)))

(defadvice tcl-electric-char (after emacspeak pre act comp)
  "Speak what you inserted."
  (when (ems-interactive-p)
    (emacspeak-speak-this-char last-input-event)))

(defadvice tcl-electric-brace (after emacspeak pre act comp)
  "Speak what you inserted."
  (when (ems-interactive-p)
    (emacspeak-speak-this-char last-input-event)))

;;}}}
;;{{{  Actions in the tcl mode buffer:

(defadvice switch-to-tcl (before emacspeak pre act comp)
  "Announce yourself."
  (when (ems-interactive-p)
    (message "Switching to the Inferior TCL buffer")))

(defadvice tcl-eval-region (after emacspeak  pre act comp)
  "Announce what you did."
  (when (ems-interactive-p)
    (message "Evaluating contents of region")))

(defadvice tcl-eval-defun (after emacspeak pre act comp)
  "Announce what you did"
  (when (ems-interactive-p)
    (let* ((start nil)
           (proc-line
            (save-excursion
              (tcl-beginning-of-defun)
              (setq start (point))
              (end-of-line)
              (buffer-substring start (point)))))
      (message "Evaluated  %s" proc-line))))

(defadvice tcl-help-on-word (after emacspeak pre act comp)
  "Speak  the help."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'help)
    (with-current-buffer "*Tcl help*"      (emacspeak-speak-buffer))))

;;}}}
;;{{{  Program structure:

(defadvice tcl-mark-defun (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (message "Marked procedure")))

(defadvice tcl-beginning-of-defun (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'paragraph)
    (emacspeak-speak-line)))

(defadvice tcl-end-of-defun (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'paragraph)))

(defadvice indent-tcl-exp (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'fill-object)))

(defadvice tcl-indent-line (after emacspeak pre act comp)
  "Speak the line"
  (when (ems-interactive-p)
    (emacspeak-speak-line)))
;;}}}

(provide  'emacspeak-tcl)
;;{{{  emacs local variables 

;; local variables:
;; folded-file: t
;; end: 

;;}}}

;;; emacspeak-auctex.el --- Speech enable AucTeX -- a powerful TeX/LaTeX authoring environment
;;; $Id$
;;; $Author$ 
;;; DescriptionEmacspeak extensions for auctex-mode
;;; Keywords:emacspeak, audio interface to emacs AUCTEX
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
(require 'voice-lock)
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
;;{{{  Introduction:

;;; Provide additional advice to auctex

;;}}}
;;{{{ voice locking:


(defvar tex-voice-lock-keywords
  (list
   '("\\(\\\\\\([a-zA-Z@]+\\|.\\)\\)" 1 voice-lock-keyword-personality t)
   '("{\\\\em\\([^}]+\\)}" 1 voice-lock-italic-personality t)
   '("{\\\\bf\\([^}]+\\)}" 1 voice-lock-bold-personality t)
   '("^[ \t\n]*\\\\def[\\\\@]\\(\\w+\\)" 1 voice-lock-function-name-personality t)
   '("\\\\\\(begin\\|end\\){\\([a-zA-Z0-9\\*]+\\)}"
     2 voice-lock-function-name-personality t)
   '("[^\\\\]\\$\\([^$]*\\)\\$" 1 voice-lock-string-personality t)
;   '("\\$\\([^$]*\\)\\$" 1 voice-lock-string-personality t)
   )
  "Additional expressions to highlight in TeX mode.")

(mapcar 
(function
 (lambda (mode)
   (voice-lock-set-major-mode-keywords mode 'tex-voice-lock-keywords)))
(list 
    'plain-tex-mode  
		   'latex-mode      
		   'slitex-mode     
		   'latex2e-mode))

;;}}}
;;{{{  Marking structured objects:
(defadvice LaTeX-fill-paragraph (after emacspeak pre act  comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'fill-object)))

(defadvice LaTeX-mark-section (after emacspeak pre act)
"Speak the first line. 
Also provide an auditory icon. "
(when (interactive-p) 
(emacspeak-speak-line)
(emacspeak-auditory-icon 'mark-object)))

(defadvice LaTeX-mark-environment (after emacspeak pre act)
"Speak the first line. 
Also provide an auditory icon. "
(when (interactive-p) 
(emacspeak-speak-line)
(emacspeak-auditory-icon 'mark-object)))

(defadvice LaTeX-format-paragraph (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'fill-object)
    (message "Filled current paragraph")))
(defadvice LaTeX-format-region (around emacspeak pre act )
  "Ask for confirmation.
Provide auditory feedback after formatting region"
  (cond
   ((and (interactive-p)
         (y-or-n-p "Really format region? "))
    ad-do-it
    (emacspeak-auditory-icon 'fill-object)
    (message "Reformatted region"))
   ((not (interactive-p)) ad-do-it))
  ad-return-value)
    

;;}}}
;;{{{  delimiter matching:

(defadvice LaTeX-find-matching-begin (after emacspeak pre act)
"Provide auditory feedback. "
(when (interactive-p)
(emacspeak-speak-line)))

(defadvice LaTeX-find-matching-end (after emacspeak pre act)
"Provide auditory feedback. "
(when (interactive-p)
(emacspeak-speak-line)))

(defadvice LaTeX-close-environment (after emacspeak pre act)
  "Speak the inserted line. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-read-previous-line)))

(defadvice TeX-insert-dollar (after emacspeak pre act comp)
  "Speak what you inserted"
  (when (interactive-p)
    (emacspeak-speak-this-char  (preceding-char ))))

;;}}}
;;{{{  Inserting structures

(defadvice LaTeX-insert-item (after emacspeak pre act)
"Provide auditory feedback. "
(when (interactive-p)
(emacspeak-speak-line )))

(defadvice LaTeX-environment (after emacspeak pre act)
"Provide auditory feedback, by speaking 
the opening line of the newly inserted environment. "
(when (interactive-p)
(emacspeak-auditory-icon 'open-object)
(emacspeak-read-previous-line)))

(defadvice TeX-insert-macro (around  emacspeak pre act)
"Provide spoken feedback."
(let ((opoint (point )))
ad-do-it
(emacspeak-speak-region opoint (point))))

;;}}}
;;{{{  Commenting chunks:

(defadvice TeX-comment-region (after emacspeak pre act)
  "Provide spoken and auditory feedback. "
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice TeX-un-comment (after emacspeak pre act)
  "Provide spoken and auditory feedback. "
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice TeX-un-comment-region (after emacspeak pre act)
  "Provide spoken and auditory feedback. "
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice TeX-comment-paragraph (after emacspeak pre act)
  "Provide spoken and auditory feedback. "
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

;;}}}

;;{{{  Debugging tex

(defadvice TeX-next-error (after emacspeak pre act)
  "Speak the error line. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line )))

;;}}}
;;{{{  Hooks

;;; We add imenu settings to LaTeX-mode-hook

(add-hook  'LaTeX-mode-hook
           (function
            (lambda ()
    (declare (special imenu-generic-expression
                    imenu-create-index-function))
    (require 'imenu)
    (setq imenu-create-index-function 'imenu-default-create-index-function)
  (setq imenu-generic-expression
        '(
          (nil
            "^ *\\\\\\(sub\\)*section{\\([^}]+\\)"
            2))))))
              

;;}}}
;;{{{ advice font changes 
(defadvice TeX-font (around emacspeak pre act comp)
  "Speak the font we inserted"
  (cond 
   ((interactive-p)
    (let ((orig (point)))
      ad-do-it
      (emacspeak-speak-region orig (point))))
   (t ad-do-it))
  ad-return-value)
;;}}}
(provide  'emacspeak-auctex)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

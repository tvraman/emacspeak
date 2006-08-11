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
;;;  $Revision: 24.0 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2004, T. V. Raman 
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
;;{{{ Required modules 
(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction:

;;; Provide additional advice to auctex

;;}}}
;;{{{ voice locking:

;;; faces from AUCTeX 11
(voice-setup-add-map
 '(
   (font-latex-bold-face voice-bolden)
   (font-latex-doctex-documentation-face voice-monotone-medium)
   (font-latex-doctex-preprocessor-face voice-brighten-medium)
   (font-latex-italic-face voice-animate)
   (font-latex-math-face voice-brighten-extra)
   (font-latex-sedate-face voice-smoothen)
   (font-latex-string-face voice-lighten)
   (font-latex-subscript-face voice-smoothen)
   (font-latex-superscript-face voice-brighten)
   (font-latex-title-1-face voice-bolden-extra)
   (font-latex-title-2-face voice-bolden-medium)
   (font-latex-title-3-face voice-bolden)
   (font-latex-title-4-face voice-smoothen-extra)
   (font-latex-verbatim-face voice-monotone)
   (font-latex-warning-face voice-bolden-and-animate)
   ))

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
      (if (ad-get-arg 0)
          (emacspeak-speak-line)
        (emacspeak-speak-region orig (point)))))
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

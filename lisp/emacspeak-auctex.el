;;; emacspeak-auctex.el --- Speech enable AucTeX -- a powerful TeX/LaTeX authoring environment  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; DescriptionEmacspeak extensions for auctex-mode
;;; Keywords:emacspeak, audio interface to emacs AUCTEX
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com 
;;; A speech interface to Emacs |
;;; $Date: 2007-09-27 09:14:42 -0700 (Thu, 27 Sep 2007) $ |
;;;  $Revision: 4532 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2021, T. V. Raman 
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;{{{ Required modules 
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction:
;;; Commentary:
;;; Speech-enables the AucTeX package.
;;; AucTeX, now available from ELPA, has been my authoring environment of choice for writing LaTeX since 1991.
;;; Code:
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
   (font-latex-verbatim-face voice-monotone-extra)
   (font-latex-warning-face voice-bolden-and-animate)
   ))

;;}}}
;;{{{  Marking structured objects:

(defadvice LaTeX-fill-paragraph (after emacspeak pre act  comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'fill-object)))

(defadvice LaTeX-mark-section (after emacspeak pre act comp)
  "Speak the first line. 
Also provide an auditory icon. "
  (when (ems-interactive-p) 
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice LaTeX-mark-environment (after emacspeak pre act comp)
  "Speak the first line. 
Also provide an auditory icon. "
  (when (ems-interactive-p) 
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice LaTeX-format-paragraph (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'fill-object)
    (message "Filled current paragraph")))
(defadvice LaTeX-format-region (around emacspeak pre act comp)
  "Ask for confirmation.
speak after formatting region"
  (cond
   ((and (ems-interactive-p)
         (y-or-n-p "Really format region? "))
    ad-do-it
    (emacspeak-auditory-icon 'fill-object)
    (message "Reformatted region"))
   ((not (ems-interactive-p)) ad-do-it))
  ad-return-value)

;;}}}
;;{{{  delimiter matching:

(defadvice LaTeX-find-matching-begin (after emacspeak pre act comp)
  "speak. "
  (when (ems-interactive-p)
    (emacspeak-speak-line)))

(defadvice LaTeX-find-matching-end (after emacspeak pre act comp)
  "speak. "
  (when (ems-interactive-p)
    (emacspeak-speak-line)))

(defadvice LaTeX-close-environment (after emacspeak pre act comp)
  "Speak the inserted line. "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-read-previous-line)))

(cl-loop
 for f in 
 '(TeX-insert-dollar TeX-insert-quote)
 do
 (eval
  `(defadvice ,f(around emacspeak pre act com)
  "Speak quotes that were inserted."
  (cond
   ((ems-interactive-p)
    (let ((orig (point)))
      ad-do-it
      (emacspeak-speak-region orig (point))))
   (t ad-do-it))
  ad-return-value)))


;;}}}
;;{{{  Inserting structures

(defadvice TeX-newline (after emacspeak pre act comp)
  "speak to indicate indentation."
  (when (ems-interactive-p)
    (emacspeak-speak-line)))

(defadvice LaTeX-insert-item (after emacspeak pre act comp)
  "speak. "
  (when (ems-interactive-p)
    (emacspeak-speak-line)))

(defadvice LaTeX-environment (after emacspeak pre act comp)
  "speak, by speaking
the opening line of the newly inserted environment. "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-read-previous-line)))

(defadvice TeX-insert-macro (around  emacspeak pre act comp)
  "Speak."
  (let ((opoint (point)))
    ad-do-it
    (emacspeak-speak-region opoint (point))))

;;}}}
;;{{{  Commenting chunks:

(defadvice TeX-comment-region (after emacspeak pre act comp)
  "Provide spoken and auditory feedback. "
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice TeX-un-comment (after emacspeak pre act comp)
  "Provide spoken and auditory feedback. "
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice TeX-un-comment-region (after emacspeak pre act comp)
  "Provide spoken and auditory feedback. "
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice TeX-comment-paragraph (after emacspeak pre act comp)
  "Provide spoken and auditory feedback. "
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

;;}}}
;;{{{  Debugging tex

(defadvice TeX-next-error (after emacspeak pre act comp)
  "Speak the error line. "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'item)
    (emacspeak-speak-line)))

;;}}}
;;{{{  Hooks

;;; We add imenu settings to LaTeX-mode-hook

(add-hook  'LaTeX-mode-hook
            #'(lambda ()
              (cl-declare (special imenu-generic-expression
                                   imenu-create-index-function))
              (require 'imenu)
              (setq imenu-create-index-function 'imenu-default-create-index-function)
              (setq imenu-generic-expression
                    '(
                      (nil
                       "^ *\\\\\\(sub\\)*section{\\([^}]+\\)"
                       2)))))

;;}}}
;;{{{ advice font changes 

(defadvice TeX-font (around emacspeak pre act comp)
  "Speak the font we inserted"
  (cond 
   ((ems-interactive-p)
    (let ((orig (point)))
      ad-do-it
      (if (ad-get-arg 0)
          (emacspeak-speak-line)
        (emacspeak-speak-region orig (point)))))
   (t ad-do-it))
  ad-return-value)

;;}}}
;;{{{ tex utils:


(defun emacspeak-auctex-end-of-word (arg)
  "move to end of word"
  (interactive "P")
  (if arg
      (forward-word arg)
    (forward-word 1)))


(defun emacspeak-auctex-comma-at-end-of-word ()
  "Move to the end of current word and add a comma."
  (interactive)
  (forward-word 1)
  (insert-char ?,))


(defun emacspeak-auctex-lacheck-buffer-file ()
  "Run Lacheck on current buffer."
  (interactive)
  (compile (format "lacheck %s"
                   (buffer-file-name (current-buffer)))))


(defun emacspeak-auctex-tex-tie-current-word (n)
  "Tie the next n  words."
  (interactive "P")
  (or n (setq n 1))
  (while
      (> n 0)
    (setq n (- n 1))
    (forward-word 1)
    (delete-horizontal-space)
    (insert-char 126 1))
  (forward-word 1))

;;}}}
(provide  'emacspeak-auctex)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}

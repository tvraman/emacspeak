;;; emacspeak-python.el --- Speech enable Python development environment
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description: Auditory interface to python mode
;;; Keywords: Emacspeak, Speak, Spoken Output, python
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (c) 1995 -- 2015, T. V. Raman
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

;;{{{ Introduction

;;; Commentary:

;;; This speech-enables python-mode bundled with Emacs

;;; Code:

;;}}}
;;{{{  Required modules
(require 'cl)
(require 'emacspeak-preamble)
(eval-when-compile
  (require 'python "python" 'no-error))
;;}}}
;;{{{ interactive programming

(defadvice python-check (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)))
(defadvice python-send-region (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'task-done)))

(defadvice python-send-buffer (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'task-done)))

;;}}}
;;{{{  whitespace management and indentation

(defadvice  python-indent-dedent-line-backspace (around emacspeak pre act)
  "Speak character you're deleting."
  (cond
   ((ems-interactive-p  )
    (dtk-tone 500 30 'force)
    (emacspeak-speak-this-char (preceding-char ))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)


(defadvice python-fill-paragraph (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'fill-object)))

(defadvice python-indent-shift-left (after emacspeak pre act comp)
  "Speak number of lines that were shifted"
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'large-movement)
    (dtk-speak
     (format "Left shifted block  containing %s lines"
             (count-lines  (region-beginning)
                           (region-end))))))
(defadvice python-indent-shift-right (after emacspeak pre act comp)
  "Speak number of lines that were shifted"
  (when (ems-interactive-p )
    (dtk-speak
     (format "Right shifted block  containing %s lines"
             (count-lines  (region-beginning)
                           (region-end))))))
(defadvice python-indent-region (after emacspeak pre act comp)
  "Speak number of lines that were shifted"
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'large-movement)
    (dtk-speak
     (format "Indented region   containing %s lines"
             (count-lines  (region-beginning)
                           (region-end))))))

;;}}}
;;{{{  buffer navigation

(loop
 for f in
 '(
   python-nav-up-list python-nav-if-name-main python-nav-forward-statement
                      python-nav-forward-sexp-safe python-nav-forward-sexp python-nav-forward-defun
                      python-nav-forward-block python-nav-end-of-statement python-nav-end-of-defun
                      python-nav-end-of-block python-nav-beginning-of-statement python-nav-beginning-of-block
                      python-nav-backward-up-list python-nav-backward-statement python-nav-backward-sexp-safe
                      python-nav-backward-sexp python-nav-backward-defun python-nav-backward-block
                      )
 do
 (eval
  `(defadvice  ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p )
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'large-movement)))))

;;}}}
;;{{{ the process buffer

(defadvice python-process-filter (around emacspeak pre act)
  "Make comint in Python speak its output. "
  (declare (special emacspeak-comint-autospeak))
  (let ((prior (point ))
        (dtk-stop-immediately nil))
    ad-do-it
    (when (and  emacspeak-comint-autospeak
                (window-live-p
                 (get-buffer-window (process-buffer (ad-get-arg 0)))))
      (condition-case nil
          (emacspeak-speak-region prior (point ))
        (error (emacspeak-auditory-icon 'scroll)
               (dtk-stop ))))
    ad-return-value))

;;}}}

;;{{{ keybindings

(progn
  (declaim (special  python-mode-map))
  (define-key python-mode-map "\M-n" 'python-next-statement)
  (define-key python-mode-map "\M-p" 'python-previous-statement)
  (define-key python-mode-map "\C-\M-u" 'python-goto-block-up)
  )
(add-hook 'python-mode-hook
          'emacspeak-setup-programming-mode)
;;}}}
(provide 'emacspeak-python )
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}

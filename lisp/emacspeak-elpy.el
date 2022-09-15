;;; emacspeak-elpy.el --- Speech-enable ELPY -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable ELPY An Emacs Interface to elpy
;; Keywords: Emacspeak,  Audio Desktop elpy
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
;; Copyright (C) 1995 -- 2007, 2011, T. V. Raman
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
;; MERCHANTABILITY or FITNELPY FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;; ELPY ==  Emacs Lisp Python IDE
;; Speech-enables all aspects of elpy.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Advice Interactive Commands:

(cl-loop
 for f in
 '(
   elpy-autopep8-fix-code elpy-config elpy-check
   elpy-occur-definitions elpy-rgrep-symbol
   elpy-set-project-root elpy-set-project-variable
   elpy-set-test-runner
   elpy-shell-send-current-statement elpy-shell-send-region-or-buffer
   elpy-shell-switch-to-buffer elpy-shell-switch-to-shell
   elpy-use-cpython elpy-use-ipython
   elpy-importmagic-add-import elpy-importmagic-fixup)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'task-done)
       (emacspeak-speak-mode-line)))))

(defadvice elpy-enable (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'on)
    (message "Enabled elpy")))

(defadvice elpy-disable (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'off)
    (message "Disabled elpy")))

(defadvice elpy-doc (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'help)
    (message "Displayed help in other window.")))

(defadvice elpy-find-file (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))
(cl-loop
 for f in
 '(elpy-flymake-next-error elpy-flymake-previous-error
                           elpy-goto-definition)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))

                                        ; elpy-flymake-show-error

(cl-loop
 for f in
 '(
   elpy-nav-backward-block elpy-nav-backward-indent
   elpy-nav-expand-to-indentation elpy-nav-forward-block
   elpy-nav-forward-indent
   elpy-nav-indent-shift-left elpy-nav-indent-shift-right
   elpy-open-and-indent-line-below elpy-open-and-indent-line-above
   elpy-nav-move-line-or-region-down elpy-nav-move-line-or-region-up)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))

;;}}}
(provide 'emacspeak-elpy)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}

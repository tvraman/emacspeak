;;; emacspeak-ein.el --- Speech-enable EIN
;;; $Id: emacspeak-ein.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable EIN An Emacs Interface to ein
;;; Keywords: Emacspeak,  Audio Desktop ein
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, 2011, T. V. Raman
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
;;; MERCHANTABILITY or FITNEIN FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; EIN ==  Emacs IPython Notebook
;;; You can install package EIN via ELPA
;;; This module speech-enables EIN

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{  Face->Voice mappings

(voice-setup-add-map
 '(
   (ein:cell-input-prompt voice-animate)
   (ein:cell-input-area voice-lighten)
   (ein:cell-heading-1 voice-bolden)
   (ein:cell-heading-2 voice-smoothen)
   (ein:cell-heading-3 voice-smoothen-extra)
   (ein:cell-heading-4 voice-annotate)
   (ein:cell-output-prompt voice-monotone)
   (ein:cell-output-stderr voice-smoothen)
                                        ;ein:pos-tip-face
                                        ;ein:notification-tab-selected
                                        ;ein:notification-tab-normal
   ))

;;}}}
;;{{{ Additional Interactive Commands:

(defun emacspeak-ein-speak-current-cell ()
  "Speak current cell."
  (interactive)
  (emacspeak-auditory-icon 'select-object)
  (emacspeak-speak-region (point) (next-overlay-change (point))))

;;}}}
;;{{{ Advice completers:

;; ein:completer-complete
;; ein:completer-dot-complete
;; ein:jedi-complete
;; ein:jedi-dot-complete

;;}}}
;;{{{ Advice Tasks:

;; ein:connect-to-notebook
;; ein:connect-to-notebook-buffer
;; ein:connect-to-notebook-command
;; ein:console-open

;;}}}
;;{{{ Advice Notebook:

;;}}}
;;{{{ Advice NotebookList:

;; ein:notebooklist-login
;; ein:notebooklist-menu
;; ein:notebooklist-new-notebook
;; ein:notebooklist-new-notebook-with-name
;; ein:notebooklist-next-item
;; ein:notebooklist-open
;; ein:notebooklist-open-notebook-by-file-name
;; ein:notebooklist-open-notebook-global
;; ein:notebooklist-prev-item
;; ein:notebooklist-reload
;; ein:junk-new
;; ein:junk-rename

;;}}}
;;{{{   Commands:

;; ein:header-line-delete-this-tab
;; ein:header-line-insert-new-tab
;; ein:header-line-insert-next-tab
;; ein:header-line-insert-prev-tab
;; ein:header-line-move-next-tab
;; ein:header-line-move-prev-tab
;; ein:header-line-pop-to-this-tab
;; ein:header-line-switch-to-this-tab
;; ein:iexec-mode
;; ein:ipynb-mode

;; ein:pager-goto-docstring-bset-loc
;; ein:pytools-doctest
;; ein:pytools-hierarchy
;; ein:pytools-jump-back-command
;; ein:pytools-jump-to-source-command
;; ein:pytools-pandas-to-ses
;; ein:pytools-request-help
;; ein:pytools-request-tooltip
;; ein:pytools-request-tooltip-or-help
;; ein:pytools-whos
;; ein:shared-output-eval-string
;; ein:shared-output-pop-to-buffer
;; ein:shared-output-show-code-cell-at-point
;; ein:tb-show

;;}}}
;;{{{ Worksheets:

(loop for f in
      '(
        ein:worksheet-clear-all-output
        ein:worksheet-delete-cell
        ein:worksheet-clear-output
        ein:worksheet-kill-cell
        )
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon 'delete-object)))))
;; ein:worksheet-change-cell-type
;; ein:worksheet-copy-cell
;; ein:worksheet-dedent-cell-text

(loop for f in
      '(
        ein:worksheet-execute-all-cell
        ein:worksheet-execute-autoexec-cells
        ein:worksheet-execute-cell-and-insert-below
        ein:worksheet-execute-cell-and-goto-next
        ein:worksheet-execute-cell)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon 'task-done)
            (emacspeak-speak-line)))))
(loop for f in
      '(
        ein:worksheet-goto-next-input
        ein:worksheet-goto-prev-input
        ein:worksheet-next-input-history
        ein:worksheet-previous-input-history
        )
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon 'large-movement)
            (emacspeak-ein-speak-current-cell)))))

(loop for f in
      '(
        ein:worksheet-yank-cell
        ein:worksheet-insert-cell-above
        ein:worksheet-insert-cell-below)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon 'yank-object)
            (emacspeak-speak-line)))))

;; ein:worksheet-merge-cell
;; ein:worksheet-move-cell-down
;; ein:worksheet-move-cell-up

;; ein:worksheet-rename-sheet
;; ein:worksheet-set-output-visibility-all
;; ein:worksheet-split-cell-at-point
;; ein:worksheet-toggle-autoexec
;; ein:worksheet-toggle-cell-type
;; ein:worksheet-toggle-output
;; ein:worksheet-turn-on-autoexec

;;}}}
;;{{{ Bind additional interactive commands
(loop for k in
      '(
        ("\C-c." emacspeak-ein-speak-current-cell)
        )
      do
      (emacspeak-keymap-update ein:notebook-mode-map k))

;;}}}
(provide 'emacspeak-ein)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

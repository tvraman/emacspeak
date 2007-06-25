;;; emacspeak-org.el --- Speech-enable org
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak front-end for ORG
;;; Keywords: Emacspeak, org
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-01-27 10:39:40 -0800 (Sat, 27 Jan 2007) $ |
;;;  $Revision: 4347 $ |
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

;;; Commentary:
;;{{{  Introduction:

;;; Speech-enable org ---
;;;  Org allows you to keep organized notes and todo lists.
;;; Homepage: http://www.astro.uva.nl/~dominik/Tools/org/

;;}}}
;;{{{ required modules

;;; Code:
(require 'emacspeak-preamble)
(require 'emacspeak-redefine)

;;}}}
;;{{{ voice locking:
(voice-setup-add-map
 '(
   (org-level-1 voice-bolden-medium)
   (org-level-2 voice-bolden)
   (org-level-3 voice-animate)
   (org-level-4 voice-monotone)
   (org-level-5 voice-smoothen)
   (org-level-6 voice-lighten)
   (org-level-7 voice-lighten-medium)
   (org-level-8 voice-lighten-extra)
   (org-special-keyword voice-lighten-extra)
   (org-warning voice-bolden-and-animate)
   (org-headline-done voice-lighten-extra)
   (org-link voice-bolden)
   (org-date voice-animate)
   (org-tag voice-smoothen)
   (org-todo voice-bolden-and-animate)
   (org-done voice-smoothen)
   (org-table voice-bolden-medium)
   (org-formula voice-animate-extra)
   (org-scheduled-today voice-bolden-extra)
   (org-scheduled-previously voice-lighten-medium)
   (org-time-grid voice-bolden)))

;;}}}
;;{{{ Structure Navigation:

(loop for f in
      '(org-mark-ring-goto
        org-next-link org-previous-link
        org-goto  org-goto-ret
        org-goto-left org-goto-right
        org-goto-quit
        org-next-item org-previous-item
        org-metaleft org-metaright org-metaup org-metadown
        org-shiftmetaleft org-shiftmetaright org-shiftmetaup org-shiftmetadown
        )
      do
      (eval
       `(defadvice ,f(after emacspeak pre act comp)
          "Provide spoken feedback."
          (when (interactive-p)
            (emacspeak-speak-line)
            (emacspeak-auditory-icon 'large-movement)))))

(loop for f in
      '(org-cycle org-shifttab
                  )
      do
      (eval
       `(defadvice ,f(after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (interactive-p)
            (cond
             ((org-at-table-p 'any)
              (let ((emacspeak-show-point t))
                (skip-syntax-forward " ")
                (emacspeak-speak-line)
                (emacspeak-auditory-icon 'large-movement)))
             (t (emacspeak-auditory-icon 'select-object)))))))

;;}}}
;;{{{ Header insertion and relocation

(loop for f in
      '(org-insert-heading org-insert-todo-heading
                           org-promote-subtree org-demote-subtree
                           org-do-promote org-do-demote
                           org-move-subtree-up org-move-subtree-down
                           )
      do
      (eval
       `(defadvice ,f(after emacspeak pre act comp)
          "Provide spoken feedback."
          (when (interactive-p)
            (emacspeak-speak-line)
            (emacspeak-auditory-icon 'open-object)))))

;;}}}
;;{{{ cut and paste:

(loop for f in
      '(org-copy-subtree org-paste-subtree
                         org-archive-subtree)
      do
      (eval
       `(defadvice ,f(after emacspeak pre act comp)
          "Provide spoken feedback."
          (when (interactive-p)
            (emacspeak-speak-line)
            (emacspeak-auditory-icon 'yank-object)))))

;;}}}
;;{{{ completion:

(defadvice org-complete (around emacspeak pre act)
  "Say what you completed."
  (let ((prior (save-excursion
                 (backward-word 1)
                 (point )))
        (dtk-stop-immediately t))
    ad-do-it
    (let ((completions-buffer (get-buffer "*Completions*")))
      (if (> (point) prior)
          (tts-with-punctuations 'all
                                 (dtk-speak (buffer-substring prior (point ))))
        (when (and completions-buffer
                   (window-live-p (get-buffer-window completions-buffer )))
          (save-excursion
            (set-buffer completions-buffer )
            (emacspeak-prepare-completions-buffer)
            (dtk-speak (buffer-string ))))))
    ad-return-value))

;;}}}
;;{{{ toggles:

;;}}}
;;{{{ ToDo:

;;}}}
;;{{{ timestamps and calendar:

;;}}}
;;{{{ Agenda:

;;}}}
;;{{{ misc file commands:

;;}}}
;;{{{ tables:

;;}}}
;;{{{ table minor mode:
(defadvice orgtbl-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon
     (if orgtbl-mode 'on 'off))
    (message "Turned %s org table mode."
             (if orgtbl-mode 'on 'off))))

;;}}}
;;{{{ import/export:

;;}}}
;;{{{ Meta Navigators:

(loop for f in
      '(org-metadown org-metaup org-metaleft org-metaright)
      do
      (eval
       `(defadvice ,f(after emacspeak pre act comp)
          "Provide spoken feedback."
          (when (interactive-p)
            (emacspeak-speak-line)
            (emacspeak-auditory-icon 'yank-object)))))

;;}}}
;;{{{ org-goto fixup:
(declaim (special org-goto-map org-mode-map))
(loop for k in
      '(
        ([backtab]    org-shifttab)
        ([shift up] org-shiftup)
        ([shift down] org-shiftdown)
        ([shift left] org-shiftleft)
        ([shift right] org-shiftright)
        ([27 shift down] org-shiftmetadown)
        ([27 shift up] org-shiftmetaup)
        ([27 shift left] org-shiftmetaleft)
        ([27 shift right] org-shiftmetaright)
        ([27 S-Return] org-insert-todo-heading)
        ("\C-j" org-insert-heading)
        ("\M-n" org-next-item)
        ("\M-p" org-previous-item)
        )
      do
      (emacspeak-keymap-update  org-mode-map k))

(loop for k in'(
                ( "\C-e" emacspeak-prefix-command)
                ( "\C-h" help-command))
      do
      (emacspeak-keymap-update  org-goto-map k))

;;}}}
;;{{{ deleting chars:

(defadvice org-delete-backward-char (around emacspeak pre act)
  "Speak character you're deleting."
  (cond
   ((interactive-p )
    (dtk-tone 500 30 'force)
    (emacspeak-speak-this-char (preceding-char ))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)
(defadvice org-force-self-insert (after emacspeak pre act comp)
  "speak char that was inserted."
  (when (and emacspeak-character-echo
             (interactive-p ))
    (when dtk-stop-immediately-while-typing (dtk-stop))
    (emacspeak-speak-this-char last-input-char )))

(defadvice org-delete-char (around emacspeak pre act)
  "Speak character you're deleting."
  (cond
   ((interactive-p )
    (dtk-tone 500 30 'force)
    (and emacspeak-delete-char-speak-deleted-char
         (emacspeak-speak-char t))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice org-return (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-line)))

;;}}}
;;{{{ mode hook:

(defun emacspeak-org-mode-setup ()
  "Placed on org-mode-hook to do Emacspeak setup."
  (unless emacspeak-audio-indentation
    (emacspeak-toggle-audio-indentation)))

(add-hook 'org-mode-hook 'emacspeak-org-mode-setup)

;;}}}
;;{{{ fix misc commands:
(loop for f in
      '(org-occur
        org-next-link org-previous-link
        org-insert-heading org-insert-todo-heading)
      do
      (eval
       `(defadvice ,f (around emacspeak pre act comp)
          "Avoid outline errors bubbling up."
          (cond
           ((interactive-p)
            (ems-with-errors-silenced ad-do-it))
           (t ad-do-it))
          ad-return-value)))

;;}}}
;;{{{ global input wizard

;;;###autoload
(defun emacspeak-org-popup-input ()
  "Pops up an org input area."
  (interactive)
  (emacspeak-wizards-popup-input-buffer 'org-mode))

;;}}}
(provide 'emacspeak-org)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

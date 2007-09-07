;;; emacspeak-org.el --- Speech-enable org
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak front-end for ORG
;;; Keywords: Emacspeak, org
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date$ |
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

;;{{{  Introduction:

;;; Commentary:
;;; Speech-enable org ---
;;;  Org allows you to keep organized notes and todo lists.
;;; Homepage: http://www.astro.uva.nl/~dominik/Tools/org/
;;; or http://orgmode.org/
;;; 
;;; Code:

;;}}}
;;{{{ required modules

(require 'emacspeak-preamble)
                                        ;(require 'emacspeak-redefine)

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
   (org-headline-done voice-monotone-medium)
   (org-link voice-bolden)
   (org-date voice-animate)
   (org-tag voice-smoothen)
   (org-todo voice-bolden-and-animate)
   (org-done voice-monotone)
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
(defadvice org-cycle-list-bullet (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(loop for f in
      '(org-cycle org-shifttab)
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

(defadvice org-overview (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (message "Showing top-level overview.")))

(defadvice org-content (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (message "Showing table of contents.")))

(defadvice org-tree-to-indirect-buffer(after emacspeak pre act
                                             comp)
  "Provide spoken feedback."
  (when (interactive-p)
    (message "Cloned %s"
             (save-excursion
               (set-buffer org-last-indirect-buffer)
               (goto-char (point-min))
               (buffer-substring
                (line-beginning-position)
                (line-end-position))))))
;;}}}
;;{{{ Header insertion and relocation

(loop for f in
      '(
        org-insert-heading org-insert-todo-heading
        org-insert-subheading org-insert-todo-subheading
        org-promote-subtree org-demote-subtree
        org-do-promote org-do-demote
        org-move-subtree-up org-move-subtree-down
        org-convert-to-odd-levels org-convert-to-oddeven-levels
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
      '(
        org-cut-subtree org-copy-subtree
        org-paste-subtree org-archive-subtree
        org-narrow-to-subtree )
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
    (if (> (point) prior)
        (tts-with-punctuations
         'all
         (if (> (length (emacspeak-get-minibuffer-contents)) 0)
             (dtk-speak (emacspeak-get-minibuffer-contents))
           (emacspeak-speak-line)))
      (emacspeak-speak-completions-if-available))
    ad-return-value))

;;}}}
;;{{{ toggles:

(loop for f in
      '(
        org-toggle-archive-tag org-toggle-comment)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide spoken feedback."
          (when (interactive-p)
            (emacspeak-auditory-icon 'button)
            (emacspeak-speak-line)))))

;;}}}
;;{{{ ToDo:

;;}}}
;;{{{ timestamps and calendar:

(loop for f in
      '(
        org-timestamp-down org-timestamp-down-day
        org-timestamp-up org-timestamp-up-day)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (interactive-p)
            (emacspeak-auditory-icon 'select-object)
            (emacspeak-speak-line)))))

(defadvice org-eval-in-calendar (after emacspeak pre act comp)
  "Speak what is returned."
  (dtk-speak org-ans2))

;;}}}
;;{{{ Agenda:

;;; agenda navigation

(loop for f in
      '(
        org-agenda-next-date-line org-agenda-previous-date-line
        org-agenda-goto-today
        )
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (interactive-p)
            (emacspeak-auditory-icon 'select-object)
            (emacspeak-speak-line)))))
            
(loop for f in
      '(
        org-agenda-quit org-agenda-exit)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (interactive-p)
            (emacspeak-auditory-icon 'close-object)
            (emacspeak-speak-mode-line)))))
(loop for f in
      '(
        org-agenda-goto org-agenda-show org-agenda-switch-to)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (interactive-p)
            (emacspeak-auditory-icon 'open-object)
            (emacspeak-speak-line)))))

(defadvice org-agenda (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))
                                  
;;}}}
;;{{{ misc file commands:

(defadvice org-end-of-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (dtk-stop)
    (emacspeak-auditory-icon 'select-object)))

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
;;{{{ org-goto fixup:

(loop for f in
      '(org-metadown org-metaup org-metaleft org-metaright)
      do
      (eval
       `(defadvice ,f(after emacspeak pre act comp)
          "Provide spoken feedback."
          (when (interactive-p)
            (emacspeak-speak-line)
            (emacspeak-auditory-icon 'yank-object)))))

(defun emacspeak-org-update-keys ()
  "Update keys in org mode."
  (declare (special org-goto-map org-mode-map))
  (loop for k in
        '(
          ([(shift tab)]    org-shifttab)
          ([(shift up)] org-shiftup)
          ([(shift down)] org-shiftdown)
          ([(shift left)] org-shiftleft)
          ([(shift right)] org-shiftright)
	  ([(meta shift return)] org-insert-todo-heading)
          ([(meta shift down)] org-shiftmetadown)
          ([(meta shift up)] org-shiftmetaup)
          ([(meta shift left)] org-shiftmetaleft)
          ([(meta shift right)] org-shiftmetaright)
          ([(meta shift return)] org-insert-todo-heading)
          ("\C-j" org-insert-heading)
          ("\M-n" org-next-item)
          ("\M-p" org-previous-item)
          )
        do
        (emacspeak-keymap-update  org-mode-map k))
  (loop for k in
        '(
          ( "\C-e" emacspeak-prefix-command)
          ( "\C-h" help-command))
        do
        (emacspeak-keymap-update  org-goto-map k)))

(add-hook 'org-mode-hook 'emacspeak-org-update-keys)

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
    (emacspeak-speak-char t)
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
  (declare (special org-mode-map))
  (unless emacspeak-audio-indentation
    (emacspeak-toggle-audio-indentation))
  (define-key org-mode-map
    emacspeak-prefix'emacspeak-prefix-command)
  (define-key org-mode-map
    (concat emacspeak-prefix "e")
    'org-end-of-line))

(add-hook 'org-mode-hook 'emacspeak-org-mode-setup)

(defadvice org-toggle-checkbox (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'button)
    (emacspeak-speak-line)))

;;}}}
;;{{{ fix misc commands:

(loop for f in
      '(
        org-occur org-next-link org-previous-link
        org-beginning-of-item
        org-beginning-of-item-list
        org-back-to-heading
        org-insert-heading org-insert-todo-heading)
      do
      (eval
       `(defadvice ,f (around emacspeak pre act comp)
          "Avoid outline errors bubbling up."
          
          (ems-with-errors-silenced ad-do-it))))
           
          

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

;;; emacspeak-org.el --- Speech-enable org
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak front-end for ORG
;;; Keywords: Emacspeak, org
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2008-03-22 18:58:40 -0700 (Sat, 22 Mar 2008) $ |
;;;  $Revision: 4347 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1999, 2011 T. V. Raman <raman@cs.cornell.edu>
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
   (org-date voice-animate)
   (org-done voice-monotone)
   (org-formula voice-animate-extra)
   (org-headline-done voice-monotone-medium)
   (org-level-1 voice-bolden-medium)
   (org-level-2 voice-bolden)
   (org-level-3 voice-animate)
   (org-level-4 voice-monotone)
   (org-level-5 voice-smoothen)
   (org-level-6 voice-lighten)
   (org-level-7 voice-lighten-medium)
   (org-level-8 voice-lighten-extra)
   (org-link voice-bolden)
   (org-scheduled-previously voice-lighten-medium)
   (org-scheduled-today voice-bolden-extra)
   (org-special-keyword voice-lighten-extra)
   (org-table voice-bolden-medium)
   (org-tag voice-smoothen)
   (org-time-grid voice-bolden)
   (org-todo voice-bolden-and-animate)
   (org-warning voice-bolden-and-animate)
   (org-agenda-calendar-event voice-animate-extra)
   (org-agenda-calendar-sexp voice-animate)
   (org-agenda-column-dateline voice-monotone)
   (org-agenda-diary voice-animate)
   (org-agenda-dimmed-todo-face voice-smoothen-medium)
   (org-agenda-done voice-monotone)
   (org-agenda-filter-category voice-lighten-extra)
   (org-agenda-filter-tags voice-lighten)
   (org-agenda-restriction-lock voice-monotone)
   (org-agenda-structure voice-bolden)
   (org-archived voice-monotone)
   (org-beamer-tag voice-bolden)
   (org-block voice-monotone)
   (org-block-background voice-monotone)
   (org-checkbox voice-animate)
   (org-clock-overlay voice-animate)
   (org-code voice-monotone)
   (org-column voice-lighten)
   (org-column-title voice-lighten-extra)
   (org-date-selected voice-bolden)
   (org-default voice-smoothen)
   (org-document-info voice-bolden-medium)
   (org-document-info-keyword voice-bolden-extra)
   (org-document-title voice-bolden)
   (org-drawer voice-smoothen-medium)
   (org-ellipsis voice-smoothen-extra)
   (org-footnote voice-smoothen)
   (org-habit-alert-face voice-monotone)
   (org-habit-alert-future-face voice-monotone)
   (org-habit-clear-face voice-monotone)
   (org-habit-clear-future-face voice-monotone)
   (org-habit-overdue-face voice-monotone)
   (org-habit-overdue-future-face voice-monotone)
   (org-habit-ready-future-face voice-monotone)
   (org-hide voice-smoothen-extra)
   (org-indent voice-smoothen)
   (org-inlinetask voice-smoothen-extra)
   (org-latex-and-export-specials voice-monotone)
   (org-meta-line voice-smoothen-medium)
   (org-property-value voice-animate)
   (org-scheduled voice-animate)
   (org-sexp-date voice-monotone)
   (org-target voice-bolden-medium)
   (org-upcoming-deadline voice-animate)
   (org-verbatim voice-monotone)
   (org-habit-ready-face voice-monotone)))

;;}}}
;;{{{ Structure Navigation:

(loop for f in
      '(org-mark-ring-goto org-mark-ring-push
                           org-forward-heading-same-level org-backward-heading-same-level
                           org-next-link org-previous-link org-open-at-point
                           org-goto  org-goto-ret
                           org-goto-left org-goto-right
                           org-goto-quit
                           org-next-item org-previous-item
                           org-metaleft org-metaright org-metaup org-metadown
                           org-meta-return
                           org-shiftmetaleft org-shiftmetaright org-shiftmetaup org-shiftmetadown
                           org-mark-element org-mark-subtree
                           org-agenda-forward-block org-agenda-backward-block
                           )
      do
      (eval
       `(defadvice ,f(after emacspeak pre act comp)
          "Provide spoken feedback."
          (when (ems-interactive-p )
            (emacspeak-speak-line)
            (emacspeak-auditory-icon 'large-movement)))))

(defadvice org-cycle-list-bullet (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(loop for f in
      '(org-cycle org-shifttab)
      do
      (eval
       `(defadvice ,f(after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p )
            (cond
             ((org-at-table-p 'any)
              (emacspeak-org-table-speak-current-element))
             (t
              (emacspeak-speak-line)
              (emacspeak-auditory-icon 'select-object)))))))xrjp

(defadvice org-overview (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (message "Showing top-level overview.")))

(defadvice org-content (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (message "Showing table of contents.")))

(defadvice org-tree-to-indirect-buffer(after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (ems-interactive-p )
    (message "Cloned %s"
             (with-current-buffer org-last-indirect-buffer
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
          (when (ems-interactive-p )
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
          (when (ems-interactive-p )
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
          (when (ems-interactive-p )
            (emacspeak-auditory-icon 'button)
            (emacspeak-speak-line)))))

;;}}}
;;{{{ ToDo:

;;}}}
;;{{{ timestamps and calendar:

(loop for f in
      '(org-timestamp-down-day org-timestamp-up-day)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p )
            (emacspeak-auditory-icon 'select-object)
            (emacspeak-speak-line)))))

(loop for f in
      '(org-timestamp-down org-timestamp-up)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p )
            (emacspeak-auditory-icon 'select-object)
            (dtk-speak org-last-changed-timestamp)))))

(defadvice org-eval-in-calendar (after emacspeak pre act comp)
  "Speak what is returned."
  (declare (special org-ans2))
  (dtk-speak org-ans2))

;;}}}
;;{{{ Agenda:

;;; AGENDA NAVIGATION

(loop for f in
      '(
        org-agenda-next-date-line org-agenda-previous-date-line
                                  org-agenda-next-line org-agenda-previous-line
                                  org-agenda-goto-today
                                  )
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p )
            (emacspeak-auditory-icon 'select-object)
            (emacspeak-speak-line)))))

(loop for f in
      '(
        org-agenda-quit org-agenda-exit)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p )
            (emacspeak-auditory-icon 'close-object)
            (emacspeak-speak-mode-line)))))
(loop for f in
      '(
        org-agenda-goto org-agenda-show org-agenda-switch-to)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p )
            (emacspeak-auditory-icon 'open-object)
            (emacspeak-speak-line)))))

(defadvice org-agenda (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

;;}}}
;;{{{ tables:

;;}}}
;;{{{ table minor mode:

(defadvice orgtbl-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon
     (if orgtbl-mode 'on 'off))
    (message "Turned %s org table mode."
             (if orgtbl-mode 'on 'off))))

;;}}}
;;{{{ import/export:
;;; Work In Progress:
;;; Need to be able to use Emacspeak keys within the dispatcher.

                                        ;(defadvice org-export--dispatch-action (around emacspeak (prompt allowed-keys entries options first-key expertp)  pre act comp)
                                        ;"Enable Emacspeak keys."
                                        ;(ad-set-argument '(prompt allowed-keys entries options first-key expertp) 1  (pushnew  5 (ad-get-arg 1)))
                                        ;ad-do-it)

;;}}}
;;{{{ org-goto fixup:

(loop for f in
      '(org-metadown org-metaup org-metaleft org-metaright)
      do
      (eval
       `(defadvice ,f(after emacspeak pre act comp)
          "Provide spoken feedback."
          (when (ems-interactive-p )
            (emacspeak-speak-line)
            (emacspeak-auditory-icon 'yank-object)))))

(defun emacspeak-org-update-keys ()
  "Update keys in org mode."
  (declare (special  org-mode-map))
  (loop for k in
        '(
          ("C-e" emacspeak-prefix-command)
          ("C-j" org-insert-heading)
          ("M-<down>" org-metadown)
          ("M-<left>"  org-metaleft)
          ("M-<right>" org-metaright)
          ("M-<up>" org-metaup)
          ("M-RET" org-meta-return)
          ("M-S-<down>" org-shiftmetadown)
          ("M-S-<left>" org-shiftmetaleft)
          ("M-S-<right>" org-shiftmetaright)
          ("M-S-<up>" org-shiftmetaup)
          ("M-S-RET" org-insert-todo-heading)
          ("M-n" org-next-item)
          ("M-p" org-previous-item)
          ("S-<down>" org-shiftdown)
          ("S-<left>" org-shiftleft)
          ("S-<right>" org-shiftright)
          ("S-<up>" org-shiftup)
          ("S-TAB" org-shifttab)
          )
        do
        (emacspeak-keymap-update  org-mode-map k))
  )

(add-hook 'org-mode-hook 'emacspeak-org-update-keys)

;;}}}
;;{{{ deleting chars:

(defadvice org-delete-backward-char (around emacspeak pre act)
  "Speak character you're deleting."
  (cond
   ((ems-interactive-p  )
    (dtk-tone 500 30 'force)
    (emacspeak-speak-this-char (preceding-char ))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice org-force-self-insert (after emacspeak pre act comp)
  "speak char that was inserted."
  (when (and emacspeak-character-echo
             (ems-interactive-p  ))
    (dtk-stop)
    (emacspeak-speak-this-char last-input-event )))

(defadvice org-delete-char (around emacspeak pre act)
  "Speak character you're deleting."
  (cond
   ((ems-interactive-p  )
    (dtk-tone 500 30 'force)
    (emacspeak-speak-char t)
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice org-return (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (cond
     ((org-at-table-p 'any)
      (emacspeak-org-table-speak-current-element))
     (t
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'select-object)))))

;;}}}
;;{{{ mode hook:

(defun emacspeak-org-mode-setup ()
  "Placed on org-mode-hook to do Emacspeak setup."
  (declare (special org-mode-map))
  (unless emacspeak-audio-indentation (emacspeak-toggle-audio-indentation))
  (when (fboundp 'org-end-of-line)
    (define-key org-mode-map emacspeak-prefix  'emacspeak-prefix-command)))

(add-hook 'org-mode-hook 'emacspeak-org-mode-setup)

;;; advice end-of-line here to call org specific action
(defadvice end-of-line (after emacspeak-org pre act comp)
  "Call org specific actions in org mode."
  (when (and (ems-interactive-p )
             (eq major-mode 'org-mode)
             (fboundp 'org-end-of-line))
    (org-end-of-line)))

(defadvice org-toggle-checkbox (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'button)
    (emacspeak-speak-line)))

;;}}}
;;{{{ fix misc commands:
(defadvice org-end-of-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (dtk-stop)
    (emacspeak-auditory-icon 'select-object)))

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
;;{{{ org capture

(defcustom emacspeak-org-bookmark-key "h"
  "Key of template used for capturing  hot list."
  :type 'string
  :group 'emacspeak-org)

;;;###autoload
(defun emacspeak-org-bookmark (&optional goto)
  "Bookmark from org."
  (interactive "P")
  (declare (special emacspeak-org-bookmark-key))
  (org-capture goto emacspeak-org-bookmark-key))

(defadvice org-capture-goto-last-stored (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p )
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice org-capture-goto-target (after emacspeak pre act comp)
  "Provide auditory feedback."
  (emacspeak-auditory-icon 'large-movement)
  (emacspeak-speak-line))

(defadvice org-capture-finalize (after emacspeak pre act comp)
  "Provide auditory feedback."
  (emacspeak-auditory-icon 'save-object))

(defadvice org-capture-kill (after emacspeak pre act comp)
  "Provide auditory feedback."
  (emacspeak-auditory-icon 'close-object))

;;;###autoload
(defun emacspeak-org-table-speak-current-element ()
  "echoes current table element"
  (interactive)
  (dtk-speak-and-echo (org-table-get-field)))
(loop
 for f in
 '(org-table-next-field org-table-previous-field)
 do
 (eval
  `(defadvice ,f  (after emacspeak pre act comp)
     "Provide auditory feedback."
     (emacspeak-org-table-speak-current-element))))

;;}}}
(provide 'emacspeak-org)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}

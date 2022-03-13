;;; emacspeak-org.el --- Speech-enable org  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak front-end for ORG
;;; Keywords: Emacspeak, org
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2008-03-22 18:58:40 -0700 (Sat, 22 Mar 2008) $ |
;;;  $Revision: 4347 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1995 -- 2021, T. V. Raman
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

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'org "org" 'no-error)
(require 'org-table "org-table" 'no-error)
(defvar org-ans2 nil)

;;}}}
;;{{{ voice locking:

(voice-setup-add-map
 '(
   (org-date voice-animate)
   (org-done voice-monotone-extra)
   (org-formula voice-animate-extra)
   (org-headline-done voice-monotone-medium)
   (org-level-1 voice-bolden-medium)
   (org-level-2 voice-bolden)
   (org-level-3 voice-animate)
   (org-level-4 voice-monotone-extra)
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
   (org-agenda-column-dateline voice-monotone-extra)
   (org-agenda-diary voice-animate)
   (org-agenda-dimmed-todo-face voice-smoothen-medium)
   (org-agenda-done voice-monotone-extra)
   (org-agenda-filter-category voice-lighten-extra)
   (org-agenda-filter-tags voice-lighten)
   (org-agenda-restriction-lock voice-monotone-extra)
   (org-agenda-structure voice-bolden)
   (org-archived voice-monotone-extra)
   (org-beamer-tag voice-bolden)
   (org-block voice-monotone-extra)
   (org-checkbox voice-animate)
   (org-coverlay voice-animate)
   (org-code voice-monotone-extra)
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
   (org-habit-alert-face voice-monotone-extra)
   (org-habit-alert-future-face voice-monotone-extra)
   (org-habit-clear-face voice-monotone-extra)
   (org-habit-clear-future-face voice-monotone-extra)
   (org-habit-overdue-face voice-monotone-extra)
   (org-habit-overdue-future-face voice-monotone-extra)
   (org-habit-ready-future-face voice-monotone-extra)
   (org-hide voice-smoothen-extra)
   (org-indent voice-smoothen)
   (org-inlinetask voice-smoothen-extra)
   (org-meta-line voice-smoothen-medium)
   (org-property-value voice-animate)
   (org-scheduled voice-animate)
   (org-sexp-date voice-monotone-extra)
   (org-target voice-bolden-medium)
   (org-upcoming-deadline voice-animate)
   (org-verbatim voice-monotone-extra)
   (org-habit-ready-face voice-monotone-extra)))

;;}}}
;;{{{ Structure Navigation:

(cl-loop
 for f in
 '(
   org-mark-ring-goto org-mark-ring-push
   org-next-visible-heading org-previous-visible-heading
   org-forward-heading-same-level org-backward-heading-same-level
   org-backward-sentence org-forward-sentence
   org-backward-element org-forward-element
   org-backward-paragraph org-forward-paragraph
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
     "Speak."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'paragraph)))))

(defadvice org-cycle-list-bullet (after emacspeak pre act comp)
  "Speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'item)
    (emacspeak-speak-line)))

(defcustom emacspeak-org-table-after-movement-function
  #'emacspeak-org-table-speak-current-element
  "The function to call after moving in a table"
  :type
  '(choice
    (const :tag "speak cell contents only" emacspeak-org-table-speak-current-element)
    (const :tag "speak column header" emacspeak-org-table-speak-column-header)
    (const :tag "speak row header" emacspeak-org-table-speak-row-header)
    (const :tag "speak cell contents and column header" emacspeak-org-table-speak-column-header-and-element)
    (const :tag "speak cell contents and row header" emacspeak-org-table-speak-row-header-and-element)
    (const :tag "speak column contents and both headers" emacspeak-org-table-speak-both-headers-and-element))
  :group 'emacspeak-org)

;;; orgalist-mode defines structured navigators that in turn call org-cycle.
;;; Removing itneractive check in advice for org-cycle
;;; to speech enable all such nav commands.
;;; Note that org itself produces the folded state via org-unlogged-message
;;; Which gets spoken by Emacspeak
(cl-loop
 for f in
 '(org-cycle org-shifttab)
 do
 (eval
  `(defadvice ,f(after emacspeak pre act comp)
     "speak."
     (cond
      ((org-at-table-p 'any)
       (funcall emacspeak-org-table-after-movement-function))
      (t
       (let ((dtk-stop-immediately nil))
         (when (ems-interactive-p)
           (emacspeak-speak-line))))))))

(defadvice org-overview (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (message "Showing top-level overview.")))

(defadvice org-content (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (message "Showing table of contents.")))

(defadvice org-tree-to-indirect-buffer(after emacspeak pre act comp)
  "Speak."
  (when (ems-interactive-p)
    (message "Cloned %s"
             (with-current-buffer org-last-indirect-buffer
               (goto-char (point-min))
               (buffer-substring
                (line-beginning-position)
                (line-end-position))))))

;;}}}
;;{{{ Header insertion and relocation

(cl-loop
 for f in
 '(
   org-delete-indentation
   org-insert-heading org-insert-todo-heading
   org-promote-subtree org-demote-subtree
   org-do-promote org-do-demote
   org-move-subtree-up org-move-subtree-down
   org-convert-to-odd-levels org-convert-to-oddeven-levels
   )
 do
 (eval
  `(defadvice ,f(after emacspeak pre act comp)
     "Speak."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'open-object)))))

;;}}}
;;{{{ cut and paste:

(cl-loop
 for f in
 '(
   org-cut-subtree org-copy-subtree
   org-paste-subtree org-archive-subtree
   org-narrow-to-subtree)
 do
 (eval
  `(defadvice ,f(after emacspeak pre act comp)
     "Speak."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'yank-object)))))

;;}}}
;;{{{ completion:

(defadvice org-complete (around emacspeak pre act comp)
  "Say what you completed."
  (let ((prior (save-excursion (skip-syntax-backward "^ >") (point)))
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

(cl-loop
 for f in
 '(
   org-toggle-archive-tag org-toggle-comment)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'button)
       (emacspeak-speak-line)))))

;;}}}
;;{{{ ToDo:

;;}}}
;;{{{ timestamps and calendar:

(cl-loop
 for f in
 '(org-timestamp-down-day org-timestamp-up-day)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'select-object)
       (emacspeak-speak-line)))))

(cl-loop for f in
         '(org-timestamp-down org-timestamp-up)
         do
         (eval
          `(defadvice ,f (after emacspeak pre act comp)
             "speak."
             (when (ems-interactive-p)
               (emacspeak-auditory-icon 'select-object)
               (dtk-speak org-last-changed-timestamp)))))

(defadvice org-eval-in-calendar (after emacspeak pre act comp)
  "Speak what is returned."
  (cl-declare (special org-ans2))
  (dtk-speak org-ans2))

;;}}}
;;{{{ Agenda:

;;; AGENDA NAVIGATION

(cl-loop
 for f in
 '(
   org-agenda-next-date-line org-agenda-previous-date-line
   org-agenda-next-line org-agenda-previous-line
   org-agenda-goto-today
   )
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'select-object)
       (emacspeak-speak-line)))))

(cl-loop
 for f in
 '(org-agenda-quit org-agenda-exit)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'close-object)
       (emacspeak-speak-mode-line)))))

(cl-loop
 for f in
 '(org-agenda-goto org-agenda-show org-agenda-switch-to)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)
       (emacspeak-speak-line)))))

(defadvice org-agenda (after emacspeak pre act comp)
  "Speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

;;}}}
;;{{{ tables:

;;}}}
;;{{{ table minor mode:

(defadvice orgtbl-mode (after emacspeak pre act comp)
  "speak."
  (cl-declare (special orgtbl-mode))
  (when (ems-interactive-p)
    (emacspeak-auditory-icon (if orgtbl-mode 'on 'off))
    (message "Turned %s org table mode."
             (if orgtbl-mode 'on 'off))))

;;}}}
;;{{{ deleting chars:

(defadvice org-delete-backward-char (around emacspeak pre act comp)
  "Speak character you're deleting."
  (cond
   ((ems-interactive-p)
    (dtk-tone-deletion)
    (emacspeak-speak-this-char (preceding-char))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice org-delete-char (around emacspeak pre act comp)
  "Speak character you're deleting."
  (cond
   ((ems-interactive-p)
    (dtk-tone-deletion)
    (emacspeak-speak-char t)
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice org-return (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (cond
     ((org-at-table-p 'any)
      (funcall emacspeak-org-table-after-movement-function))
     (t
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'select-object)))))

;;}}}
;;{{{ Keymap update:

(defun emacspeak-org-update-keys ()
  "Update keys in org mode."
  (cl-declare (special  org-mode-map))
  (cl-loop
   for k in
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
     ("S-RET" org-table-previous-row)
     ("M-n" org-next-item)
     ("M-p" org-previous-item)
     ("S-<down>" org-shiftdown)
     ("S-<left>" org-shiftleft)
     ("S-<right>" org-shiftright)
     ("S-<up>" org-shiftup)
     ("S-TAB" org-shifttab))
   do
   (emacspeak-keymap-update  org-mode-map k)))

;;}}}
;;{{{ mode hook:

(defun emacspeak-org-mode-setup ()
  "Placed on org-mode-hook to do Emacspeak setup."
  (cl-declare (special org-mode-map
                       org-link-parameters))
  (emacspeak-org-update-keys)
  (define-key org-mode-map (ems-kbd "C-o e") 'tvr-org-enumerate)
  (define-key org-mode-map (ems-kbd "C-o i") 'tvr-org-itemize)
  (define-key outline-minor-mode-map (ems-kbd "C-o i") 'tvr-org-itemize)
  (define-key outline-minor-mode-map (ems-kbd "C-o e") 'tvr-org-enumerate)
  (when (fboundp 'org-end-of-line)
    (define-key org-mode-map emacspeak-prefix  'emacspeak-prefix-command)
    (emacspeak-setup-programming-mode)
    (when dtk-caps (dtk-toggle-caps))))

(add-hook 'org-mode-hook #'emacspeak-org-mode-setup)

;;; advice end-of-line here to call org specific action
(defadvice end-of-line (after emacspeak-org pre act comp)
  "Call org specific actions in org mode."
  (when (and (ems-interactive-p)
             (eq major-mode 'org-mode)
             (fboundp 'org-end-of-line))
    (org-end-of-line)))

(defadvice org-toggle-checkbox (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'button)
    (emacspeak-speak-line)))

;;}}}
;;{{{ fix misc commands:

(cl-loop
 for f in
 '(
   org-occur org-beginning-of-line org-end-of-line
   org-beginning-of-item org-beginning-of-item-list)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p) (emacspeak-speak-line)
           (emacspeak-auditory-icon 'select-object)))))

;;}}}
;;{{{ global input wizard


(defun emacspeak-org-popup-input ()
  "Pops up an org input area."
  (interactive)
  (emacspeak-org-popup-input-buffer 'org-mode))

;;}}}
;;{{{ org capture

(defadvice org-capture-goto-last-stored (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice org-capture-goto-target (after emacspeak pre act comp)
  "speak."
  (emacspeak-auditory-icon 'large-movement)
  (emacspeak-speak-line))

(defadvice org-capture-finalize (after emacspeak pre act comp)
  "speak."
  (emacspeak-auditory-icon 'save-object))

(defadvice org-capture-kill (after emacspeak pre act comp)
  "speak."
  (emacspeak-auditory-icon 'close-object))


(defun emacspeak-org-table-speak-current-element ()
  "echoes current table element"
  (interactive)
  (let ((field (org-table-get-field)))
    (cond
     ((string-match "^ *$" field) (dtk-speak "space"))
     (t (dtk-speak-and-echo field)))))


(defun emacspeak-org-table-speak-column-header ()
  "echoes column header"
  (interactive)
  (dtk-speak-and-echo
   (propertize (org-table-get 1 nil) 'face 'bold)))


(defun emacspeak-org-table-speak-row-header ()
  "echoes row header"
  (interactive)
  (dtk-speak-and-echo
   (propertize (org-table-get nil 1) 'face 'italic)))


(defun emacspeak-org-table-speak-coordinates ()
  "echoes coordinates"
  (interactive)
  (dtk-speak-and-echo
   (concat "row " (number-to-string (org-table-current-line))
           ", column " (number-to-string (org-table-current-column)))))


(defun emacspeak-org-table-speak-both-headers-and-element ()
  "echoes both row and col headers."
  (interactive)
  (dtk-speak-and-echo
   (concat
    (propertize (org-table-get nil 1) 'face 'italic)
    " "
    (propertize (org-table-get  1 nil) 'face 'bold) " "
    (org-table-get-field))))


(defun emacspeak-org-table-speak-row-header-and-element ()
  "echoes row header and element"
  (interactive)
  (dtk-speak-and-echo
   (concat
    (propertize (org-table-get nil 1) 'face 'italic)
    " "
    (org-table-get-field))))


(defun emacspeak-org-table-speak-column-header-and-element ()
  "echoes col header and element"
  (interactive)
  (dtk-speak-and-echo
   (concat
    (propertize (org-table-get  1 nil) 'face 'bold)
    " "
    (org-table-get-field))))

(cl-loop
 for f in
 '(org-table-next-field org-table-previous-field
                        org-table-next-row org-table-previous-row)
 do
 (eval
  `(defadvice ,f  (after emacspeak pre act comp)
     "speak."
     (funcall emacspeak-org-table-after-movement-function))))

;;}}}
;;{{{ Additional table function:


(unless (fboundp 'org-table-previous-row)
  (defun org-table-previous-row ()
    "Go to the previous row (same column) in the current table.
Before doing so, re-align the table if necessary."
    (interactive)
    (org-table-maybe-eval-formula)
    (org-table-maybe-recalculate-line)
    (if (or (looking-at "[ \t]*$")
            (save-excursion (skip-chars-backward " \t") (bolp)))
        (newline)
      (if (and org-table-automatic-realign
               org-table-may-need-update)
          (org-table-align))
      (let ((col (org-table-current-column)))
        (beginning-of-line 0)
        (when (or (not (org-at-table-p)) (org-at-table-hline-p))
          (beginning-of-line 1))
        (org-table-goto-column col)
        (skip-chars-backward "^|\n\r")
        (if (looking-at " ") (forward-char 1))))))

;;}}}
;;{{{ Capture
;;;###autoload
(defun emacspeak-org-capture-link (&optional open)
  "Capture hyperlink to current context.
To use this command, first do `customize-variable'
`org-capture-template' and assign letter `h' to a template that
creates the hyperlink on capture.  Optional interactive prefix
arg just opens the file"
  (interactive "P")
  (require 'org)
  (require 'ol-eww)
  (cond
   (open
    (funcall-interactively #'find-file (expand-file-name "~/.org/hotlist.org")))
   (t
    (org-store-link nil)
    (org-capture nil "h"))))

(declare-function emacspeak-eww-current-title "emacspeak-eww" nil)

;;}}}
;;{{{ Speech-enable export prompt:
(defadvice org-export--dispatch-action (before emacspeak pre act comp)
  "Speak prompt intelligently."
  (let ((prompt (ad-get-arg 0))
        (entries (ad-get-arg 2))
        (first-key (ad-get-arg 4))
        (choices nil))
    (setq choices
          (cond
           ((null first-key) entries)
           (t                           ;third  is cl-caddr
            (cl-caddr (assoc first-key entries)))))
    (dtk-notify-speak
     (mapconcat
      #'(lambda (e)
          (format "%c: %s\n" (cl-first e) (cl-second e)))
      choices "\n"))
    (sit-for 5)))

;;}}}
;;{{{ Preview HTML With EWW:

(defun emacspeak-org-eww-file (file _link)
  "Preview HTML files with EWW from exporter."
  (add-hook 'emacspeak-eww-post-process-hook  #'emacspeak-speak-buffer)
  (funcall-interactively #'eww-open-file file))

;;}}}
;;{{{ Edit Special Advice:

(cl-loop
 for f in
 '(org-edit-src-exit org-edit-src-abort)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'close-object)
       (emacspeak-speak-line)))))

(cl-loop
 for f in
 '(org-edit-src-code org-edit-special org-switchb) do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)
       (emacspeak-speak-mode-line)))))

;;}}}
;;{{{ Fillers:

(defadvice org-fill-paragraph (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'fill-object)
    (message "Filled current paragraph")))

(defadvice org-todo (after emacspeak pre act comp)
  "speak when changing the state of a TODO item."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'button)
    (let ((state (org-get-todo-state)))
      (if (null state)
          (message "State unset")
        (message state)))))

;;}}}
;;{{{TVR: Conveniences

(defun tvr-org-itemize ()
  "Start a numbered  list."
  (interactive)
  (forward-line 0)
  (insert "  -  ")
  (emacspeak-speak-line)
  (emacspeak-auditory-icon 'item))

(defun tvr-org-enumerate ()
  "Start a numbered  list."
  (interactive)
  (forward-line 0)
  (insert "  1.  ")
  (emacspeak-speak-line)
  (emacspeak-auditory-icon 'item))


;;}}}
;;{{{ specialized input buffers:

;;; Taken from a message on the org mailing list.


(defun emacspeak-org-popup-input-buffer (mode)
  "Provide an input buffer in a specified mode."
  (interactive
   (list
    (intern
     (completing-read
      "Mode: "
      (mapcar
       #'(lambda (e)
         (list (symbol-name e)))
              (apropos-internal "-mode$" 'commandp))
      nil t))))
  (let ((buffer-name (generate-new-buffer-name "*input*")))
    (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
    (narrow-to-region (point) (point))
    (funcall mode)
    (let ((map (copy-keymap (current-local-map))))
      (define-key map (ems-kbd "C-c C-c")
        #'(lambda ()
          (interactive)
          (kill-buffer nil)
          (delete-window)))
      (use-local-map map))
    (shrink-window-if-larger-than-buffer)))

;;}}}
(provide 'emacspeak-org)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

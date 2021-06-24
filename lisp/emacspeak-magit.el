;;; emacspeak-magit.el --- Speech-enable MAGIT: Git Client  -*- lexical-binding: t; -*-
;;; $Id: emacspeak-magit.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable MAGIT An Emacs Interface to magit
;;; Keywords: Emacspeak,  Audio Desktop magit
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2018, T. V. Raman
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
;;; MERCHANTABILITY or FITNMAGIT FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; MAGIT ==  Git interface in Emacs
;;; git clone git://github.com/magit/magit.git

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)


;;}}}
;;{{{ Map voices to faces:

(voice-setup-add-map
 '(
   (magit-blame-dimmed voice-smoothen)
   (magit-blame-highlight voice-brighten)
   (magit-branch-remote-head voice-bolden)
   (magit-branch-upstream voice-animate)
   (magit-diff-revision-summary voice-monotone-extra)
   (magit-diff-revision-summary-highlight voice-animate-extra)
   (magit-diff-whitespace-warning voice-monotone-medium)
   (magit-header-line-key voice-bolden-extra)
   (magit-header-line-log-select voice-animate)
   (magit-keyword-squash voice-monotone-extra)
   (magit-bisect-bad voice-animate)
   (magit-bisect-good voice-lighten)
   (magit-bisect-skip voice-monotone-extra)
   (magit-blame-date voice-bolden-and-animate)
   (magit-blame-hash voice-monotone-extra)
   (magit-blame-heading voice-bolden)
   (magit-blame-name voice-animate)
   (magit-blame-summary voice-lighten)
   (magit-branch-current voice-lighten)
   (magit-branch-local voice-brighten)
   (magit-branch-remote voice-smoothen)
   (magit-cherry-equivalent voice-lighten)
   (magit-cherry-unmatched voice-bolden)
   (magit-diff-added voice-animate-extra)
   (magit-diff-added-highlight voice-animate)
   (magit-diff-added-highlight voice-animate-extra)
   (magit-diff-base voice-annotate)
   (magit-diff-base-highlight voice-animate)
   (magit-diff-conflict-heading voice-bolden-extra)
   (magit-diff-context voice-monotone-extra)
   (magit-diff-context-highlight voice-brighten)
   (magit-diff-file-heading voice-brighten)
   (magit-diff-file-heading-highlight voice-bolden-extra)
   (magit-diff-file-heading-selection voice-lighten)
   (magit-diff-hunk-heading voice-bolden)
   (magit-diff-hunk-heading-highlight voice-brighten)
   (magit-diff-hunk-heading-selection voice-lighten)
   (magit-diff-hunk-region voice-smoothen)
   (magit-diff-lines-boundary voice-monotone-extra)
   (magit-diff-lines-heading voice-lighten)
   (magit-diff-our voice-smoothen)
   (magit-diff-our-highlight voice-lighten)
   (magit-diff-removed voice-monotone-extra)
   (magit-diff-removed-highlight voice-smoothen)
   (magit-diff-their voice-animate)
   (magit-diff-their-highlight voice-bolden-and-animate)
   (magit-diffstat-added voice-animate)
   (magit-diffstat-removed voice-monotone-extra)
   (magit-dimmed voice-smoothen)
   (magit-filename voice-bolden)
   (magit-hash voice-animate)
   (magit-head voice-bolden-medium)
   (magit-header-line voice-bolden)
   (magit-keyword voice-animate)
   (magit-log-author voice-monotone-extra)
   (magit-log-date voice-monotone-medium)
   (magit-log-graph voice-monotone)
   (magit-reflog-amend voice-animate)
   (magit-reflog-checkout voice-smoothen)
   (magit-reflog-cherry-pick voice-lighten)
   (magit-reflog-commit voice-monotone-extra)
   (magit-reflog-merge voice-annotate)
   (magit-reflog-other voice-monotone-extra)
   (magit-reflog-rebase voice-lighten)
   (magit-reflog-remote voice-annotate)
   (magit-reflog-reset voice-lighten)
   (magit-refname voice-bolden)
   (magit-refname-stash voice-monotone-extra)
   (magit-refname-wip voice-lighten)
   (magit-section-heading voice-bolden)
   (magit-section-heading-selection voice-bolden-medium)
   (magit-section-highlight voice-animate)
   (magit-section-secondary-heading voice-bolden-medium)
   (magit-sequence-done voice-monotone-extra)
   (magit-sequence-drop voice-lighten)
   (magit-sequence-head voice-bolden)
   (magit-sequence-onto voice-lighten)
   (magit-sequence-part voice-monotone-extra)
   (magit-sequence-pick voice-animate)
   (magit-sequence-stop voice-smoothen)
   (magit-signature-bad voice-animate)
   (magit-signature-error voice-bolden-and-animate)
   (magit-signature-expired voice-monotone-extra)
   (magit-signature-expired-key voice-monotone-medium)
   (magit-signature-good voice-smoothen)
   (magit-signature-revoked voice-bolden)
   (magit-signature-untrusted voice-brighten)
   (magit-tag voice-animate)))

;;}}}
;;{{{ Pronunciations in Magit:
(emacspeak-pronounce-add-dictionary-entry 'magit-mode
                                          emacspeak-pronounce-sha-checksum-pattern
                                          (cons 're-search-forward
                                                'emacspeak-pronounce-sha-checksum))
(emacspeak-pronounce-add-super 'magit-mode 'magit-commit-mode)
(emacspeak-pronounce-add-super 'magit-mode 'magit-revision-mode)
(emacspeak-pronounce-add-super 'magit-mode 'magit-log-mode)

(add-hook
 'magit-mode-hook
 'emacspeak-pronounce-refresh-pronunciations)

;;}}}
;;{{{ Advice navigation commands:

;;; Advice navigators:
(defadvice magit-mark-item (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-speak-line)))

(cl-loop
 for f in
 '(
   magit-section-forward magit-section-backward magit-section-up
   magit-next-line magit-previous-line
   magit-section-forward-sibling magit-section-backward-sibling
   magit-ignore-file magit-ignore-item
   magit-stash
   magit-unstage magit-unstage-all magit-unstage-file
   magit-stage magit-stage-file  magit-stage-modified
   magit-ignore-item-locally)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak"
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'select-object)
       (emacspeak-speak-line)))))

;;}}}
;;{{{ Section Toggle:

(cl-loop
 for f in
 '(
   magit-show-commit
   magit-section-show-level-1  magit-section-show-level-2
   magit-section-show-level-3 magit-section-show-level-4
   magit-section-show-level-1-all magit-section-show-level-2-all
   magit-section-show-level-3-all magit-section-show-level-4-all
   magit-section-cycle-diffs)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'select-object)))))
(defadvice magit-section-cycle-global (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (dtk-speak "Cycling global visibility of sections")))

(cl-loop
 for f in
 '(
   magit-section-toggle magit-section-cycle)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon
        (if   (oref (ad-get-arg 0) hidden) 'close-object 'open-object))))))

;;}}}
;;{{{blob mode:
(defadvice magit-blob-visit-file (after emacspeak pre act comp)
  "Speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(cl-loop
 for f in 
 '(magit-blob-previous magit-blob-next)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))



;;}}}

;;{{{ Additional commands to advice:

(defadvice magit-refresh (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (emacspeak-speak-line)))

(defadvice magit-status (after emacspeak pre act  comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(cl-loop
 for f in
 '(magit-mode-quit-window magit-mode-bury-buffer magit-log-bury-buffer)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act  comp)
     "speak."
     (when (ems-interactive-p)
       (with-current-buffer (window-buffer (selected-window))
         (emacspeak-auditory-icon 'close-object)
         (emacspeak-speak-mode-line))))))

(defadvice magit-refresh-all (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (emacspeak-speak-line)))

(defadvice magit-display-buffer (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))


;;}}}
;;{{{ Advise process-sentinel:

(defadvice magit-process-finish(after emacspeak pre act comp)
  "Produce auditory icon."
  (emacspeak-auditory-icon 'task-done))

;;}}}
;;{{{ Magit Blame:

(defun emacspeak-magit-blame-speak ()
  "Summarize current blame chunk."
  (let ((o (cl-first (overlays-at (point)))))
    (when o
      (dtk-speak
       (concat
        (buffer-substring (line-beginning-position) (line-end-position))
        (overlay-get o 'before-string))))))

(cl-loop
 for f in
 '(
   magit-blame-previous-chunk magit-blame-previous-chunk-same-commit
   magit-blame-next-chunk magit-blame-next-chunk-same-commit)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-magit-blame-speak)
       (emacspeak-auditory-icon 'large-movement)))))

(defadvice magit-blame-quit (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))
(defadvice magit-blame-toggle-headings (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon (if magit-blame-show-headings 'on 'off))
    (message "Toggled blame headings.")))

(defadvice magit-blame (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (message "Entering Magit Blame")
    (emacspeak-auditory-icon 'open-object)))

(defadvice magit-diff-show-or-scroll-up (around emacspeak pre act comp)
  "speak."
  (cond
   ((ems-interactive-p)
    (let ((orig (point)))
      ad-do-it
      (cond
       ((= orig (point))
        (message "Displayed commit in other window.")
        (emacspeak-auditory-icon 'open-object))
       (t (emacspeak-auditory-icon 'scroll)
          (emacspeak-speak-line)))))
   (t ad-do-it))
  ad-return-value)

;;}}}
;;{{{Keys:
(cl-declaim (special magit-file-mode-map))
(when (and (bound-and-true-p magit-file-mode-map)
           (keymapp magit-file-mode-map))
  (define-key magit-file-mode-map (ems-kbd "C-c g") 'magit-file-dispatch))
(cl-declaim (special ctl-x-map))
(define-key ctl-x-map  "g" 'magit-status)

;;}}}
(provide 'emacspeak-magit)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

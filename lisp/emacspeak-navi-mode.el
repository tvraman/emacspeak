;;; emacspeak-navi-mode.el --- Speech-enable NAVI-MODE  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable NAVI-MODE An Emacs Interface to navi-mode
;; Keywords: Emacspeak,  Audio Desktop navi-mode
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
;; MERCHANTABILITY or FITNNAVI-MODE FOR A PARTICULAR PURPOSE.  See the
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
;; NAVI-MODE ==  Remote control for buffer navigation

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Interactive Commands:

'(
  navi-act-on-thing
  navi-act-on-thing-at-point
  navi-agenda-remove-restriction-lock
  navi-agenda-set-restriction-lock
  navi-cease-edit
  navi-clock-cancel
  navi-clock-goto
  navi-clock-in
  navi-clock-out
  navi-clock-report
  navi-copy-thing-at-point-to-register-s
  navi-deadline
  navi-demote-subtree
  navi-edit-as-org
  navi-edit-mode
  navi-export-dispatch
  navi-footnote-action

  navi-inc-effort
  navi-insert-drawer
  navi-insert-last-stored-link
  navi-insert-link
  navi-isearch
  navi-kill-thing-at-point
  navi-mail-subtree
  navi-mark-thing-at-point-and-switch
  navi-move-down-subtree
  navi-move-up-subtree
  navi-narrow-to-thing-at-point
  navi-next-block
  navi-next-link

  navi-previous-block
  navi-previous-link
  navi-priority
  navi-promote-subtree
  navi-query-replace

  navi-revert-function
  navi-schedule
  navi-search-and-switch
  navi-set-effort
  navi-set-property
  navi-set-property-and-value
  navi-set-tags-command
  navi-show-help
  navi-sort-entries

  navi-time-stamp
  navi-time-stamp-inactive
  navi-timer
  navi-timer-cancel-timer
  navi-timer-item
  navi-timer-pause-or-continue
  navi-timer-set-timer
  navi-timer-start
  navi-todo
  navi-toggle-archive-tag
  navi-toggle-checkbox
  navi-toggle-comment
  navi-toggle-fixed-width
  navi-underline-line-with
  navi-undo
  navi-widen
  navi-yank-thing-from-register-s
  )

(cl-loop
 for f in
 '(navi-switch-to-twin-buffer navi-goto-occurrence-other-window)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'select-object)
       (emacspeak-speak-line)))))

(defadvice navi-quit-and-switch (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

;;}}}
(provide 'emacspeak-navi-mode)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}

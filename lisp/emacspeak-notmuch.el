;;; emacspeak-notmuch.el --- Speech-enable NOTMUCH  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable NOTMUCH An Emacs Interface to notmuch
;;; Keywords: Emacspeak,  Audio Desktop notmuch
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, 2019, T. V. Raman
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
;;; MERCHANTABILITY or FITNNOTMUCH FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; NOTMUCH ==  Emacs interface to notmuch mail

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map Faces:

(voice-setup-add-map 
 '(
   (notmuch-crypto-decryption voice-smoothen)
   (notmuch-crypto-part-header voice-smoothen-extra)
   (notmuch-crypto-signature-bad voice-smoothen-medium)
   (notmuch-crypto-signature-good voice-animate)
   (notmuch-crypto-signature-good-key voice-animate-extra)
   (notmuch-crypto-signature-unknown voice-animate-medium)
   (notmuch-jump-key voice-lighten)
   (notmuch-message-summary-face voice-annotate)
   (notmuch-search-count voice-bolden)
   (notmuch-search-date voice-bolden )
   (notmuch-search-flagged-face voice-lighten)
   (notmuch-search-matching-authors voice-lighten)
   (notmuch-search-non-matching-authors voice-monotone)
   (notmuch-search-subject voice-bolden)
   (notmuch-search-unread-face voice-animate)
   (notmuch-tag-face voice-bolden)
   (notmuch-wash-cited-text voice-smoothen)))

;;}}}
;;{{{ Interactive Commands:

'(
  notmuch-cycle-notmuch-buffers
  notmuch-jump-search
  notmuch-message-mode
  notmuch-poll
  notmuch-poll-and-refresh-this-buffer
  notmuch-refresh-all-buffers
  notmuch-refresh-this-buffer
  notmuch-search
  notmuch-search-add-tag
  notmuch-search-archive-thread
  notmuch-search-by-tag
  notmuch-search-filter
  notmuch-search-filter-by-tag
  notmuch-search-first-thread
  notmuch-search-from-tree-current-query
  notmuch-search-last-thread
  notmuch-search-mode
  notmuch-search-mode-transient
  notmuch-search-next-thread
  notmuch-search-previous-thread
  notmuch-search-refresh-view
  notmuch-search-remove-tag
  notmuch-search-reply-to-thread
  notmuch-search-reply-to-thread-sender
  notmuch-search-scroll-down
  notmuch-search-scroll-up
  notmuch-search-show-thread
  notmuch-search-stash-thread-id
  notmuch-search-stash-transient
  notmuch-search-tag
  notmuch-search-tag-all
  notmuch-search-toggle-order
  notmuch-search-transient
  notmuch-show
  notmuch-show-add-tag
  notmuch-show-advance
  notmuch-show-advance-and-archive
  notmuch-show-archive-message
  notmuch-show-archive-message-then-next-or-exit
  notmuch-show-archive-message-then-next-or-next-thread
  notmuch-show-archive-thread
  notmuch-show-archive-thread-then-exit
  notmuch-show-archive-thread-then-next
  notmuch-show-browse-urls
  notmuch-show-choose-mime-of-part
  notmuch-show-filter-thread
  notmuch-show-forward-message
  notmuch-show-forward-open-messages
  notmuch-show-interactively-view-part
  notmuch-show-mark-read
  notmuch-show-mode
  notmuch-show-mode-transient
  notmuch-show-next-button
  notmuch-show-next-matching-message
  notmuch-show-next-message
  notmuch-show-next-open-message
  notmuch-show-next-thread
  notmuch-show-next-thread-show
  notmuch-show-open-or-close-all
  notmuch-show-part-button-default
  notmuch-show-part-transient
  notmuch-show-pipe-message
  notmuch-show-pipe-part
  notmuch-show-previous-button
  notmuch-show-previous-message
  notmuch-show-previous-open-message
  notmuch-show-previous-thread-show
  notmuch-show-print-message
  notmuch-show-refresh-view
  notmuch-show-remove-tag
  notmuch-show-reply
  notmuch-show-reply-sender
  notmuch-show-resend-message
  notmuch-show-resume-message
  notmuch-show-rewind
  notmuch-show-save-attachments
  notmuch-show-save-part
  notmuch-show-setup-w3m
  notmuch-show-stash-cc
  notmuch-show-stash-date
  notmuch-show-stash-filename
  notmuch-show-stash-from
  notmuch-show-stash-git-send-email
  notmuch-show-stash-message-id
  notmuch-show-stash-message-id-stripped
  notmuch-show-stash-mlarchive-link
  notmuch-show-stash-mlarchive-link-and-go
  notmuch-show-stash-subject
  notmuch-show-stash-tags
  notmuch-show-stash-to
  notmuch-show-stash-transient
  notmuch-show-tag
  notmuch-show-tag-all
  notmuch-show-toggle-elide-non-matching
  notmuch-show-toggle-message
  notmuch-show-toggle-part-invisibility
  notmuch-show-toggle-process-crypto
  notmuch-show-toggle-thread-indentation
  notmuch-show-toggle-visibility-headers
  notmuch-show-view-all-mime-parts
  notmuch-show-view-part
  notmuch-show-view-raw-message
  notmuch-stash-query
  notmuch-subkeymap-help
  notmuch-tag-jump
  notmuch-tag-transient
  notmuch-tag-undo
  notmuch-tree
  notmuch-tree-add-tag
  notmuch-tree-archive-message
  notmuch-tree-archive-message-then-next
  notmuch-tree-archive-message-then-next-or-exit
  notmuch-tree-archive-thread
  notmuch-tree-archive-thread-then-exit
  notmuch-tree-archive-thread-then-next
  notmuch-tree-close-message-window
  notmuch-tree-filter
  notmuch-tree-filter-by-tag
  notmuch-tree-forward-message
  notmuch-tree-from-search-current-query
  notmuch-tree-from-search-thread
  notmuch-tree-from-show-current-query
  notmuch-tree-from-unthreaded-current-query
  notmuch-tree-help
  notmuch-tree-jump-search
  notmuch-tree-matching-message
  notmuch-tree-mode
  notmuch-tree-mode-transient
  notmuch-tree-new-mail
  notmuch-tree-next-matching-message
  notmuch-tree-next-message
  notmuch-tree-next-message-button
  notmuch-tree-next-thread
  notmuch-tree-next-thread-from-search
  notmuch-tree-next-thread-in-tree
  notmuch-tree-prev-matching-message
  notmuch-tree-prev-message
  notmuch-tree-prev-thread
  notmuch-tree-prev-thread-in-tree
  notmuch-tree-previous-message-button
  notmuch-tree-quit
  notmuch-tree-refresh-view
  notmuch-tree-remove-tag
  notmuch-tree-reply
  notmuch-tree-reply-sender
  notmuch-tree-resume-message
  notmuch-tree-scroll-message-window
  notmuch-tree-scroll-message-window-back
  notmuch-tree-scroll-or-next
  notmuch-tree-show-message
  notmuch-tree-show-message-in
  notmuch-tree-show-message-out
  notmuch-tree-tag
  notmuch-tree-tag-thread
  notmuch-tree-to-search
  notmuch-tree-to-tree
  notmuch-tree-toggle-message-process-crypto
  notmuch-tree-toggle-order
  notmuch-tree-view-raw-message
  notmuch-tree-worker
  notmuch-unthreaded
  notmuch-unthreaded-from-search-current-query
  notmuch-unthreaded-from-show-current-query
  notmuch-unthreaded-from-tree-current-query
  )
(cl-loop
 for f in 
 '(notmuch notmuch-hello notmuch-hello-update)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)
       (emacspeak-speak-mode-line)))))

(defadvice notmuch-bury-or-kill-this-buffer (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice notmuch-search-show-thread (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

;;}}}
;;{{{MUA:

'(notmuch-mua-kill-buffer
  notmuch-mua-mail
  notmuch-mua-new-mail
  notmuch-mua-send
  notmuch-mua-send-and-exit
  notmuch-mua-send-common)

;;}}}
(provide 'emacspeak-notmuch)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

;;; emacspeak-evil.el --- Speech-enable EVIL  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable EVIL An Emacs Interface to evil
;;; Keywords: Emacspeak,  Audio Desktop evil
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
;;; MERCHANTABILITY or FITNEVIL FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; EVIL ==  VIM In Emacs
;;; This is work-in-progress and is not complete.
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map Faces:

(voice-setup-add-map
 '(
   (evil-ex-commands voice-bolden)
   (evil-ex-info voice-monotone)
   (evil-ex-lazy-highlight voice-animate)
   (evil-ex-search voice-bolden-and-animate)
   (evil-ex-substitute-matches voice-lighten)
   (evil-ex-substitute-replacement voice-smoothen)))

;;}}}
;;{{{ Interactive Commands:

'(
  evil-a-WORD
  evil-a-back-quote
  evil-a-bracket
  evil-a-curly
  evil-a-double-quote
  evil-a-paragraph
  evil-a-paren
  evil-a-sentence
  evil-a-single-quote
  evil-a-symbol
  evil-a-tag
  evil-a-word
  evil-align-center
  evil-align-left
  evil-align-right
  evil-an-angle
  evil-append
  evil-append-line
  evil-backward-WORD-begin
  evil-backward-WORD-end
  evil-backward-char
  evil-backward-paragraph
  evil-backward-section-begin
  evil-backward-section-end
  evil-backward-sentence-begin
  evil-backward-word-begin
  evil-backward-word-end
  evil-beginning-of-line
  evil-beginning-of-line-or-digit-argument
  evil-beginning-of-visual-line
  evil-buffer
  evil-buffer-new
  evil-change
  evil-change-line
  evil-change-to-initial-state
  evil-change-to-previous-state
  evil-change-whole-line
  evil-close-fold
  evil-close-folds
  evil-command-window-ex
  evil-command-window-execute
  evil-command-window-mode
  evil-command-window-search-backward
  evil-command-window-search-forward
  evil-commentary-mode
  evil-commentary/org-comment-or-uncomment-region
  evil-complete-next
  evil-complete-next-line
  evil-complete-previous
  evil-complete-previous-line
  evil-copy
  evil-copy-chars-from-line
  evil-copy-from-above
  evil-copy-from-below
  evil-delete
  evil-delete-backward-char
  evil-delete-backward-char-and-join
  evil-delete-backward-word
  evil-delete-buffer
  evil-delete-char
  evil-delete-line
  evil-delete-marks
  evil-delete-whole-line
  evil-digit-argument-or-evil-beginning-of-line
  evil-downcase
  evil-ediff-init
  evil-edit
  evil-emacs-state
  evil-embrace-disable-evil-surround-integration
  evil-embrace-enable-evil-surround-integration
  evil-end-of-line
  evil-end-of-visual-line
  evil-ex
  evil-ex-completion
  evil-ex-delete-backward-char
  evil-ex-global
  evil-ex-global-inverted
  evil-ex-join
  evil-ex-line-number
  evil-ex-nohighlight
  evil-ex-normal
  evil-ex-repeat
  evil-ex-repeat-global-substitute
  evil-ex-repeat-substitute
  evil-ex-repeat-substitute-with-flags
  evil-ex-repeat-substitute-with-search
  evil-ex-repeat-substitute-with-search-and-flags
  evil-ex-resize
  evil-ex-search-abort
  evil-ex-search-backward
  evil-ex-search-exit
  evil-ex-search-forward
  evil-ex-search-next
  evil-ex-search-previous
  evil-ex-search-unbounded-word-backward
  evil-ex-search-unbounded-word-forward
  evil-ex-search-word-backward
  evil-ex-search-word-forward
  evil-ex-set-initial-state
  evil-ex-show-digraphs
  evil-ex-sort
  evil-ex-substitute
  evil-execute-in-emacs-state
  evil-execute-in-normal-state
  evil-execute-macro
  evil-exit-emacs-state
  evil-exit-visual-and-repeat
  evil-exit-visual-state
  evil-fill
  evil-fill-and-move
  evil-find-char
  evil-find-char-backward
  evil-find-char-to
  evil-find-char-to-backward
  evil-find-file-at-point-with-line
  evil-first-non-blank
  evil-first-non-blank-of-visual-line
  evil-force-normal-state
  evil-forward-WORD-begin
  evil-forward-WORD-end
  evil-forward-char
  evil-forward-paragraph
  evil-forward-section-begin
  evil-forward-section-end
  evil-forward-sentence-begin
  evil-forward-word-begin
  evil-forward-word-end
  evil-goto-char
  evil-goto-column
  evil-goto-definition
  evil-goto-error
  evil-goto-first-line
  evil-goto-line
  evil-goto-mark
  evil-goto-mark-line
  evil-indent
  evil-indent-line
  evil-inner-WORD
  evil-inner-angle
  evil-inner-back-quote
  evil-inner-bracket
  evil-inner-curly
  evil-inner-double-quote
  evil-inner-paragraph
  evil-inner-paren
  evil-inner-sentence
  evil-inner-single-quote
  evil-inner-symbol
  evil-inner-tag
  evil-inner-word
  evil-insert
  evil-insert-digraph
  evil-insert-line
  evil-insert-resume
  evil-insert-state
  evil-invert-case
  evil-invert-char
  evil-join
  evil-join-whitespace
  evil-jump-backward
  evil-jump-backward-swap
  evil-jump-forward
  evil-jump-item
  evil-jump-to-tag
  evil-last-non-blank
  evil-leader-mode
  evil-leader/set-key
  evil-leader/set-key-for-mode
  evil-line
  evil-list-view-goto-entry
  evil-list-view-mode
  evil-local-mode
  evil-lookup
  evil-magit-init
  evil-magit-revert
  evil-make
  evil-matchit-mode
  evil-middle-of-visual-line
  evil-mode
  evil-motion-state
  evil-mouse-drag-region
  evil-move
  evil-move-to-column
  evil-next-buffer
  evil-next-close-brace
  evil-next-close-paren
  evil-next-flyspell-error
  evil-next-line
  evil-next-line-1-first-non-blank
  evil-next-line-first-non-blank
  evil-next-match
  evil-next-visual-line
  evil-normal-state
  evil-open-above
  evil-open-below
  evil-open-fold
  evil-open-fold-rec
  evil-open-folds
  evil-opener-mode
  evil-operator-shortcut-mode
  evil-operator-state
  evil-paste-after
  evil-paste-before
  evil-paste-from-register
  evil-paste-last-insertion
  evil-paste-pop
  evil-paste-pop-next
  evil-prev-buffer
  evil-prev-flyspell-error
  evil-previous-line
  evil-previous-line-first-non-blank
  evil-previous-match
  evil-previous-open-brace
  evil-previous-open-paren
  evil-previous-visual-line
  evil-quit
  evil-quit-all
  evil-quit-all-with-error-code
  evil-read
  evil-read-digraph-char
  evil-read-quoted-char
  evil-record-macro
  evil-repeat
  evil-repeat-find-char
  evil-repeat-find-char-reverse
  evil-repeat-pop
  evil-repeat-pop-next
  evil-replace
  evil-replace-backspace
  evil-replace-state
  evil-ret
  evil-ret-and-indent
  evil-rot13
  evil-save
  evil-save-and-close
  evil-save-and-quit
  evil-save-modified-and-close
  evil-scroll-bottom-line-to-top
  evil-scroll-column-left
  evil-scroll-column-right
  evil-scroll-count-reset
  evil-scroll-down
  evil-scroll-left
  evil-scroll-line-down
  evil-scroll-line-to-bottom
  evil-scroll-line-to-center
  evil-scroll-line-to-top
  evil-scroll-line-up
  evil-scroll-page-down
  evil-scroll-page-up
  evil-scroll-right
  evil-scroll-top-line-to-bottom
  evil-scroll-up
  evil-search-backward
  evil-search-forward
  evil-search-next
  evil-search-previous
  evil-search-unbounded-word-backward
  evil-search-unbounded-word-forward
  evil-search-word-backward
  evil-search-word-forward
  evil-set-marker
  evil-shell-command
  evil-shift-left
  evil-shift-left-line
  evil-shift-right
  evil-shift-right-line
  evil-show-file-info
  evil-show-files
  evil-show-jumps
  evil-show-marks
  evil-show-registers
  evil-split-buffer
  evil-split-next-buffer
  evil-split-prev-buffer
  evil-substitute
  evil-surround-change
  evil-surround-delete
  evil-surround-mode
  evil-switch-to-windows-last-buffer
  evil-toggle-fold
  evil-upcase
  evil-update-insert-state-bindings
  evil-use-register
  evil-version
  evil-visual-block
  evil-visual-char
  evil-visual-exchange-corners
  evil-visual-line
  evil-visual-make-region
  evil-visual-paste
  evil-visual-restore
  evil-visual-rotate
  evil-visual-state
  evil-window-bottom
  evil-window-bottom-right
  evil-window-decrease-height
  evil-window-decrease-width
  evil-window-delete
  evil-window-down
  evil-window-increase-height
  evil-window-increase-width
  evil-window-left
  evil-window-middle
  evil-window-move-far-left
  evil-window-move-far-right
  evil-window-move-very-bottom
  evil-window-move-very-top
  evil-window-mru
  evil-window-new
  evil-window-next
  evil-window-prev
  evil-window-right
  evil-window-rotate-downwards
  evil-window-rotate-upwards
  evil-window-set-height
  evil-window-set-width
  evil-window-split
  evil-window-top
  evil-window-top-left
  evil-window-up
  evil-window-vnew
  evil-window-vsplit
  evil-write
  evil-write-all
  evil-yank
  evil-yank-line
  evilmi-delete-items
  evilmi-jump-items
  evilmi-jump-to-percentage
  evilmi-select-items
  evilmi-version
  )

;;}}}
;;{{{ linewise Motion:

(cl-loop
 for f in
 '(
   evil-backward-section-begin evil-backward-section-end
   evil-backward-sentence-begin evil-beginning-of-line evil-first-non-blank
   evil-forward-section-begin evil-forward-section-end
   evil-forward-sentence-begin
   evil-goto-definition evil-goto-first-line evil-goto-line
   evil-goto-mark evil-goto-mark-line
   evil-jump-backward evil-jump-forward evil-jump-to-tag
   evil-last-non-blank
   evil-next-close-paren
   evil-next-line evil-next-line-1-first-non-blank
   evil-next-line-first-non-blank
   evil-next-match
   evil-previous-line evil-previous-line-first-non-blank
   evil-previous-match
   evil-previous-open-paren
   evil-ret
   evil-window-top)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p) (emacspeak-speak-line)))))


;;}}}
;;{{{ Update keymaps:

(defun emacspeak-evil-fix-emacspeak-prefix (keymap)
  "Move original evil command on C-e to C-e e."
  (declare (special emacspeak-prefix))
  (let ((orig (lookup-key keymap emacspeak-prefix)))
(when orig 
  (define-key keymap emacspeak-prefix  'emacspeak-prefix-command)
  (define-key keymap (concat emacspeak-prefix "e") orig)
  (define-key keymap (concat emacspeak-prefix emacspeak-prefix) orig))))
(declaim (special 
  evil-normal-state-map evil-insert-state-map
  evil-visual-state-map evil-replace-state-map
  evil-operator-state-map evil-motion-state-map))

(eval-after-load
    "evil-maps"
  `(mapc
  #'emacspeak-evil-fix-emacspeak-prefix
 (list
  evil-normal-state-map evil-insert-state-map
  evil-visual-state-map evil-replace-state-map
  evil-operator-state-map evil-motion-state-map)))
(global-set-key (concat emacspeak-prefix "e") 'end-of-line)
(global-set-key (concat emacspeak-prefix emacspeak-prefix) 'end-of-line) 

;;}}}
;;{{{ State Hooks:

(defun  emacspeak-evil-sate-change-hook  ()
  "State change feedback."
  (declare (special evil-previous-state evil-next-state))
  (when (and evil-previous-state evil-next-state
             (not (eq evil-previous-state evil-next-state)))
    (emacspeak-auditory-icon 'select-object)
    (dtk-notify-speak
     (format "Changing state from %s to %s"
             evil-previous-state evil-next-state))))

(cl-loop

 for hook in 
 '(evil-normal-state-exit-hook evil-insert-state-exit-hook
                               evil-visual-state-exit-hook evil-replace-state-exit-hook
                               evil-operator-state-exit-hook evil-motion-state-exit-hook)
 do
 (add-hook hook #'emacspeak-evil-state-change-hook))
(defadvice evil-exit-emacs-state (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-notify-speak "Leaving Emacs state.")))


;;}}}
(provide 'emacspeak-evil)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

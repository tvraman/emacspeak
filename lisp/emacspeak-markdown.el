;;; emacspeak-markdown.el --- Speech-enable MARKDOWN-mode.el  -*- lexical-binding: t; -*-
;;; $Id: emacspeak-markdown.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable MARKDOWN An Emacs Interface to markdown
;;; Keywords: Emacspeak,  Audio Desktop markdown
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
;;;Copyright (C) 1995 -- 2021, T. V. Raman
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
;;; MERCHANTABILITY or FITNMARKDOWN FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; MARKDOWN ==  Light-weight markup.
;;; This module speech-enables markdown.el

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map faces to voices:
(voice-setup-add-map
 '(
   (markdown-italic-face  voice-animate)
   (markdown-bold-face voice-bolden)
   (markdown-header-rule-face voice-bolden-medium)
   (markdown-header-delimiter-face voice-lighten)
   (markdown-header-face voice-bolden)
   (markdown-header-face-1 voice-bolden-medium)
   (markdown-header-face-2 voice-bolden-and-animate)
   (markdown-header-face-3 voice-bolden-extra)
   (markdown-header-face-4 voice-smoothen)
   (markdown-header-face-5 voice-lighten-extra)
   (markdown-header-face-6 voice-monotone-extra)
   (markdown-inline-code-face voice-monotone-extra)
   (markdown-list-face voice-animate)
   (markdown-blockquote-face voice-lighten)
   (markdown-pre-face voice-monotone-extra)
   (markdown-language-keyword-face voice-smoothen)
   (markdown-link-face voice-bolden)
   (markdown-missing-link-face voice-animate)
   (markdown-reference-face voice-lighten)
   (markdown-url-face voice-bolden-and-animate)
   (markdown-link-title-face voice-lighten)
   (markdown-line-break-face voice-monotone-extra)
   (markdown-comment-face voice-monotone-extra)
   (markdown-math-face voice-animate)
   (markdown-metadata-key-face voice-smoothen)
   (markdown-metadata-value-face voice-smoothen-medium)
   ))
;;}}}
;;{{{ Advice Interactive Commands:
(cl-loop
 for f in
 '(markdown-outdent-or-delete markdown-exdent-or-delete)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Speak character you're deleting."
     (cond
      ((ems-interactive-p)
       (dtk-tone 500 100 'force)
       (emacspeak-speak-this-char (preceding-char))
       ad-do-it)
      (t ad-do-it))
     ad-return-value)))

(cl-loop
 for f in
 '(
   markdown-backward-block markdown-backward-page
   markdown-beginning-of-list markdown-beginning-of-text-block
   markdown-edit-code-block markdown-end-of-list
   markdown-end-of-text-block markdown-forward-block markdown-forward-page
   markdown-insert-inline-link-dwim markdown-insert-kbd
   markdown-insert-strike-through
   markdown-outline-next markdown-outline-next-same-level
   markdown-outline-previous
   markdown-outline-previous-same-level markdown-outline-up
   markdown-reference-goto-link
   markdown-up-heading markdown-up-list
   markdown-demote-subtree markdown-demote markdown-demote-list-item
   markdown-promote-subtree markdown-move-subtree-up markdown-move-subtree-down
   markdown-backward-paragraph markdown-cycle
   markdown-enter-key
   markdown-beginning-of-block markdown-beginning-of-defun
   markdown-end-of-block markdown-end-of-block-element
   markdown-insert-footnote markdown-insert-code
   markdown-insert-bold markdown-insert-blockquote
   markdown-forward-paragraph markdown-footnote-goto-text
   markdown-end-of-defun markdown-insert-gfm-code-block
   markdown-insert-header markdown-insert-header-atx-1
   markdown-insert-header-atx-2 markdown-insert-header-atx-3
   markdown-insert-header-atx-4 markdown-insert-header-atx-5
   markdown-insert-header-atx-6 markdown-insert-header-dwim
   markdown-insert-header-setext-1 markdown-insert-header-setext-2
   markdown-insert-header-setext-dwim
   markdown-insert-hr markdown-insert-image
   markdown-insert-italic markdown-insert-link
   markdown-insert-list-item markdown-insert-pre
   markdown-insert-reference-image markdown-insert-reference-link-dwim
   markdown-insert-uri markdown-insert-wiki-link
   markdown-jump
   markdown-move-down markdown-move-list-item-down
   markdown-move-list-item-up markdown-move-up
   markdown-next-visible-heading markdown-previous-visible-heading
   markdown-next-heading markdown-previous-heading
   markdown-forward-same-level markdown-backward-same-level
   markdown-hide-subtree markdown-hide-body markdown-hide-sublevels
   markdown-indent-line
   markdown-next-link markdown-previous-link
   markdown-promote markdown-promote-list-item
   markdown-reference-goto-definition
   )
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))

(cl-loop
 for f in
 '(
   markdown-check-refs markdown-check-change-for-wiki-link
   markdown-export markdown-export-and-preview
   markdown-indent-region markdown-blockquote-region)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'task-done)
       (emacspeak-speak-line)))))

(cl-loop
 for f in
 '(
   markdown-complete-region markdown-complete-buffer
   markdown-complete-at-point markdown-complete)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'complete)
       (emacspeak-speak-line)))))

;;}}}
(provide 'emacspeak-markdown)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

;;; emacspeak-markdown.el --- Speech-enable MARKDOWN-mode.el
;;; $Id: emacspeak-markdown.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable MARKDOWN An Emacs Interface to markdown
;;; Keywords: Emacspeak,  Audio Desktop markdown
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
;;; MERCHANTABILITY or FITNMARKDOWN FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; MARKDOWN ==  Light-weight markup.
;;; This module speech-enables markdown.el

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
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
   (markdown-header-face-6 voice-monotone)
   (markdown-inline-code-face voice-monotone)
   (markdown-list-face voice-animate)
   (markdown-blockquote-face voice-lighten)
   (markdown-pre-face voice-monotone)
   (markdown-language-keyword-face voice-smoothen)
   (markdown-link-face voice-bolden)
   (markdown-missing-link-face voice-animate)
   (markdown-reference-face voice-lighten)
   (markdown-footnote-face voice-smoothen)
   (markdown-url-face voice-bolden-and-animate)
   (markdown-link-title-face voice-lighten)
   (markdown-line-break-face voice-monotone)
   (markdown-comment-face voice-monotone)
   (markdown-math-face voice-animate)
   (markdown-metadata-key-face voice-smoothen)
   (markdown-metadata-value-face voice-smoothen-medium)
   ))
;;}}}
;;{{{ Advice Interactive Commands:

(defadvice markdown-exdent-or-delete (around emacspeak pre act)
  "Speak character you're deleting."
  (cond
   ((ems-interactive-p  )
    (dtk-tone 500 30 'force)
    (emacspeak-speak-this-char (preceding-char ))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(loop
 for f in
 '(markdown-backward-paragraph
   markdown-beginning-of-block
   markdown-beginning-of-defun
   markdown-demote markdown-demote-list-item
   markdown-end-of-block markdown-end-of-block-element
   markdown-insert-footnote markdown-insert-code
   markdown-insert-bold markdown-insert-blockquote
   markdown-forward-paragraph markdown-footnote-goto-text
   markdown-end-of-defun
   markdown-insert-gfm-code-block
   markdown-insert-header
   markdown-insert-header-atx-1
   markdown-insert-header-atx-2
   markdown-insert-header-atx-3
   markdown-insert-header-atx-4
   markdown-insert-header-atx-5
   markdown-insert-header-atx-6
   markdown-insert-header-dwim
   markdown-insert-header-setext-1
   markdown-insert-header-setext-2
   markdown-insert-header-setext-dwim
   markdown-insert-hr
   markdown-insert-image
   markdown-insert-italic
   markdown-insert-link
   markdown-insert-list-item
   markdown-insert-pre
   markdown-insert-reference-image
   markdown-insert-reference-link-dwim
   markdown-insert-uri
   markdown-insert-wiki-link
   markdown-jump
   markdown-move-down markdown-move-list-item-down
   markdown-move-list-item-up markdown-move-up
   markdown-next-link markdown-previous-link
   markdown-promote markdown-promote-list-item
   markdown-reference-goto-definition
   )
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))

;; (markdown-blockquote-region
;;  markdown-check-change-for-wiki-link
;;  markdown-check-refs
;;  markdown-cleanup-list-numbers
;;  markdown-complete
;;  markdown-complete-at-point
;;  markdown-complete-buffer
;;  markdown-complete-region
;;  markdown-cycle
;;  markdown-enable-math
;;  markdown-enter-key
;;  markdown-exdent-region
;;  markdown-export
;;  markdown-export-and-preview
;;  markdown-follow-link-at-point
;;  markdown-follow-thing-at-point
;;  markdown-follow-wiki-link-at-point
;;  markdown-fontify-buffer-wiki-links
;;  markdown-footnote-kill
;;  markdown-footnote-return
;;  markdown-indent-line
;;  markdown-indent-region
;;  markdown-kill-ring-save
;;  markdown-kill-thing-at-point
;;  markdown-mode
;;  markdown-mode-menu
;;  markdown-open
;;  markdown-other-window
;;  markdown-pre-region
;;  markdown-preview
;;  markdown-reload-extensions
;;  markdown-remove-header
;;  markdown-shifttab
;;  markdown-standalone
;;  markdown-unfontify-region-wiki-links)

;;}}}
(provide 'emacspeak-markdown)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

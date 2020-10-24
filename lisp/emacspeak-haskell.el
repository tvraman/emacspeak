;;; emacspeak-haskell.el --- Speech-enable HASKELL  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable HASKELL An Emacs Interface to haskell
;;; Keywords: Emacspeak,  Audio Desktop haskell
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
;;; MERCHANTABILITY or FITNHASKELL FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; Speech-enable package haskell-mode

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
   (haskell-c2hs-hook-name-face voice-lighten)
   (haskell-c2hs-hook-pair-face voice-brighten)
   (haskell-constructor-face voice-monotone-medium)
   (haskell-debug-heading-face voice-smoothen-extra)
   (haskell-debug-keybinding-face voice-smoothen)
   (haskell-debug-muted-face voice-annotate)
   (haskell-debug-newline-face voice-monotone)
   (haskell-debug-trace-number-face voice-lighten)
   (haskell-debug-warning-face voice-warning)
   (haskell-definition-face voice-type-personality)
   (haskell-error-face voice-warning)
   (haskell-hole-face voice-bolden)
   (haskell-interactive-face-compile-error voice-warning)
   (haskell-interactive-face-compile-warning voice-warning)
   (haskell-interactive-face-garbage voice-monotone-light)
   (haskell-interactive-face-prompt voice-brighten)
   (haskell-interactive-face-prompt-cont voice-brighten-extra)
   (haskell-interactive-face-result voice-lighten)
   (haskell-keyword-face voice-keyword-personality)
   (haskell-liquid-haskell-annotation-face voice-bolden-extra)
   (haskell-literate-comment-face voice-monotone)
   (haskell-operator-face voice-smoothen-extra)
   (haskell-pragma-face voice-monotone-light)
   (haskell-quasi-quote-face voice-string-personality)
   (haskell-type-face voice-type-personality)
   (haskell-warning-face voice-warning)))

;;}}}
;;{{{ Interactive Commands:
'(
  haskell-add-import
  haskell-align-imports
  haskell-auto-insert-module-template
  haskell-c2hs-mode
  haskell-cabal-add-dependency
  haskell-cabal-beginning-of-section
  haskell-cabal-beginning-of-subsection
  haskell-cabal-end-of-section
  haskell-cabal-end-of-subsection
  haskell-cabal-find-or-create-source-file
  haskell-cabal-get-field
  haskell-cabal-goto-benchmark-section
  haskell-cabal-goto-common-section
  haskell-cabal-goto-executable-section
  haskell-cabal-goto-exposed-modules
  haskell-cabal-goto-library-section
  haskell-cabal-goto-test-suite-section
  haskell-cabal-guess-setting
  haskell-cabal-indent-line
  haskell-cabal-mode
  haskell-cabal-next-section
  haskell-cabal-next-subsection
  haskell-cabal-previous-section
  haskell-cabal-previous-subsection
  haskell-cabal-section-end
  haskell-cabal-subsection-arrange-lines
  haskell-cabal-visit-file
  haskell-check
  haskell-collapse-mode
  haskell-compilation-mode
  haskell-compile
  haskell-debug
  haskell-debug-mode
  haskell-debug/abandon
  haskell-debug/break-on-function
  haskell-debug/breakpoint-numbers
  haskell-debug/continue
  haskell-debug/delete
  haskell-debug/next
  haskell-debug/previous
  haskell-debug/refresh
  haskell-debug/select
  haskell-debug/start-step
  haskell-debug/step
  haskell-debug/trace
  haskell-decl-scan-mode
  haskell-delete-indentation
  haskell-delete-nested
  haskell-describe
  haskell-doc-ask-mouse-for-type
  haskell-doc-check-active
  haskell-doc-extract-and-insert-types
  haskell-doc-get-global-fct-type
  haskell-doc-imported-list
  haskell-doc-install-keymap
  haskell-doc-make-global-fct-index
  haskell-doc-mode
  haskell-doc-show-global-types
  haskell-doc-show-prelude
  haskell-doc-show-reserved
  haskell-doc-show-strategy
  haskell-doc-show-type
  haskell-doc-show-user-defined
  haskell-ds-backward-decl
  haskell-ds-forward-decl
  haskell-error-mode
  haskell-font-lock--forward-type
  haskell-forward-sexp
  haskell-goto-first-error
  haskell-goto-next-error
  haskell-goto-prev-error
  haskell-hide-toggle
  haskell-hide-toggle-all
  haskell-hoogle
  haskell-hoogle-kill-server
  haskell-hoogle-lookup-from-local
  haskell-hoogle-lookup-from-website
  haskell-hoogle-start-server
  haskell-indent-align-guards-and-rhs
  haskell-indent-cycle
  haskell-indent-insert-equal
  haskell-indent-insert-guard
  haskell-indent-insert-otherwise
  haskell-indent-insert-where
  haskell-indent-mode
  haskell-indent-put-region-in-literate
  haskell-indentation-common-electric-command
  haskell-indentation-indent-backwards
  haskell-indentation-indent-line
  haskell-indentation-indent-rigidly
  haskell-indentation-mode
  haskell-indentation-newline-and-indent
  haskell-interactive-bring
  haskell-interactive-copy-to-prompt
  haskell-interactive-kill
  haskell-interactive-mode
  haskell-interactive-mode-beginning
  haskell-interactive-mode-bol
  haskell-interactive-mode-clear
  haskell-interactive-mode-error-backward
  haskell-interactive-mode-error-forward
  haskell-interactive-mode-history-next
  haskell-interactive-mode-history-previous
  haskell-interactive-mode-kill-whole-line
  haskell-interactive-mode-newline-indent
  haskell-interactive-mode-prompt-next
  haskell-interactive-mode-prompt-previous
  haskell-interactive-mode-reset-error
  haskell-interactive-mode-return
  haskell-interactive-mode-space
  haskell-interactive-mode-tab
  haskell-interactive-mode-visit-error
  haskell-interactive-switch
  haskell-interactive-switch-back
  haskell-kill-nested
  haskell-kill-session-process
  haskell-literate-mode
  haskell-menu
  haskell-menu-mode
  haskell-menu-mode-ret
  haskell-mode
  haskell-mode-enable-process-minor-mode
  haskell-mode-find-uses
  haskell-mode-format-imports
  haskell-mode-generate-tags
  haskell-mode-goto-loc
  haskell-mode-insert-scc-at-point
  haskell-mode-jump-to-def
  haskell-mode-jump-to-def-or-tag
  haskell-mode-jump-to-tag
  haskell-mode-kill-scc-at-point
  haskell-mode-menu
  haskell-mode-show-type-at
  haskell-mode-stylish-buffer
  haskell-mode-tag-find
  haskell-mode-toggle-scc-at-point
  haskell-mode-view-news
  haskell-move-nested-left
  haskell-move-nested-right
  haskell-navigate-imports
  haskell-navigate-imports-go
  haskell-navigate-imports-return
  haskell-presentation-clear
  haskell-presentation-mode
  haskell-process-cabal
  haskell-process-cabal-build
  haskell-process-cabal-macros
  haskell-process-cd
  haskell-process-clear
  haskell-process-do-info
  haskell-process-do-type
  haskell-process-generate-tags
  haskell-process-interrupt
  haskell-process-load-file
  haskell-process-load-or-reload
  haskell-process-minimal-imports
  haskell-process-reload
  haskell-process-reload-devel-main
  haskell-process-restart
  haskell-process-unignore
  haskell-rgrep
  haskell-session-change
  haskell-session-change-target
  haskell-session-kill
  haskell-sort-imports
  haskell-svg-toggle-render-images
  haskell-unicode-input-method-enable
  haskell-update-ghc-support
  haskell-version
  haskell-w3m-open-haddock
  haskell-yesod-parse-routes-mode
  )


;;}}}
(provide 'emacspeak-haskell)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

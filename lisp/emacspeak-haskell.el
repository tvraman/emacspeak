;;; $Author: tv.raman.tv $
;;; emacspeak-haskell.el --- Speech-enable HASKELL  -*- lexical-binding: t; -*-
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

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
   (haskell-constructor-face voice-bolden)
   (haskell-debug-heading-face voice-smoothen-extra)
   (haskell-debug-keybinding-face voice-smoothen)
   (haskell-debug-muted-face voice-annotate)
   (haskell-debug-newline-face voice-monotone-extra)
   (haskell-debug-trace-number-face voice-lighten)
   (haskell-debug-warning-face voice-warning)
   (haskell-definition-face voice-type-personality)
   (haskell-error-face voice-warning)
   (haskell-hole-face voice-bolden)
   (haskell-interactive-face-compile-error voice-warning)
   (haskell-interactive-face-compile-warning voice-warning)
   (haskell-interactive-face-garbage voice-monotone-medium)
   (haskell-interactive-face-prompt voice-brighten)
   (haskell-interactive-face-prompt-cont voice-brighten-extra)
   (haskell-interactive-face-result voice-lighten)
   (haskell-keyword-face voice-keyword-personality)
   (haskell-liquid-haskell-annotation-face voice-bolden-extra)
   (haskell-literate-comment-face voice-monotone-extra)
   (haskell-operator-face voice-smoothen-extra)
   (haskell-pragma-face voice-monotone-medium)
   (haskell-quasi-quote-face voice-string-personality)
   (haskell-type-face voice-type-personality)
   (haskell-warning-face voice-warning)))

;;}}}
;;{{{ Interactive Commands:
'(
  haskell-delete-nested
  haskell-describe
  haskell-ds-backward-decl
  haskell-ds-forward-decl
  haskell-error-mode
  haskell-font-lock--forward-type
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
  haskell-yesod-parse-routes-mode
  )

(cl-loop
 for f in
 '(haskell-add-import haskell-align-imports haskell-auto-insert-module-template)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'task-done)))))

(cl-loop
 for f in
 '(
   haskell-cabal-beginning-of-section haskell-cabal-beginning-of-subsection
   haskell-cabal-end-of-section haskell-cabal-end-of-subsection
   haskell-cabal-goto-benchmark-section haskell-cabal-goto-common-section
   haskell-cabal-goto-executable-section haskell-cabal-goto-exposed-modules
   haskell-cabal-goto-library-section haskell-cabal-goto-test-suite-section
   haskell-cabal-next-section haskell-cabal-next-subsection
   haskell-cabal-previous-section haskell-cabal-previous-subsection
   haskell-cabal-section-end haskell-cabal-indent-line haskell-delete-indentation
   haskell-forward-sexp haskell-goto-first-error
   haskell-goto-next-error haskell-goto-prev-error
   haskell-mode-jump-to-def
   haskell-mode-jump-to-def-or-tag haskell-mode-jump-to-tag)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))

(defadvice haskell-cabal-mode (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-setup-programming-mode)))

;;}}}
;;{{{haskell-debugger:

;;}}}
;;{{{haskell-interactive

;;}}}
;;{{{haskell-indentation

(cl-loop
 for f in
 '(
   haskell-indentation-common-electric-command haskell-indentation-indent-backwards
   haskell-indentation-indent-line haskell-indentation-indent-rigidly
   haskell-indentation-newline-and-indent)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'select-object)))))

;;}}}
;;{{{haskell-mode-hook:

(add-hook
 'haskell-mode-hook
 #'(lambda ()
     (haskell-indentation-mode )))

;;}}}
(provide 'emacspeak-haskell)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

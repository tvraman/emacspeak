;;; emacspeak-lispy.el --- Speech-enable LISPY  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable LISPY An Emacs Interface to lispy
;;; Keywords: Emacspeak,  Audio Desktop lispy
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
;;; MERCHANTABILITY or FITNLISPY FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; LISPY ==  smart Navigation Of Lisp code
;;; This module speech-enables lispy.
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(eval-when-compile (require 'lispy "lispy" 'no-error))

;;}}}
;;{{{ Map Faces:

(voice-setup-add-map 
'(
(lispy-command-name-face voice-bolden)
(lispy-cursor-face voice-animate)
(lispy-face-hint voice-smoothen)
(lispy-face-key-nosel voice-monotone)
(lispy-face-key-sel voice-brighten)
(lispy-face-opt-nosel voice-monotone)
(lispy-face-opt-sel voice-lighten)
(lispy-face-req-nosel voice-monotone )
(lispy-face-req-sel voice-brighten-extra)
(lispy-face-rst-nosel voice-monotone)
(lispy-face-rst-sel voice-lighten-extra)
(lispy-test-face voice-annotate)))

;;}}}
;;{{{ Setup:

(defun emacspeak-lispy-setup ()
  "Setup emacspeak for use with lispy"
  (cl-declare (special lispy-mode-map))
  (define-key lispy-mode-map (kbd "C-e") 'emacspeak-prefix-command))

(emacspeak-lispy-setup)

;;}}}
;;{{{ Interactive Commands:

'(
  lispy--ediff-regions
lispy-ace-char
lispy-ace-paren
lispy-ace-subword
lispy-ace-symbol
lispy-ace-symbol-replace
lispy-alt-line
lispy-alt-multiline
lispy-arglist-inline
lispy-at
lispy-back
lispy-backtick
lispy-backward
lispy-backward-barf-sexp
lispy-backward-delete
lispy-backward-kill-word
lispy-backward-slurp-sexp
lispy-barf
lispy-barf-to-point
lispy-barf-to-point-nostring
lispy-beginning-of-defun
lispy-bind-variable
lispy-braces
lispy-braces-auto-wrap
lispy-braces-barf-to-point-or-jump-nostring
lispy-brackets
lispy-brackets-auto-wrap
lispy-brackets-barf-to-point-or-jump-nostring
lispy-buffer-kill-ring-save
lispy-build-semanticdb
lispy-cleanup
lispy-clone
lispy-close-curly
lispy-close-round-and-newline
lispy-close-square
lispy-colon
lispy-comment
lispy-comment-region
lispy-convolute
lispy-convolute-left
lispy-convolute-sexp
lispy-cursor-ace
lispy-cursor-down
lispy-debug-step-in
lispy-dedent-adjust-parens
lispy-delete
lispy-delete-backward
lispy-delete-backward-or-splice-or-slurp
lispy-delete-or-splice-or-slurp
lispy-describe
lispy-describe-bindings-C-4
lispy-describe-inline
lispy-different
lispy-doublequote
lispy-down
lispy-down-slurp
lispy-edebug
lispy-edebug-stop
lispy-ediff-regions
lispy-ert
lispy-eval
lispy-eval-and-comment
lispy-eval-and-insert
lispy-eval-and-replace
lispy-eval-current-outline
lispy-eval-expression
lispy-eval-other-window
lispy-expr-canonical-p
lispy-extract-block
lispy-extract-defun
lispy-fill
lispy-flatten
lispy-flow
lispy-follow
lispy-forward
lispy-forward-barf-sexp
lispy-forward-delete
lispy-forward-slurp-sexp
lispy-goto
lispy-goto-def-ace
lispy-goto-def-down
lispy-goto-elisp-commands
lispy-goto-local
lispy-goto-mode
lispy-goto-projectile
lispy-goto-recursive
lispy-goto-symbol
lispy-goto-verb
lispy-hash
lispy-hat
lispy-iedit
lispy-indent-adjust-parens
lispy-insert-outline-below
lispy-insert-outline-left
lispy-join
lispy-kill
lispy-kill-at-point
lispy-kill-sentence
lispy-kill-word
lispy-knight-down
lispy-knight-up
lispy-left
lispy-let-flatten
lispy-map-done
lispy-mark
lispy-mark-car
lispy-mark-left
lispy-mark-list
lispy-mark-right
lispy-mark-symbol
lispy-meta-doublequote
lispy-meta-return
lispy-mode
lispy-move-beginning-of-line
lispy-move-down
lispy-move-end-of-line
lispy-move-left
lispy-move-outline-up
lispy-move-right
lispy-move-up
lispy-multiline
lispy-narrow
lispy-new-copy
lispy-newline-and-indent
lispy-newline-and-indent-plain
lispy-occur
lispy-oneline
lispy-open-curly
lispy-open-line
lispy-open-square
lispy-other-mode
lispy-other-space
lispy-other-verb
lispy-out-forward-newline
lispy-outline-demote
lispy-outline-goto-child
lispy-outline-left
lispy-outline-next
lispy-outline-prev
lispy-outline-promote
lispy-outline-right
lispy-parens
lispy-parens-auto-wrap
lispy-parens-barf-to-point-or-jump-nostring
lispy-parens-down
lispy-paste
lispy-quit
lispy-quotes
lispy-raise
lispy-raise-sexp
lispy-raise-some
lispy-repeat
lispy-reverse
lispy-right
lispy-right-nostring
lispy-setq
lispy-shifttab
lispy-slurp
lispy-slurp-or-barf-left
lispy-slurp-or-barf-right
lispy-space
lispy-splice
lispy-splice-sexp-killing-backward
lispy-split
lispy-store-region-and-buffer
lispy-string-oneline
lispy-stringify
lispy-stringify-oneline
lispy-tab
lispy-teleport
lispy-tick
lispy-tilde
lispy-to-cond
lispy-to-defun
lispy-to-ifs
lispy-to-lambda
lispy-unbind-variable
lispy-unbind-variable-clojure
lispy-underscore
lispy-undo
lispy-unstringify
lispy-up
lispy-up-slurp
lispy-view
lispy-view-test
lispy-visit
lispy-widen
lispy-wrap-braces
lispy-wrap-brackets
lispy-wrap-round
lispy-x
lispy-x-more-verbosity
lispy-yank
)

;;}}}
(provide 'emacspeak-lispy)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

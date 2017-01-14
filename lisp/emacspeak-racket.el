;;; emacspeak-racket.el --- Speech-enable RACKET  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable RACKET An Emacs IDE for  racket
;;; Keywords: Emacspeak,  Audio Desktop racket IDE
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
;;; MERCHANTABILITY or FITNRACKET FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; racket-mode implements an IDE for racket, a dialect of scheme.

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
   (racket-check-syntax-def-face voice-bolden )
   (racket-check-syntax-use-face  voice-annotate)
   (racket-here-string-face voice-lighten )
   (racket-keyword-argument-face voice-animate )
   (racket-paren-face voice-smoothen )
   (racket-selfeval-face voice-bolden-and-animate )))

;;}}}
;;{{{ Interactive Commands:

'(
  racket--orp/enter
  racket--orp/next
  racket--orp/nop
  racket--orp/prev
  racket--orp/quit
  racket--profile-next
  racket--profile-prev
  racket--profile-quit
  racket--profile-refresh
  racket--profile-show-zero
  racket--profile-sort
  racket--profile-visit
  racket--toggle-trace
  racket-align
  racket-backward-up-list
  racket-base-requires
  racket-bug-report
  racket-check-syntax-mode
  racket-check-syntax-mode-goto-def
  racket-check-syntax-mode-goto-next-def
  racket-check-syntax-mode-goto-next-use
  racket-check-syntax-mode-goto-prev-def
  racket-check-syntax-mode-goto-prev-use
  racket-check-syntax-mode-help
  racket-check-syntax-mode-quit
  racket-check-syntax-mode-rename
  racket-cycle-paren-shapes
  racket-describe
  racket-describe--next-button
  racket-describe--prev-button
  racket-describe-mode
  racket-doc
  racket-expand-again
  racket-expand-definition
  racket-expand-last-sexp
  racket-expand-region
  racket-find-collection
  racket-fold-all-tests
  racket-indent-line
  racket-insert-closing
  racket-insert-lambda
  racket-make-doc/write-reference-file
  racket-mode
  racket-mode-menu
  racket-open-require-path
  racket-profile
  racket-profile-mode
  racket-racket
  racket-raco-test
  racket-repl
  racket-repl--clean-image-cache
  racket-repl-eval-or-newline-and-indent
  racket-repl-mode
  racket-repl-mode-menu
  racket-repl-switch-to-edit
  racket-run
  racket-run-and-switch-to-repl
  racket-run-with-errortrace
  racket-send-definition
  racket-send-last-sexp
  racket-send-region
  racket-smart-open-bracket
  racket-test
  racket-tidy-requires
  racket-trim-requires
  racket-unalign
  racket-unfold-all-tests
  racket-unicode-input-method-enable
  racket-unvisit
  racket-view-last-image
  racket-visit-definition
  racket-visit-module
  )

;;}}}
(provide 'emacspeak-racket)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

;;; emacspeak-clojure.el --- Speech-enable CLOJURE  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable CLOJURE-mode
;;; Keywords: Emacspeak,  Audio Desktop clojure
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
;;; MERCHANTABILITY or FITNCLOJURE FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; CLOJURE-mode: Specialized mode for Clojure programming.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(eval-when-compile
  (require 'clojure-mode "clojure-mode" 'no-error))

;;}}}
;;{{{ Map Faces:

(voice-setup-add-map
 '(
   (clojure-interop-method-face  voice-lighten)
   (clojure-character-face voice-bolden-medium)
   (clojure-keyword-face voice-animate)))

;;}}}
;;{{{ Advice Interactive Commands:

'(clojure-align
  clojure-backward-logical-sexp
  clojure-cheatsheet
  
  clojure-cycle-if
  clojure-cycle-privacy
  clojure-forward-logical-sexp
  clojure-insert-ns-form
  clojure-insert-ns-form-at-point
  clojure-introduce-let
  clojure-let-backward-slurp-sexp
  clojure-let-forward-slurp-sexp
  clojure-mode
  clojure-mode-display-version
  clojure-mode-menu
  clojure-mode-report-bug
  clojure-move-to-let
  clojure-quick-repls-connect
  clojure-sort-ns
  clojure-thread
  clojure-thread-first-all
  clojure-thread-last-all
  clojure-toggle-keyword-string
  clojure-unwind
  clojure-unwind-all
  clojure-update-ns
  clojure-view-cheatsheet
  clojure-view-grimoire
  clojure-view-guide
  clojure-view-reference-section
  clojure-view-style-guide
  clojurec-mode
  clojurescript-mode
  clojurex-mode)

;;}}}
;;{{{ Speech-Enable Refactoring:

(loop
 for f in
 '(
   clojure-convert-collection-to-list clojure-convert-collection-to-map
                                      clojure-convert-collection-to-quoted-list clojure-convert-collection-to-set
  clojure-convert-collection-to-vector) do
  (eval
   `(defadvice ,f (pre act comp)
      "Provide auditory feedback."
      (when (ems-interactive-p)
        (emacspeak-speak-line)))))

;;}}}
(provide 'emacspeak-clojure)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

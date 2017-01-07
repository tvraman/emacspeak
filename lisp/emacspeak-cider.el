;;; emacspeak-cider.el --- Speech-enable CIDER  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable CIDER An Emacs Interface to cider
;;; Keywords: Emacspeak,  Audio Desktop, cider
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
;;; MERCHANTABILITY or FITNCIDER FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; Speech-Enable CIDER --- Clojure IDE
;;; CIDER == 

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
(cider-debug-code-overlay-face voice-monotone)
 (cider-debug-prompt-face voice-animate)
 (cider-deprecated-face voice-monotone)
 (cider-docview-emphasis-face voice-lighten)
 (cider-docview-literal-face voice-monotone-light)
 (cider-docview-strong-face voice-monotone-medium)
 (cider-enlightened-face voice-lighten)
 (cider-enlightened-local-face voice-lighten-extra)
 (cider-error-highlight-face voice-animate)
 (cider-fragile-button-face voice-annotate)
 (cider-instrumented-face voice-monotone-light)
 (cider-repl-input-face voice-animate)
 (cider-repl-prompt-face voice-annotate)
 (cider-repl-result-face voice-bolden)
 (cider-repl-stderr-facevoice-animate)
 (cider-repl-stdout-face voice-bolden-medium)
 (cider-result-overlay-face voice-bolden)
 (cider-stacktrace-error-class-face voice-animate)
 (cider-stacktrace-error-message-face voice-animate-extra)
 (cider-stacktrace-face voice-bolden)
 (cider-stacktrace-filter-hidden-face voice-smoothen)
 (cider-stacktrace-filter-shown-face voice-bolden-and-animate)
 (cider-stacktrace-fn-face voice-bolden)
 (cider-stacktrace-ns-face voice-smoothen)
 (cider-stacktrace-promoted-button-face voice-animate)
 (cider-stacktrace-suppressed-button-face voice-smoothen-extra)
 (cider-test-error-face voice-animate)
 (cider-test-failure-face voice-animate-extra)
 (cider-test-success-face voice-bolden-medium)
 (cider-traced-face voice-bolden)
 (cider-warning-highlight-face voice-animate-extra)
))

;;}}}
(provide 'emacspeak-cider)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

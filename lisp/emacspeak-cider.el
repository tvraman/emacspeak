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
;;{{{ Interactive Commands:

'(
  
  cider-auto-test-mode
  cider-browse-instrumented-defs
  cider-browse-ns
  cider-browse-ns-all
  cider-browse-ns-doc-at-point
  cider-browse-ns-find-at-point
  cider-browse-ns-handle-mouse
  cider-browse-ns-mode
  cider-browse-ns-operate-at-point
  cider-change-buffers-designation
  cider-classpath
  cider-clear-buffer-local-connection
  cider-clear-compilation-highlights
  cider-clojure-mode-menu-open
  cider-close-ancillary-buffers
  cider-close-nrepl-session
  cider-connect
  cider-connection-browser
  cider-connections-buffer-mode
  cider-connections-close-connection
  cider-connections-goto-connection
  cider-connections-make-default
  cider-create-sibling-cljs-repl
  cider-debug-defun-at-point
  cider-debug-mode-menu
  cider-debug-mode-send-reply
  cider-debug-move-here
  cider-debug-toggle-locals
  cider-describe-nrepl-session
  cider-disable-on-existing-clojure-buffers
  cider-display-connection-info
  cider-doc
  cider-docview-grimoire
  cider-docview-grimoire-web
  cider-docview-javadoc
  cider-docview-mode
  cider-docview-mode-menu
  cider-docview-source
  cider-drink-a-sip
  cider-enable-on-existing-clojure-buffers
  cider-enlighten-mode
  cider-eval-buffer
  cider-eval-defun-at-point
  cider-eval-defun-to-comment
  cider-eval-file
  cider-eval-last-sexp
  cider-eval-last-sexp-and-replace
  cider-eval-last-sexp-to-repl
  cider-eval-ns-form
  cider-eval-print-last-sexp
  cider-eval-region
  cider-eval-sexp-at-point
  cider-find-and-clear-repl-output
  cider-find-dwim
  cider-find-dwim-other-window
  cider-find-ns
  cider-find-resource
  cider-find-var
  cider-format-buffer
  cider-format-defun
  cider-format-edn-buffer
  cider-format-edn-region
  cider-format-region
  cider-grimoire
  cider-grimoire-web
  cider-insert-defun-in-repl
  cider-insert-last-sexp-in-repl
  cider-insert-ns-form-in-repl
  cider-insert-region-in-repl
  cider-inspect
  cider-inspect-defun-at-point
  cider-inspect-expr
  cider-inspect-last-result
  cider-inspect-last-sexp
  cider-inspect-read-and-inspect
  cider-inspector-mode
  cider-inspector-next-inspectable-object
  cider-inspector-next-page
  cider-inspector-operate-on-click
  cider-inspector-operate-on-point
  cider-inspector-pop
  cider-inspector-prev-page
  cider-inspector-previous-inspectable-object
  cider-inspector-refresh
  cider-inspector-set-page-size
  cider-interrupt
  cider-jack-in
  cider-jack-in-clojurescript
  cider-javadoc
  cider-jump-to-compilation-error
  cider-load-all-project-ns
  cider-load-buffer
  cider-load-buffer-and-switch-to-repl-buffer
  cider-load-file
  cider-macroexpand-1
  cider-macroexpand-all
  cider-make-connection-default
  cider-mode
  cider-mode-eval-menu-open
  cider-mode-interactions-menu-open
  cider-mode-menu-open
  cider-open-classpath-entry
  cider-ping
  cider-pop-back
  cider-popup-buffer-mode
  cider-popup-buffer-quit
  cider-popup-buffer-quit-function
  cider-pprint-eval-defun-at-point
  cider-pprint-eval-last-sexp
  cider-pprint-eval-last-sexp-to-repl
  cider-quit
  cider-read-and-eval
  cider-read-and-eval-defun-at-point
  cider-refresh
  cider-refresh-dynamic-font-lock
  cider-repl-backward-input
  cider-repl-beginning-of-defun
  cider-repl-bol-mark
  cider-repl-clear-banners
  cider-repl-clear-buffer
  cider-repl-clear-help-banner
  cider-repl-clear-output
  cider-repl-closing-return
  cider-repl-end-of-defun
  cider-repl-forward-input
  cider-repl-handle-shortcut
  cider-repl-history-load
  cider-repl-history-save
  cider-repl-indent-and-complete-symbol
  cider-repl-kill-input
  cider-repl-mode
  cider-repl-mode-menu
  cider-repl-newline-and-indent
  cider-repl-next-input
  cider-repl-next-matching-input
  cider-repl-next-prompt
  cider-repl-previous-input
  cider-repl-previous-matching-input
  cider-repl-previous-prompt
  cider-repl-require-repl-utils
  cider-repl-return
  cider-repl-set-ns
  cider-repl-shortcuts-help
  cider-repl-switch-to-other
  cider-repl-tab
  cider-repl-toggle-pretty-printing
  cider-replicate-connection
  cider-report-bug
  cider-restart
  cider-rotate-default-connection
  cider-run
  cider-scratch
  cider-selector
  cider-spy-summary
  cider-stacktrace-cycle-all-causes
  cider-stacktrace-cycle-cause-1
  cider-stacktrace-cycle-cause-2
  cider-stacktrace-cycle-cause-3
  cider-stacktrace-cycle-cause-4
  cider-stacktrace-cycle-cause-5
  cider-stacktrace-cycle-current-cause
  cider-stacktrace-jump
  cider-stacktrace-mode
  cider-stacktrace-mode-menu
  cider-stacktrace-next-cause
  cider-stacktrace-previous-cause
  cider-stacktrace-toggle-all
  cider-stacktrace-toggle-clj
  cider-stacktrace-toggle-duplicates
  cider-stacktrace-toggle-java
  cider-stacktrace-toggle-repl
  cider-stacktrace-toggle-tooling
  cider-switch-to-last-clojure-buffer
  cider-switch-to-repl-buffer
  cider-test-clear-highlights
  cider-test-ediff
  cider-test-ediff-cleanup
  cider-test-jump
  cider-test-next-result
  cider-test-previous-result
  cider-test-report-mode
  cider-test-report-mode-menu
  cider-test-rerun-failed-tests
  cider-test-rerun-test
  cider-test-run-loaded-tests
  cider-test-run-ns-tests
  cider-test-run-project-tests
  cider-test-run-test
  cider-test-show-report
  cider-test-stacktrace
  cider-toggle-buffer-connection
  cider-toggle-request-dispatch
  cider-toggle-trace-ns
  cider-toggle-trace-var
  cider-undef
  cider-version
  cider-view-manual
  cider-view-refcard
  cider-visit-error-buffer)

;;}}}
;;{{{ Apropos:

(cl-loop
 for f in
 '(
   cider-apropos cider-apropos-documentation
  cider-apropos-documentation-select cider-apropos-select)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-speak-mode-line)
       (emacspeak-auditory-icon 'open-object)))))

;;}}}
;;{{{ Assoc Connection:

(cl-loop
 for f in
 '(cider-assoc-buffer-with-connection cider-assoc-project-with-connection)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'task-done)))))

;;}}}
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
(provide 'emacspeak-cider)

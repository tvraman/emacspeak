;;; emacspeak-cider.el --- Speech-enable CIDER, A Clojure IDE  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable CIDER An Emacs Interface to cider
;;; Keywords: Emacspeak,  Audio Desktop, cider
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA..

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; Speech-Enable CIDER --- Clojure IDE


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
   (cider-debug-code-overlay-face voice-monotone-extra)
   (cider-debug-prompt-face voice-animate)
   (cider-deprecated-face voice-monotone-extra)
   (cider-docview-emphasis-face voice-lighten)
   (cider-docview-literal-face voice-monotone-medium)
   (cider-docview-strong-face voice-monotone-medium)
   (cider-enlightened-face voice-lighten)
   (cider-enlightened-local-face voice-lighten-extra)
   (cider-error-highlight-face voice-animate)
   (cider-fragile-button-face voice-annotate)
   (cider-instrumented-face voice-monotone-medium)
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
;;{{{ Apropos:

(cl-loop
 for f in
 '(
   cider-visit-error-buffer
   cider-selector cider-scratch
   cider-switch-to-last-clojure-buffer cider-switch-to-repl-buffer
   cider-apropos cider-apropos-documentation
   cider-apropos-documentation-select cider-apropos-select)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-speak-mode-line)
       (emacspeak-auditory-icon 'open-object)))))

;;}}}
;;{{{ Associate Connection:

(cl-loop
 for f in
 '(
   cider-assoc-buffer-with-connection cider-assoc-project-with-connection
   cider-format-buffer cider-format-region
   cider-format-edn-region cider-format-edn-buffer
   cider-undef)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'task-done)))))

;;}}}
;;{{{ Browse:

(cl-loop
 for f in
 '(
   cider-browse-instrumented-defs cider-browse-ns cider-browse-ns-all
   cider-browse-ns-operate-at-point cider-browse-ns-doc-at-point
   cider-classpath-operate-on-point
   cider-browse-ns-find-at-point cider-classpath cider-doc)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (with-current-buffer (window-buffer (selected-window))
         (emacspeak-auditory-icon 'open-object)
         (emacspeak-speak-line))))))

;;}}}
;;{{{ Speech-enable Eval:

(cl-loop
 for f in
 '(
   cider-eval-defun-at-point cider-eval-defun-to-comment cider-eval-file
   cider-eval-last-sexp cider-eval-last-sexp-and-replace
   cider-eval-last-sexp-to-repl cider-eval-ns-form cider-eval-print-last-sexp
   cider-eval-buffer cider-eval-region cider-eval-sexp-at-point)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'task-done)))))

;;}}}
;;{{{ cider-repl:

;;; Navigators:

(cl-loop
 for f in
 '(
   cider-repl-previous-prompt cider-repl-previous-matching-input
   cider-repl-previous-input cider-repl-next-prompt
   cider-repl-next-matching-input cider-repl-next-input
   cider-repl-forward-input  cider-repl-backward-input
   cider-repl-end-of-defun cider-repl-beginning-of-defun
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
 '(cider-repl-closing-return cider-repl-return)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (save-excursion
         (goto-char
          (previous-single-property-change (point)   'face nil (point-min)))
         (emacspeak-speak-this-personality-chunk))
       (emacspeak-auditory-icon 'close-object)))))

(cl-loop
 for f in
 '(
   cider-clear-compilation-highlights cider-repl-kill-input
   cider-scratch-reset
   cider -repl-clear-banners cider-repl-clear-buffer
   cider-find-and-clear-repl-output
   cider-repl-clear-help-banner cider-repl-clear-output)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'delete-object)))))

(cl-loop
 for f in
 '(
   cider-repl-tab cider-repl-indent-and-complete-symbol
   cider-repl-newline-and-indent cider-repl-bol-mark)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'select-object)))))

(defadvice cider-repl-switch-to-other(after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'select-object)))
(defadvice cider-repl-set-ns (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice cider-repl-toggle-pretty-printing (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon (f cider-repl-use-pretty-printing 'on 'off))
    (message "Turned  %s pretty printing."
             (if cider-repl-use-pretty-printing 'on 'off))))

;;}}}
;;{{{ find:

(cl-loop
 for f in
 '(cider-find-var cider-find-resource cider-find-ns)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."(when (ems-interactive-p)
                                   (emacspeak-speak-line)))))

;;}}}
;;{{{ misc commands:
(cl-loop
 for f in
 '(cider-popup-buffer-quit-function cider-popup-buffer-quit)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (with-current-buffer (window-buffer (selected-window))
         (emacspeak-auditory-icon 'close-object)
         (emacspeak-speak-mode-line))))))

(defadvice cider-connections-goto-connection (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'open-object)))

(defadvice cider-connect (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'open-object)))

(defadvice  cider-close-nrepl-session(after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Closed Repl Session")))
(defadvice cider-close-ancillary-buffers (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Closed ancillary buffers")))
(cl-loop
 for f in
 '(cider-describe-nrepl-session cider-connection-browser
                                cider-display-connection-info)
 do
 (eval
  `(defadvice ,f  (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)
       (message "Displayed in other window.")))))

;;}}}
;;{{{ Speech-enable Debug:
(cl-loop
 for f in
 '(cider-debug-defun-at-point cider-debug-move-here cider-debug-toggle-locals)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'button)))))

;;}}}
;;{{{ Speech-enable Insert:

(cl-loop
 for f in
 '(
   cider-insert-defun-in-repl cider-insert-last-sexp-in-repl
   cider-insert-ns-form-in-repl cider-insert-region-in-repl)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'yank-object)))))

;;}}}
;;{{{ Inspect And Inspector:

(cl-loop
 for f in
 '(
   cider-inspector-refresh
   cider-inspect cider-inspect-defun-at-point
   cider-inspect-expr cider-inspect-last-result
   cider-inspect-last-sexp cider-inspect-read-and-inspect cider-inspector-pop)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)
       (emacspeak-speak-mode-line)))))
(cl-loop
 for f in
 '(
   cider-inspector-next-inspectable-object cider-inspector-next-page
   cider-inspector-operate-on-click cider-inspector-operate-on-point
   cider-inspector-prev-page cider-inspector-previous-inspectable-object
   cider-stacktrace-cycle-current-cause cider-stacktrace-cycle-all-causes
   cider-stacktrace-cycle-cause-1 cider-stacktrace-cycle-cause-2
   cider-stacktrace-cycle-cause-3 cider-stacktrace-cycle-cause-4
   cider-stacktrace-cycle-cause-5
   cider-stacktrace-next-cause cider-stacktrace-previous-cause
   cider-stacktrace-jump)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'select-object)
       (emacspeak-speak-line)))))

;;}}}
(provide 'emacspeak-cider)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

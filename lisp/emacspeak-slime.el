;;; emacspeak-slime.el --- Speech-enable SLIME  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable SLIME An Emacs Interface to slime
;;; Keywords: Emacspeak,  Audio Desktop slime
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
;;; MERCHANTABILITY or FITNSLIME FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
 ;;; SLIME == Superior  Lisp Interaction Mode For Emacs

;;; Slime is a powerful IDE for developing in Common Lisp and Clojure.
;;; It's similar but more modern than package ILisp that I used as a
;;; graduate student when developing AsTeR.

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
   (slime-error-face voice-animate)
   (slime-warning-face voice-animate-medium)
   (slime-style-warning-face voice-animate-medium)
   (slime-note-face voice-monotone)
   (slime-highlight-face voice-bolden)
   (slime-apropos-symbol voice-monotone-light)
   (slime-apropos-label voice-monotone-light)
   (slime-inspector-topline-face voice-bolden-medium)
   (slime-inspector-label-face voice-monotone-medium)
   (slime-inspector-value-face voice-animate)
   (slime-inspector-action-face voice-bolden)
   (slime-inspector-type-face voice-annotate)
   (sldb-catch-tag-face   voice-lighten)
   (sldb-condition-face  voice-smoothen)
   (sldb-detailed-frame-line-face voice-monotone)
   (sldb-frame-label-face voice-annotate)
   (sldb-frame-line-face voice-lighten-extra)
   (sldb-local-name-face voice-bolden)
   (sldb-local-value-face voice-animate)
   (sldb-non-restartable-frame-line-face voice-animate-extra)
   (sldb-reference-face voice-smoothen-extra)
   (sldb-restart-face voice-bolden)
   (sldb-restart-number-face voice-smoothen)
   (sldb-restart-type-face voice-animate)
   (sldb-restartable-frame-line-face voice-bolden)
   (sldb-section-face voice-bolden-medium)
   (sldb-topline-face voice-bolden)
   (slime-reader-conditional-face  voice-brighten)
   (slime-repl-input-face voice-brighten-medium)
   (slime-repl-inputed-output-face voice-bolden-and-animate)
   (slime-repl-output-face voice-bolden)
   (slime-repl-output-mouseover-face voice-bolden-and-animate)
   (slime-repl-prompt-face voice-smoothen)
   (slime-repl-result-face voice-animate)))

;;}}}
;;{{{ Navigation And Repl:
(defadvice slime-selector (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))


(loop
 for f in
 '(
   slime-repl-backward-input slime-repl-forward-input
                             slime-repl-previous-matching-input slime-repl-previous-input
                             slime-repl-next-matching-input slime-repl-next-input
                             slime-repl-end-of-defun slime-repl-beginning-of-defun
                             slime-end-of-defun                   slime-beginning-of-defun
                             slime-close-all-parens-in-sexp
                             slime-repl-previous-prompt slime-repl-next-prompt
                             slime-next-presentation slime-previous-presentation
                             slime-next-location slime-previous-location
                             slime-edit-definition slime-pop-find-definition-stack
                             slime-edit-definition-other-frame slime-edit-definition-other-window
                             slime-next-note slime-previous-note
                             )
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))

(loop
 for f in
 '(slime-repl-return slime-repl-closing-return
                     slime-repl-set-package slime-handle-repl-shortcut)
 do
 (eval
  `(defadvice  ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (save-excursion
         (goto-char
          (previous-single-property-change (point)   'face nil (point-min)))
         (emacspeak-speak-this-personality-chunk))
       (emacspeak-auditory-icon 'close-object)))))

(loop
 for f in
 '(slime-complete-symbol slime-indent-and-complete-symbol)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Say what you completed."
     (ems-with-messages-silenced
      (let ((prior (point)))
        ad-do-it
        (if (> (point) prior)
            (tts-with-punctuations
             'all
             (dtk-speak (buffer-substring prior (point))))
          (emacspeak-speak-completions-if-available))
        ad-return-value)))))

(loop
 for f in
 '(
   slime-repl-delete-from-input-history slime-repl-delete-current-input
                                        slime-repl-kill-input
                                        slime-repl-clear-output slime-repl-clear-buffer)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'delete-object)))))

(loop
 for f in
 '(
   slime-repl-sayoonara slime-repl-quit slime-
                        repl-disconnect-all slime-repl-disconnect)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'close-object)))))

(loop
 for f in
 '(
   slime-repl-test/force-system slime-repl-test-system
                                slime-repl-reload-system slime-repl-open-system
                                slime-repl-load/force-system slime-repl-load-system
                                slime-repl-delete-system-fasls slime-repl-compile/force-system  slime-repl-compile-system
                                slime-repl-compile-and-load slime-repl-browse-system)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'task-done)))))

(defadvice slime-repl-inspect (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)))

(defadvice slime-repl-shortcut-help (after emacspeak pre act comp)

(defadvice slime-cheat-sheet (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'help)
    (dtk-speak "Displaying  help in new frame.")))

  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'help)
    (dtk-speak "Displayed help in other window.")))

;;}}}
;;{{{ Writing Code:

;;}}}
;;{{{ Lisp Interaction:

;;}}}
;;{{{ Browsing Documentation:

(loop
 for f in
 '(
   slime-documentation-lookup
   slime-describe-function  slime-describe-symbol slime-describe-presentation
   slime-apropos slime-apropos-package slime-apropos-summary
   )
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'help)))))

;;}}}
;;{{{ Inspector:

(loop
 for f in
 '(slime-inspector-next-inspectable-object slime-inspector-previous-inspectable-object)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-speak-this-personality-chunk)
       (emacspeak-auditory-icon 'large-movement)))))

(loop
 for f in
 '(
   slime-inspector-operate-on-point slime-inspector-operate-on-click
                                    slime-inspector-show-source
                                    slime-inspect slime-inspect-definition
                                    slime-inspector-reinspect slime-inspector-show-source
                                    slime-inspector-next
                                    slime-inspector-fetch-all
                                    slime-inspect-presentation-at-mouse slime-inspect-presentation-at-point)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (with-current-buffer (get-buffer "*slime-inspector*")
         (emacspeak-speak-line)
         (emacspeak-auditory-icon 'open-object))))))

(loop
 for f in
 '(slime-inspector-history slime-inspector-describe)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (with-current-buffer (get-buffer"*slime-description*")
         (emacspeak-speak-buffer)
         (emacspeak-auditory-icon 'help))))))

'(
  slime-inspector-copy-down-to-repl

  slime-inspector-eval

  slime-inspector-next-inspectable-object
  slime-inspector-operate-on-click
  slime-inspector-operate-on-point
  slime-inspector-pop
  slime-inspector-pprint
  slime-inspector-previous-inspectable-object
  slime-inspector-quit

  slime-inspector-toggle-verbose)

;;}}}
;;{{{ Debugger:

;;}}}
(provide 'emacspeak-slime)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

;;; emacspeak-js2.el --- Speech-enable JS2
;;; $Id$
;;; $Author: raman $
;;; Description:  Speech-enable JS2 An Emacs Interface to js2
;;; Keywords: Emacspeak,  Audio Desktop js2
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2008/04/03 15:05:55 $ |
;;;  $Revision: 1.1 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2011, T. V. Raman
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
;;; MERCHANTABILITY or FITNJS2 FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; JS2-mode http://js2-mode.googlecode.com/svn/trunk
;;; is a new, powerful Emacs mode for working with JavaScript.
;;; This module speech-enables js2.

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{  map faces to voices:

(voice-setup-add-map
 '(
   (js2-error-face voice-bolden-extra)
   (js2-external-variable-face voice-animate) 
   (js2-function-param-face voice-lighten-extra)
   (js2-instance-member-face voice-lighten-medium)
   (js2-jsdoc-html-tag-delimiter-face voice-smoothen)
   (js2-jsdoc-html-tag-name-face voice-bolden-medium)
   (js2-jsdoc-tag-face voice-bolden-medium)
   (js2-jsdoc-type-face voice-smoothen-medium)
   (js2-jsdoc-value-face voice-lighten-medium)
   (js2-magic-paren-face voice-lighten)
   (js2-private-function-call-face voice-smoothen-extra)
   (js2-private-member-face voice-lighten-extra)
   (js2-warning-face voice-bolden-and-animate)
   ))

;;}}}
;;{{{ Advice new interactive commands:
(defadvice js2-mark-defun  (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-speak-line)))

(loop for f in
      '(js2-mode-forward-sexp js2-mode-backward-sibling js2-next-error)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon 'large-movement)
            (emacspeak-speak-line)))))
(loop for f in
      '(
        js2-beginning-of-line js2-indent-line
                              js2-indent-bounce-backwards js2-forward-sws
                              js2-backward-sws js2-enter-key
                              js2-end-of-line
                              js2-mode-match-single-quote js2-mode-match-paren
                              js2-mode-match-double-quote js2-mode-match-curly
                              js2-mode-match-bracket js2-mode-magic-close-paren
                              js2-insert-and-indent     )
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-speak-line)))))

(loop for f in
      '(js2-mode-hide-comments js2-mode-hide-element
                               js2-mode-hide-functions js2-mode-hide-warnings-and-errors)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon 'close-object)
            (message "Hid %s"
                     ,(substring (symbol-name f)
                                 (length "js2-mode-hide-")))))))

(loop for f in
      '(js2-mode-show-all js2-mode-show-comments
                          js2-mode-show-element js2-mode-show-functions
                          js2-mode-show-node)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon 'open-object)
            (message "Showed %s"
                     ,(substring (symbol-name f)
                                 (length "js2-mode-show-")))))))

(loop for f in
      '(js2-mode-toggle-warnings-and-errors
        js2-mode-toggle-hide-functions
        js2-mode-toggle-hide-comments                    js2-mode-toggle-element)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon 'button)
            (message "Toggled %s"
                     ,(substring (symbol-name f)
                                 (length "js2-mode-toggle-")))))))
;;                   js2-narrow-to-defun
;;                   js2-next-error)
                                        ;js2-mode-show-node

;;}}}
;;{{{ js2-mode hook

(defun emacspeak-js2-hook ()
  "Hook to setup emacspeak."
  (declare (special js2-mode-map))
  (define-key js2-mode-map "\C-e" 'emacspeak-prefix-command)
  (define-key js2-mode-map "\C-ee" 'js2-end-of-line)
  (emacspeak-setup-programming-mode)
  (when (locate-library "js2-imenu-extras")
    (require 'js2-imenu-extras)
    (js2-imenu-extras-setup)))

(add-hook 'js2-mode-hook 'emacspeak-js2-hook)

;;}}}
(provide 'emacspeak-js2)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

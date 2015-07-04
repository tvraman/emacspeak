;;; emacspeak-slime.el --- Speech-enable SLIME
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
;;{{{ Navigation:

(loop
 for f in
 '(
   slime-end-of-defun                   ;slime-beginning-of-defun
   slime-close-all-parens-in-sexp
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
 '(slime-complete-symbol slime-indent-and-complete-symbol)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
  "Say what you completed."
  (let ((prior (point ))
        (emacspeak-speak-messages nil))
    ad-do-it
    (if (> (point) prior)
        (tts-with-punctuations
         'all
         (dtk-speak (buffer-substring prior (point))))
      (emacspeak-speak-completions-if-available))
    ad-return-value))))

;;}}}
;;{{{ Writing Code:

;;}}}
;;{{{ Lisp Interaction:

;;}}}
;;{{{ Browsing Documentation:
(loop
 for f in
 '(
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

(provide 'emacspeak-slime)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

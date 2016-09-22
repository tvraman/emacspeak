;;; emacspeak-ems-new-interactive.el --- Speech-enable EMS-NEW-INTERACTIVE  -*- lexical-binding: t; -*-
;;; $Id: emacspeak-ems-new-interactive.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable EMS-NEW-INTERACTIVE An Emacs Interface to ems-new-interactive
;;; Keywords: Emacspeak,  Audio Desktop ems-new-interactive
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
;;; MERCHANTABILITY or FITNEMS-NEW-INTERACTIVE FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; EMS-NEW-INTERACTIVE ==  New Interactive Check 

;;; called-interactively-p  in emacs 24.4 and later depends on 
;;; advice--called-interactively-skip implemented in nadvice.el ---
;;; And this causes possible recursive deadlocks e.g., in ruby-mode.
;;; This module attempts to implement Emacspeak's own version of the 
;;; "Was I called interactively" check.
;;;
;;; Outline:
;;; Set our own flag ems-called-interactively via an advice on call-interactive.
;;; Define an ems-interactive-set-unset function 
;;; that returns t if our flag is set, and also unsets it immediately
;;; Finally use this return value in our custom predicate.
;;; The custom emacspeak predicate will continue to be called ems-interactive-p

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'advice)

;;}}}
;;{{{ Implementation:
(defvar ems-called-interactively-p nil
  "Flag recording interactive calls.")
(defsubst ems-record-interactive-p (f)
  "Predicate to test if we need to record interactive calls of
this function. Memoizes result for future use by placing a
property 'emacspeak on the function."
  (cond
   ((get f 'emacspeak) t)
   ((and
     (symbolp f)
     (or
      (string-match "^dtk-" (symbol-name f))
      (string-match "^emacspeak-" (symbol-name f))))
    (put f 'emacspeak t))
   ((ad-find-some-advice f 'any  "emacspeak")
    (put f 'emacspeak t))
   (t nil)))
    
  ))

(defadvice call-interactively (before emacspeak pre act comp)
  "Set our interactive flag."
  (setq ems-called-interactively-p t))


(defsubst ems-interactively-p ()
  "Check our interactive flag.
Return T if set, after turning off the flag."
  (declare (special ems-called-interactively-p))
  (cond
   (ems-called-interactively-p            ;interactive call
    (setq ems-called-interactively-p nil) ; turn off now that we used  it 
    t)
   (t nil)))


;;}}}
(provide 'emacspeak-ems-new-interactive)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

;;; emacspeak-xref.el --- Speech-enable XREF  -*- lexical-binding: t; -*-
;;; $Id: emacspeak-xref.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable XREF An Emacs Interface to xref
;;; Keywords: Emacspeak,  Audio Desktop xref
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
;;;Copyright (C) 1995 -- 2021, T. V. Raman
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
;;; MERCHANTABILITY or FITNXREF FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; XREF ==  Cross-references in source code.
;;; This is part of Emacs 25.
;;; This module speech-enables xref

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{  Advice Interactive Commands:

(cl-loop
 for   f in 
 '(
   xref-find-definitions xref-pop-marker-stack pop-tag-mark
   xref-next-line xref-prev-line xref-go-back
   xref-find-regexp  xref-pop-marker-stack
   xref-find-apropos xref-goto-xref)
 do
 (eval
  `(defadvice ,f (after emacspeak pre  act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'large-movement)))))

(cl-loop
 for f in 
 '(
   xref-find-definitions-other-frame  xref-find-definitions-other-window
   xref-show-location-at-point)
 do
 (eval
  `(defadvice ,f (after emacspeak pre  act comp)
     "speak."
     (when (ems-interactive-p)
       (message "Displayed cross-reference.")
       (emacspeak-auditory-icon 'select-object)))))

(defadvice xref-find-references (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'task-done)))

;;}}}
(provide 'emacspeak-xref)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

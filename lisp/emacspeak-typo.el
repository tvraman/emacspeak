;;; emacspeak-typo.el --- Speech-enable TYPO  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable TYPO An Emacs Interface to typo
;; Keywords: Emacspeak,  Audio Desktop typo
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; 
;;  $Revision: 4532 $ |
;; Location undetermined
;; 

;;}}}
;;{{{  Copyright:
;; Copyright (C) 1995 -- 2007, 2011, T. V. Raman
;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
;; All Rights Reserved.
;; 
;; This file is not part of GNU Emacs, but the same permissions apply.
;; 
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNTYPO FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;; TYPO ==  Typographical Editing
;; This module speech-enables typo-mode.
;; Typo-mode's magic insertion commands are speech-enabled to speak the inserted char.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Interactive Commands:

(cl-loop
 for f in 
 '(
   typo-insert-quotation-mark typo-cycle-dashes typo-cycle-ellipsis
   typo-cycle-left-angle-brackets typo-cycle-left-single-quotation-mark
   typo-cycle-multiplication-signs typo-cycle-right-angle-brackets
   typo-cycle-right-single-quotation-mark typo-cycle-spaces)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-speak-this-char (preceding-char))))))

;;}}}
(provide 'emacspeak-typo)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}

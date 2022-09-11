;;; emacspeak-rg.el --- Speech-enable RG  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable RG An Emacs Interface to rg
;; Keywords: Emacspeak,  Audio Desktop rg
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
;; MERCHANTABILITY or FITNRG FOR A PARTICULAR PURPOSE.  See the
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
;; RG ==  Emacs front-end to ripgrep (rg).

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'rg "rg" 'no-error)

;;}}}
;;{{{ Map Faces:

(voice-setup-add-map 
 '(
   (rg-context-face voice-bolden)
   (rg-error-face voice-animate)
   (rg-file-tag-face voice-smoothen)
   (rg-filename-face voice-annotate)
   (rg-info-face voice-monotone-extra)
   (rg-match-face voice-lighten)
   (rg-warning-face voice-animate)))

;;}}}
;;{{{ Interactive Commands:

(cl-loop
 for f in
 '(rg rg-dwim rg-project
      rg-rerun-change-dir rg-rerun-change-regexp rg-rerun-change-files
      rg-rerun-toggle-ignore rg-rerun-toggle-case)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'task-done)))))

(cl-loop
 for f in
 '(rg-next-file
   rg-prev-file)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'select-object)
       (emacspeak-speak-line)))))

(cl-loop
 for f in
 '(rg-save-search-as-name rg-save-search)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'select-object)
       (emacspeak-speak-mode-line)))))

;;}}}
(provide 'emacspeak-rg)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}

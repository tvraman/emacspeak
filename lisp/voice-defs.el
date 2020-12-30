;;; voice-defs.el --- Define voices for voice-lock  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Voice Definitions for Emacspeak
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-09-01 15:30:13 -0700 (Sat, 01 Sep 2007) $ |
;;;  $Revision: 4672 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2018, T. V. Raman
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;{{{ Introduction

;;; Commentary:
;;; Contains just the voice definitions. Voices are defined using the
;;; facilities of module voice-setup.
;;; Code:

;;}}}
;;{{{ Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'voice-setup)

;;}}}
;;{{{ Voice Definitions: bold and friends

(defvoice  voice-punctuations-some (list nil nil nil nil  nil 'some)
  "Speaks some punctuations.")

(defvoice  voice-monotone (list nil nil 0 0 nil 'all)
  "Add monotone and  all punctuations.")

(defvoice  voice-monotone-medium (list nil nil 2 2  nil 'all)
  "Add medium monotone and  punctuations.")

(defvoice  voice-monotone-light (list nil nil 4 4   nil 'all)
  "Add light monotone and  punctuations.")

(defvoice voice-animate-extra (list nil 8 8 6)
  "Adds extra animation.")

(defvoice voice-animate (list nil 7 7 4)
  "Animates current voice.")

(defvoice voice-animate-medium (list nil 6 6  3)
  "Adds medium animation.")

(defvoice voice-smoothen-extra (list nil nil nil 4 5)
  "Extra smoothen.")

(defvoice voice-smoothen-medium (list nil nil nil 3 4)
  "Add medium smoothen.")

(defvoice voice-smoothen (list nil nil  2 2)
  "Smoothen current voice.")

(defvoice voice-brighten-medium (list nil nil nil 5 6)
  "Brighten  (medium).")

(defvoice voice-brighten (list nil nil nil 6 7)
  "Brighten.")

(defvoice voice-brighten-extra (list nil nil nil 7 8)
  "Extra brighten.")

(defvoice voice-bolden (list nil 3 6 6  6)
  "Bolden current voice.")

(defvoice voice-bolden-medium (list nil 2 6 7  7)
  "Add medium bolden.")

(defvoice voice-bolden-extra (list nil 1 6 7 8)
  "Extra bolden.")

(defvoice voice-lighten (list nil 6 6 2   nil)
  "Lighten current voice.")

(defvoice voice-lighten-medium (list nil 7 7 3  nil)
  "Add medium lightness.")

(defvoice voice-lighten-extra (list nil 9 8 7   nil)
  "Add extra lightness.")

(defvoice voice-bolden-and-animate (list nil 3 8 8 8)
  "Bolden and animate.")

;;}}}
;;{{{  indentation and annotation

(defvoice voice-indent (list nil nil 3 1 3)
  "Indent voice .")

(defvoice voice-annotate (list nil nil 4 0 4)
  "Annotation..")

;;}}}
;;{{{ voice overlays

;;; these are suitable to use as "overlay voices".
(defvoice voice-lock-overlay-0
  (list nil 8 nil nil nil nil)
  "Pitch to 8.")

(defvoice voice-lock-overlay-1
  (list nil nil 8 nil nil nil)
  "Pitch-range to 8.")

(defvoice voice-lock-overlay-2
  (list nil nil nil 8 nil nil)
  " Richness to 8.")

(defvoice voice-lock-overlay-3
  (list nil  nil nil nil 8 nil)
  "Smoothness to 8.")

;;}}}
;;{{{  Map some voice personalities:

(voice-setup-add-map
 '(
   (bold voice-bolden)
   (bold-italic voice-bolden-and-animate)
   (button voice-bolden-medium)
   (error voice-brighten)
   (file-name-shadow voice-smoothen-medium)
   (fixed-pitch voice-monotone)
   (font-lock-builtin-face voice-bolden)
   (font-lock-comment-delimiter-face voice-smoothen-medium)
   (font-lock-comment-face voice-monotone)
   (font-lock-constant-face voice-lighten)
   (font-lock-doc-face voice-monotone-medium)
   (font-lock-function-name-face voice-bolden-medium)
   (font-lock-keyword-face voice-animate-extra)
   (font-lock-negation-char-face voice-brighten-extra)
   (font-lock-preprocessor-face voice-monotone-medium)
   (font-lock-regexp-grouping-backslash voice-smoothen-extra)
   (font-lock-regexp-grouping-construct voice-smoothen)
   (font-lock-string-face voice-lighten-extra)
   (font-lock-type-face voice-smoothen)
   (font-lock-variable-name-face voice-animate)
   (font-lock-warning-face voice-bolden-and-animate)
   (fringe voice-monotone-medium)
   (help-argument-name voice-smoothen)
   (highlight voice-animate)
   (isearch voice-bolden)
   (isearch-fail voice-monotone)
   (italic voice-animate)
   (link voice-bolden-medium)
   (link-visited voice-bolden-extra)
   (match voice-lighten)
   (query-replace voice-bolden)
   (region voice-brighten)
   (secondary-selection voice-lighten-medium)
   (shadow voice-smoothen-medium)
   (shr-link voice-bolden)
   (success voice-brighten-extra)
   (underline voice-lighten-extra)
   (variable-pitch voice-animate-extra)
   (warning voice-brighten)
   ))

;;}}}
(provide 'voice-defs)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

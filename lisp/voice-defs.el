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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;{{{ Introduction

;;; Commentary:
;;; Contains just the voice definitions. Voices are defined using the
;;; macro @code{defvoice} from  module voice-setup.
;;; @subsection An Overview Of Voice Design
;;;
;;; Aural CSS defines 4 primary device-independent dimensions.
;;; Average-Pitch, Pitch-Range, Stress, and Richness. There are ten
;;; possible values along each dimension (0..9), giving a total of
;;; 10,000 possible settings.
;;;
;;; Engine-specific modules such as dectalk-voices and outloud-voices
;;; map these dimensions to device-specific parameters and are
;;; responsible for generating the final device-specific codes.
;;; @subsection Creating  Distinct Voices  Via Aural CSS
;;; Along each dimension, a setting of 5 is mapped to the default
;;; setting for the voice as implemented by a given engine. Values on
;;; either side of  5 produce opposing effects.
;;; This module  defines the following effects, which can be
;;; conceptualized as pairs.
;;; @enumerate  A
;;; @item bolden , lighten
;;; @item animate, monotone
;;; @item brighten, smoothen
;;; @end enumerate
;;; In addition, we define @code{bolden-and-animate} as an auditory
;;; analog of @code{bold-italic}.
;;; The two additional voices @code{indent} and @code{annotate}
;;; predate the above and are retained  as two @emph{softer} voices. 
;;; Finally, there are 4 @emph{overlay} voices, corresponding  to the
;;; 4 dimensions; these each set one of the dimensions to @code{8}.
;;; Thus, we have a total of 25 unique voices defined in this module.
;;; @subsection Things to note
;;; @itemize
;;; @item These voices are designed to be distinctive when used in  a given
;;; utterance.
;;; @item Non-goal ---  to be able to identify each distinct voice in
;;; isolation.
;;; @item Audio-formatting is designed to set apart different types of
;;; content so that when used in context, one can easily pick-out
;;; distinct parts of the utterance.
;;; @end itemize 
;;; Code:

;;}}}
;;{{{ Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'voice-setup)

;;}}}
;;{{{ Voice Definitions: 

(defvoice voice-animate '( nil nil 6 6 6))
(defvoice voice-animate-extra '( nil nil 8 8 8 ))
(defvoice voice-animate-medium '( nil nil 7 7 7))
(defvoice voice-annotate '( nil nil 4 0 4))
(defvoice voice-bolden '( nil 3 nil  6))
(defvoice voice-bolden-and-animate '( nil 3 8 8 8))
(defvoice voice-bolden-extra '( nil 1 nil 8))
(defvoice voice-bolden-medium '( nil 2 nil  7 ))
(defvoice voice-brighten '( nil nil nil 5 6))
(defvoice voice-brighten-extra '( nil nil nil 3 8))
(defvoice voice-brighten-medium '( nil nil nil 4 7))
(defvoice voice-indent '( nil nil 3 1 3))
(defvoice voice-lighten '( nil 6 nil 2))
(defvoice voice-lighten-extra '( nil 9 nil 4))
(defvoice voice-lighten-medium '( nil 7 nil 3))
(defvoice voice-monotone-extra '( nil nil 0 0))
(defvoice voice-monotone-medium '( nil nil 2 2))
(defvoice voice-monotone '( nil nil 4 4))
(defvoice voice-overlay-0 '( nil 8 nil nil nil nil))
(defvoice voice-overlay-1 '( nil nil 8 nil nil nil))
(defvoice voice-overlay-2 '( nil nil nil 8 nil nil))
(defvoice voice-overlay-3 '( nil nil nil nil 8 nil))
(defvoice voice-smoothen '( nil nil nil 4 6))
(defvoice voice-smoothen-extra '( nil nil nil 0 2))
(defvoice voice-smoothen-medium '( nil nil nil 2 4))

;;}}}
;;{{{  Map some voice personalities:

(voice-setup-add-map
 '(
   (bold voice-bolden)
   (bold-italic voice-bolden-and-animate)
   (button voice-bolden-medium)
   (error voice-brighten)
   (file-name-shadow voice-smoothen-medium)
   (fixed-pitch voice-monotone-extra)
   (font-lock-builtin-face voice-bolden-extra)
   (font-lock-comment-delimiter-face voice-smoothen-medium)
   (font-lock-comment-face voice-monotone-extra)
   (font-lock-constant-face voice-lighten)
   (font-lock-doc-face voice-monotone-extra)
   (font-lock-doc-markup-face voice-monotone)
   (font-lock-function-name-face voice-bolden)
   (font-lock-keyword-face voice-animate-extra)
   (font-lock-negation-char-face voice-brighten-extra)
   (font-lock-preprocessor-face voice-monotone-medium)
   (font-lock-regexp-grouping-backslash voice-smoothen-extra)
   (font-lock-regexp-grouping-construct voice-smoothen)
   (font-lock-string-face voice-lighten-extra)
   (font-lock-type-face voice-smoothen)
   (font-lock-variable-name-face voice-animate)
   (font-lock-warning-face voice-brighten)
   (fringe voice-monotone-extra)
   (help-argument-name voice-smoothen)
   (highlight voice-animate)
   (isearch voice-bolden)
   (isearch-fail voice-monotone-extra)
   (isearch-group-1 voice-lighten)
   (isearch-group-2 voice-smoothen)
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
   (variable-pitch voice-animate-medium)
   (variable-pitch-text  voice-animate-extra)
   (warning voice-brighten)
   ))

;;}}}
(provide 'voice-defs)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

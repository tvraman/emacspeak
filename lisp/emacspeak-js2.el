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
;;;Copyright (C) 1995 -- 2007, T. V. Raman
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
;;; JS2-mode http://js2-mode.gogolecode.com/svn/trunk
;;; is a new, powerful Emacs mode for working with JavaScript.
;;; This module speech-enables js2.

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Face To Voice Mappings:

;;}}}
;;{{{  map faces to voices:

(voice-setup-add-map
 '(
   (js2-warning-face voice-bolden-and-animate)
   (js2-error-face voice-bolden-extra)
   (js2-constant-face voice-lighten)
   (js2-comment-face voice-monotone)
   (js2-string-face voice-lighten-extra)
   (js2-regexp-face voice-lighten-medium)
   (js2-type-face voice-smoothen)
   (js2-builtin-face voice-bolden)
   (js2-keyword-face voice-animate-extra)
   (js2-function-name-face voice-bolden-medium)
   (js2-variable-name-face voice-bolden)
   (js2-jsdoc-tag-face voice-bolden-medium)
   (js2-jsdoc-type-face voice-smoothen-medium)
   (js2-jsdoc-value-face voice-lighten-medium)
   (js2-function-param-face voice-lighten-extra)
   (js2-instance-member-face voice-lighten-medium)
   (js2-private-member-face voice-lighten-extra)
   (js2-private-function-call-face voice-smoothen-extra)
   (js2-jsdoc-html-tag-name-face voice-bolden-medium)
   (js2-jsdoc-html-tag-delimiter-face voice-smoothen)
   ))

;;}}}

(provide 'emacspeak-js2)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

;;; emacspeak-markdown.el --- Speech-enable MARKDOWN-mode.el
;;; $Id: emacspeak-markdown.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable MARKDOWN An Emacs Interface to markdown
;;; Keywords: Emacspeak,  Audio Desktop markdown
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
;;; MERCHANTABILITY or FITNMARKDOWN FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; MARKDOWN ==  Light-weight markup.
;;; This module speech-enables markdown.el

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map faces to voices:
(voice-setup-add-map
 '(
   (markdown-italic-face  voice-animate)
   (markdown-bold-face voice-bolden)
   (markdown-header-rule-face voice-bolden-medium)
   (markdown-header-delimiter-face voice-lighten)
   (markdown-header-face voice-bolden)
   (markdown-header-face-1 voice-bolden-medium)
   (markdown-header-face-2 voice-bolden-and-animate)
   (markdown-header-face-3 voice-bolden-extra)
   (markdown-header-face-4 voice-smoothen)
   (markdown-header-face-5 voice-lighten-extra)
   (markdown-header-face-6 voice-monotone)
   (markdown-inline-code-face voice-monotone)
   (markdown-list-face voice-animate)
   (markdown-blockquote-face voice-lighten)
   (markdown-pre-face
   (markdown-language-keyword-face
   (markdown-link-face
   (markdown-missing-link-face
   (markdown-reference-face
   (markdown-footnote-face
   (markdown-url-face
   (markdown-link-title-face
   (markdown-line-break-face
   (markdown-comment-face
   (markdown-math-face
   (markdown-metadata-key-face
   (markdown-metadata-value-face
   ))
;;}}}
;;{{{ Advice Interactive Commands:

;;}}}
(provide 'emacspeak-markdown)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

;;; emacspeak-pydoc.el --- Speech-enable PYDOC  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable PYDOC An Emacs Interface to pydoc
;; Keywords: Emacspeak,  Audio Desktop pydoc
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
;; MERCHANTABILITY or FITNPYDOC FOR A PARTICULAR PURPOSE.  See the
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
;; PYDOC ==  Python Documentation Viewer

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map Faces->Voices

(voice-setup-add-map
 '(
   (pydoc-source-file-link-face voice-monotone-extra)
   (pydoc-package-link-face  voice-animate)
   (pydoc-class-name-link-face voice-bolden)
   (pydoc-superclass-name-link-face voice-bolden-extra)
   (pydoc-callable-name-face voice-animate)
   (pydoc-callable-param-face voice-annotate)
   (pydoc-envvars-face voice-monotone-extra)
   (pydoc-data-face voice-lighten-extra)
   (pydoc-string-face voice-lighten)
   (pydoc-button-face voice-bolden)
   (pydoc-sphinx-directive-face voice-monotone-extra)
   (pydoc-sphinx-param-name-face voice-monotone-extra)
   (pydoc-sphinx-param-type-face voice-monotone-extra)))

;;}}}
;;{{{ Advice Interactive Commands:

(defadvice pydoc (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'help)
    (emacspeak-speak-buffer)))

;;}}}
(provide 'emacspeak-pydoc)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}

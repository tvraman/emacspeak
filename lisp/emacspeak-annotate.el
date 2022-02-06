;;; emacspeak-annotate.el --- Speech-enable ANNOTATE  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable ANNOTATE An Emacs Interface to annotate
;;; Keywords: Emacspeak,  Audio Desktop annotate
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, 2019, T. V. Raman
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
;;; MERCHANTABILITY or FITNANNOTATE FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; ANNOTATE == annotate.el from melpa
;;; Speech-enable creation and navigation of annotations.
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'annotate "annotate" 'noerror)

;;}}}
;;{{{ Map Faces:

(voice-setup-add-map 
'(
(annotate-annotation voice-animate)
(annotate-annotation-secondary voice-monotone)
(annotate-highlight voice-smoothen)
(annotate-highlight-secondary voice-lighten)
(annotate-prefix voice-bolden)))

;;}}}
;;{{{ Interactive Commands:

'(
  annotate-annotate
annotate-clear-annotations
annotate-delete-annotation
annotate-export-annotations
annotate-goto-next-annotation
annotate-goto-previous-annotation
annotate-import-annotations
annotate-integrate-annotations
annotate-load-annotations
annotate-mode
annotate-save-annotations
annotate-show-annotation-summary
annotate-summary-of-file-from-current-pos
)


;;}}}
(provide 'emacspeak-annotate)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

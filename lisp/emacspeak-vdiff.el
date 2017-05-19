;;; emacspeak-vdiff.el --- Speech-enable VDIFF  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable VDIFF An Emacs Interface to vdiff
;;; Keywords: Emacspeak,  Audio Desktop vdiff
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
;;; MERCHANTABILITY or FITNVDIFF FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; VDIFF == 

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map Faces:

(let ((print-length 0)
      (faces (emacspeak-wizards-enumerate-unmapped-faces "^vdiff"))
      (start (point)))
  (insert "\n\n(voice-setup-add-map \n'(\n")
  (cl-loop for f in faces do 
           (insert (format "(%s)\n" f)))
  (insert "\n)\n)")
  (goto-char start)
  (backward-sexp)
  (kill-sexp)
  (goto-char (search-forward "("))
  (indent-pp-sexp))

;;}}}
;;{{{ Interactive Commands:

(let ((print-length nil)
      (start (point))
      (commands (emacspeak-wizards-enumerate-uncovered-commands "^vdiff")))
  (insert "'(\n")
  (cl-loop for c in commands do (insert (format "%s\n" c)))
  (insert ")\n")
  (goto-char start)
  (backward-sexp)
  (kill-sexp)
  (goto-char (search-forward "("))
  (indent-pp-sexp))

)

;;}}}
(provide 'emacspeak-vdiff)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

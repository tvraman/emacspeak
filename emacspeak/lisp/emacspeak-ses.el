;;; emacspeak-ses.el --- Speech-enable ses spread-sheet
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak front-end for SES 
;;; Keywords: Emacspeak, ses 
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision$ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1999 T. V. Raman <raman@cs.cornell.edu>
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;;{{{  Introduction:

;;; ses implements a simple spread sheet and is part of Emacs
;;; 21.4
;;; This module speech-enables ses

;;}}}
;;{{{ required modules

;;; Code:
(require 'emacspeak-preamble)

;;}}}
;;{{{ Accessors:
;;; these are defined as macros in ses.el 
;;; and are cloned here as defsubst forms for now.

(defsubst emacspeak-ses-get-cell (row col)
  "Return the cell structure that stores information about cell
  (ROW,COL)."
  (declare (special cells))
  (aref (aref cells row) col))

(defsubst emacspeak-ses-cell-symbol (row &optional col)
  "From a CELL or a pair (ROW,COL), get the symbol that names the local-variable holding its value.  (0,0) => A1."
  (aref (if col (emacspeak-ses-get-cell  row  col) row) 0))

(defsubst emacspeak-ses-cell-formula (row &optional col)
  "From a CELL or a pair (ROW,COL), get the function that computes its value."
  (aref  (if col (emacspeak-ses-get-cell  row  col) row) 1))

(defsubst emacspeak-ses-cell-value (row &optional col)
  "From a CELL or a pair (ROW,COL), get the current value for that cell."
  (symbol-value (emacspeak-ses-cell-symbol row col)))

(defsubst emacspeak-ses-sym-rowcol (sym)
  "From a cell-symbol SYM, gets the cons (row . col).  A1 => (0 . 0).  Result
is nil if SYM is not a symbol that names a cell."
  (and (symbolp  sym) (get  sym 'ses-cell)))
;;}}}

(provide 'emacspeak-ses)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}

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
;;;  $Revision: 24.0 $ |
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
(require 'emacspeak-redefine)

;;}}}
;;{{{ SES Accessors:

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
;;{{{ emacspeak ses accessors 

;;; these additional accessors are defined in terms of the
;;;earlier helpers by Emacspeak.
(defsubst emacspeak-ses-current-cell-symbol ()
  "Return symbol for current cell."
  (declare (special curcell))
  (or (get-text-property (point) 'intangible)
      curcell))

(defsubst emacspeak-ses-current-cell-value ()
  "Return current cell value."
  (emacspeak-ses-cell-value
   (car (emacspeak-ses-sym-rowcol (emacspeak-ses-current-cell-symbol)))
   (cdr (emacspeak-ses-sym-rowcol (emacspeak-ses-current-cell-symbol)))))

(defsubst emacspeak-ses-get-cell-value-by-name (cell-name)
  "Return current  value of cell specified by name."
  (emacspeak-ses-cell-value
   (car (emacspeak-ses-sym-rowcol cell-name))
   (cdr (emacspeak-ses-sym-rowcol cell-name))))

;;}}}
;;{{{ emacspeak ses summarizers 

(defun emacspeak-ses-summarize-cell (cell-name)
  "Summarize specified  cell."
  (interactive
   (list
    (read-minibuffer "Cell: ")))
  (message
   (format "%s: %s"
           cell-name
           (emacspeak-ses-get-cell-value-by-name cell-name))))

(defun emacspeak-ses-summarize-current-cell (&rest ignore)
  "Summarize current cell."
  (interactive)
  (emacspeak-ses-summarize-cell
   (emacspeak-ses-current-cell-symbol)))

;;}}}
;;{{{ advice internals

;;}}}
;;{{{ new navigation commands 

;;; ses uses intangible properties to enable cell navigation
;;; here we define navigation primitives that call built-ins and
;;then speak the right information.

(defun emacspeak-ses-forward-column-and-summarize ()
  "Move to next column and summarize."
  (interactive)
  (forward-char)
  (emacspeak-ses-summarize-current-cell))

(defun emacspeak-ses-backward-column-and-summarize ()
  "Move to previous column and summarize."
  (interactive)
  (forward-char -1)
  (emacspeak-ses-summarize-current-cell))

(defun emacspeak-ses-forward-row-and-summarize ()
  "Move to next row and summarize."
  (interactive)
  (next-line)
  (emacspeak-ses-summarize-current-cell))

(defun emacspeak-ses-backward-row-and-summarize ()
  "Move to previous row  and summarize."
  (interactive)
  (previous-line)
  (emacspeak-ses-summarize-current-cell))

;;}}}
;;{{{ advice interactive commands

(defun emacspeak-ses-setup ()
  "Setup SES for use with emacspeak."
  (declare (special ses-mode-map))
  (emacspeak-rebind 'emacspeak-forward-char
                    'emacspeak-ses-forward-column-and-summarize
                    ses-mode-map)
  (emacspeak-rebind 'emacspeak-backward-char
                    'emacspeak-ses-backward-column-and-summarize
                    ses-mode-map)
  (emacspeak-rebind 'next-line
                    'emacspeak-ses-forward-row-and-summarize
                    ses-mode-map)
  (emacspeak-rebind 'previous-line
                    'emacspeak-ses-backward-row-and-summarize
                    ses-mode-map)
  )
        
        
(defadvice ses-forward-or-insert (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-ses-summarize-current-cell)))

;;}}}
                                        ;(emacspeak-ses-setup)
(provide 'emacspeak-ses)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

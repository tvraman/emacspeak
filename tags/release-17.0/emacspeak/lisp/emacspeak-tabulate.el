;;; emacspeak-tabulate.el --- Interpret tabulated information as a table
;;; $Id$
;;; $Author$ 
;;; Description:  Utility to help emacspeak identify tabulated information
;;; Keywords: Emacspeak, Tabulated Data,  Visual layout gives structure
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
;;;Copyright (C) 1995 -- 2002, T. V. Raman 
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

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-speak)

;;{{{  Introduction:
;;; This module is a simple table recognizer.
;;; Can recognize the columns in tabulated output, e.g. ps, ls output

;;}}}
;;{{{  helper functions:

;;; An interval is a cons of start and end 
(defsubst ems-make-interval (start end ) (cons start end ))
(defsubst ems-interval-start (interval) (car interval ))
(defsubst ems-interval-end (interval) (cdr interval  ))
(defsubst ems-intersect-intervals (i1 i2)
  (let  ((i (cons (max (ems-interval-start i1)
                       (ems-interval-start i2))
                  (min (ems-interval-end i1)
                       (ems-interval-end i2 )))))
    (if (< (car i) (cdr i)) i nil )))

;;}}}
;;{{{  Identify the fields in a region 

(defun ems-tabulate-field-separators-in-this-line () 
  "Returns a list of intervals specifying the field separators on the line.
Fields are assumed to be delimited by whitespace. "
  (let ((positions nil )
        (end nil)
        (first nil)
        (last nil)
        (continue t))
    (save-excursion
      (end-of-line)
      (setq end (point ))
      (beginning-of-line)
      (save-restriction
        (narrow-to-region (point) end)
        (skip-syntax-forward " ")
        (while (and continue
                    (<= (point)  end ))
                                        ;skip field
          (unless (zerop (skip-syntax-forward "^ "))
            (setq first  (current-column  )))
                                        ;skip field separator 
          (unless (zerop (skip-syntax-forward " "))
            (setq last (current-column)))
                                        ;check if we found a field separator
          (cond
           ((and first
                 last
                 (< first last))
            (push (ems-make-interval  first last  ) positions))
           (t (setq continue nil)))
                                        ;reset fornext iteration
          (setq first nil
                last nil )))
      (nreverse  positions ))))

(defun ems-tabulate-field-separators-in-region (start end )
  "Return a list of column separators. "
  (when  (< end start )
    (let ((tmp end))
      (setq end start
            start tmp )))
  (save-restriction 
    (narrow-to-region start end )
    (save-excursion
      (goto-char start )
      (let  ((try nil)
             (first nil)
             (last nil)
             (interval nil)
             (new-guesses nil)
             (guesses (ems-tabulate-field-separators-in-this-line )))
        (while (and guesses
                    (< (point) end)
                    (not (= 1 (forward-line 1))))
          (setq try guesses)
          (while try
            (beginning-of-line )
            (goto-char (+ (point )  (ems-interval-start   (car try ))))
            (skip-syntax-forward "^ ")
            (setq first (current-column))
            (skip-syntax-forward " ")
            (setq last (current-column ))
            (setq interval
                  (ems-intersect-intervals (car try)
                                           (ems-make-interval first last )))
	    (when interval (push interval  new-guesses))
	    (pop try )
            (setq first nil
                  last nil
                  interval nil ))
          (end-of-line)
	  (setf guesses (nreverse new-guesses) 
		new-guesses nil))
        guesses ))))

(defsubst ems-tabulate-process-column (tl tr br bl mark-headers start)
  (let ((header ( buffer-substring  tl tr))
        (personality-table (emacspeak-possible-voices)))
    (emacspeak-voicify-rectangle
     tl br 
     (read (completing-read
	    (format "Personality for column %s from  %s through %s"
		    header (- tl start) (- tr start))
	    personality-table  nil t )))
    (and mark-headers
	 (emacspeak-put-text-property-on-rectangle
	  tl br
	  'field-name header ))))

;;;  White space contains a list of intervals giving position of inter
;;;  columnal space. All calculations are done in terms of buffer
;;;  position.
;;; Invariants: (= (- tl tr) (- bl br))
;;; tl = start for first column
;;; br = end for last column

(defun emacspeak-tabulate-region (start end  &optional mark-fields)
  "Voicifies the white-space of a table if one found.  Optional interactive prefix
arg mark-fields specifies if the header row information is used to mark fields
in the white-space."
  (interactive "r\nP")
  (let ((white-space   (ems-tabulate-field-separators-in-region start end ))
        (dtk-stop-immediately nil)
        (width nil)
        (tl nil)
        (tr nil)
        (br nil)
        (bl nil))
    (ems-modify-buffer-safely
     (progn
       (message   "Detected %s rows and  %s columns."
		  (count-lines start end)
		  (+ 1 (length white-space )))
       (sit-for 1.5)
       (save-excursion
         (goto-char end)
         (beginning-of-line)
         (setq bl  (point))
         (setq tl  start )
					;(goto-char tl )
         (setq width   (ems-interval-start (car white-space)))
         (setq tr (+ tl width)
               br (+ bl width))
         (ems-tabulate-process-column tl tr br bl mark-fields start)
         (while white-space
					;move to beginning of next column
           (goto-char (+ start (ems-interval-end (car white-space))))
           (setq tl (point))
					; width of space between columns 
           (setq width (- tl tr))
           (setq bl (+ br width))
           (setq white-space (cdr white-space))
					;Now detect right edges of this column 
           (cond
            (white-space
					;white-space holds column positions, not buffer positions
             (setq width (- (ems-interval-start (car white-space ))
                            (- tl start)))
             (setq tr (+ tl width)
                   br (+ bl width)))
            (t (goto-char start)
               (end-of-line)
               (setq tr (point)
                     br end)))
           (ems-tabulate-process-column tl tr br bl
					mark-fields start)))))))
           

;;}}}
;;{{{ Parse a region of tabular data

(defun ems-tabulate-parse-region (start end)
  "Parse  region as tabular data and return a vector of vectors"
  (let ((table nil)
        (col-start start)
        (col-end nil)
        (j 0)
        (left-edge nil)
        (row-vector nil)
        (white-space (ems-tabulate-field-separators-in-region start
                                                              end))
        (separators nil)
        (row-count (count-lines start end))
        (column-count nil)
        (element nil))
    (setq column-count (1+ (length white-space)))
    (setq table (make-vector row-count nil))
    (save-excursion
      (goto-char start)
      (loop for
            i from 0 to (1- row-count)
            do
            (setq row-vector (make-vector column-count nil))
            (setq separators white-space)
            (beginning-of-line)
            (setq col-start (point))
            (setq left-edge  col-start)
            (setq col-end
                  (+ left-edge (ems-interval-start (car separators))))
            (setq element (buffer-substring col-start col-end))
            (aset row-vector j element)
            (incf j)
            (while separators
              (setq col-start
                    (+ left-edge (ems-interval-end (car separators))))
              (setq separators (cdr separators))
              (setq col-end
                    (if separators
                        (+ left-edge (ems-interval-start (car separators)))
                      (progn (end-of-line) (point))))
              (setq element (buffer-substring col-start col-end))
              (aset row-vector j element)
              (incf j))
            (setq j 0)
            (aset table i row-vector)
            (forward-line 1)))
    table))

;;}}}
(provide 'emacspeak-tabulate)

;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

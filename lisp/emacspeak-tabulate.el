;;; emacspeak-tabulate.el --- Interpret tabulated information as a table  -*- lexical-binding: t; -*-
;; $Id$
;; $Author: tv.raman.tv $ 
;; Description:  Utility to help emacspeak identify tabulated information
;; Keywords: Emacspeak, Tabulated Data,  Visual layout gives structure
;;{{{  LCD Archive entry: 

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com 
;; A speech interface to Emacs |
;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
;;  $Revision: 4532 $ | 
;; Location undetermined
;; 

;;}}}
;;{{{  Copyright:
;; Copyright (C) 1995 -- 2021, T. V. Raman 
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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}


;;{{{  Introduction:
;;; Commentary:
;; This module is a simple table recognizer.
;; Can recognize the columns in tabulated output, e.g. ps, ls output
;;; Code:
;;}}}
;;{{{ requires
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{  helper functions:

;; An interval is a cons of start and end 
(defun ems-make-interval (start end) (cons start end))
(defun ems-interval-start (interval) (car interval))
(defun ems-interval-end (interval) (cdr interval))
(defun ems-intersect-intervals (i1 i2)
  (let  ((i (cons (max (ems-interval-start i1)
                       (ems-interval-start i2))
                  (min (ems-interval-end i1)
                       (ems-interval-end i2)))))
    (if (< (car i) (cdr i)) i nil)))

;;}}}
;;{{{  Identify the fields in a region 

(defun ems-tabulate-field-separators-in-this-line () 
  "Returns a list of intervals specifying the field separators on the line.
Fields are assumed to be delimited by whitespace. "
  (let ((positions nil)
        (end nil)
        (first nil)
        (last nil)
        (continue t))
    (save-excursion
      (end-of-line)
      (setq end (point))
      (beginning-of-line)
      (save-restriction
        (narrow-to-region (point) end)
        (skip-syntax-forward " ")
        (while (and continue
                    (<= (point)  end))
                                        ;skip field
          (unless (zerop (skip-syntax-forward "^ "))
            (setq first  (current-column)))
                                        ;skip field separator 
          (unless (zerop (skip-syntax-forward " "))
            (setq last (current-column)))
                                        ;check if we found a field separator
          (cond
           ((and first
                 last
                 (< first last))
            (push (ems-make-interval  first last) positions))
           (t (setq continue nil)))
                                        ;reset fornext iteration
          (setq first nil
                last nil)))
      (nreverse  positions))))

(defun ems-tabulate-field-separators-in-region (start end)
  "Return a list of column separators. "
  (when  (< end start)
    (let ((tmp end))
      (setq end start
            start tmp)))
  (save-restriction 
    (narrow-to-region start end)
    (save-excursion
      (goto-char start)
      (let  ((try nil)
             (first nil)
             (last nil)
             (interval nil)
             (new-guesses nil)
             (guesses (ems-tabulate-field-separators-in-this-line)))
        (while (and guesses
                    (< (point) end)
                    (not (= 1 (forward-line 1))))
          (setq try guesses)
          (while try
            (beginning-of-line)
            (goto-char (+ (point)  (ems-interval-start   (car try))))
            (skip-syntax-forward "^ ")
            (setq first (current-column))
            (skip-syntax-forward " ")
            (setq last (current-column))
            (setq interval
                  (ems-intersect-intervals (car try)
                                           (ems-make-interval first last)))
            (when interval (push interval  new-guesses))
            (pop try)
            (setq first nil
                  last nil
                  interval nil))
          (end-of-line)
          (setf guesses (nreverse new-guesses) 
                new-guesses nil))
        guesses))))

;;  White space contains a list of intervals giving position of inter
;;  columnal space. All calculations are done in terms of buffer
;;  position.
;; Invariants: (= (- tl tr) (- bl br))
;; tl = start for first column
;; br = end for last column



;;}}}
;;{{{ Parse a region of tabular data
;;;###autoload
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
      (cl-loop for
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
               (cl-incf j)
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
                 (cl-incf j))
               (setq j 0)
               (aset table i row-vector)
               (forward-line 1)))
    table))

;;}}}
(provide 'emacspeak-tabulate)

;;{{{  emacs local variables 

;; local variables:
;; folded-file: t
;; end: 

;;}}}

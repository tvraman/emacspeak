;;; emacspeak-table.el --- Implements data model for table browsing  -*- lexical-binding: t; -*-
;;
;; $Author: tv.raman.tv $ 
;; Description: Emacspeak table handling module
;; Keywords:emacspeak, audio interface to emacs tables are structured
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
;; Copyright (c) 1995 by T. V. Raman  
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

;;{{{  Introduction

;;; Commentary:
;; Implements a module that provides a high level interface to
;; tabulated information.
;;; Code:

;;}}}
;;{{{ requires

(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{  Define table data structure:

;; Tables will be represented internally as vectors.
;; User of a table can:
;; Ask to enumerate any row or column slice 
;; While enumerating a slice ask for row/column header information.
;; While enumerating a slice, ask for information about neighbors:
;; 

(cl-defstruct (emacspeak-table
               (:constructor cons-emacspeak-table))
  row-header                            ;pointer to column  0
  column-header                         ;pointer to row 0
  current-row                           ;row containing point 
  current-column                        ;column containing point 
  elements                              ;  vector of  elements 
  )

(defun emacspeak-table-rationalize-table (data)
  "Take a vector of vectors and return a `square'  table."
  (let ((n-cols (apply #'max (seq-map #'length data))))
    (cl-loop
     for row across data
     and i from 0
     unless (= (length row) n-cols)
     do
     (aset  data i 
            (vconcat row (make-vector (- n-cols (length row)) " ")))))
  data)

;;;###autoload
(defun emacspeak-table-make-table (elements)
  "Construct a table object from elements."
  (cl-assert (vectorp elements) t "Elements should be a vector of
vectors")
  (setq elements (emacspeak-table-rationalize-table elements))
  (let ((table (cons-emacspeak-table :elements elements))
        (row-h (make-vector (length  elements) nil))
        (index 0))
    (setf (emacspeak-table-column-header table) (aref elements 0)) ;first row
    (cl-loop
     for element across  elements do 
     (cl-assert (vectorp element) t "Row %s is not a vector" index)
     (aset row-h index (aref element 0)) ; build column 0
     (cl-incf index))
    (setf (emacspeak-table-row-header table) row-h)
    (setf (emacspeak-table-current-row table) 0)
    (setf (emacspeak-table-current-column table) 0)
    table))

;;}}}
;;{{{ Accessors

(defun emacspeak-table-this-element (table row column)
  (let ((elements (emacspeak-table-elements  table)))
    (aref (aref elements row) column)))

(defun emacspeak-table-current-element (table)
  (emacspeak-table-this-element table 
                                (emacspeak-table-current-row table)
                                (emacspeak-table-current-column table)))

(defun emacspeak-table-this-row (table index)
  (aref  (emacspeak-table-elements table) index))

(defun emacspeak-table-this-column (table column)
  (let*
      ((elements (emacspeak-table-elements table))
       (result (make-vector (length elements) nil))
       (index 0))
    (cl-loop
     for row across elements do
     (aset result index (aref row column))
     (cl-incf index))
    result))

(defun emacspeak-table-num-rows (table)
  (length (emacspeak-table-row-header table)))

(defun emacspeak-table-num-columns (table)
  (length (emacspeak-table-column-header table)))

(defun emacspeak-table-column-header-element (table column)
  (aref (emacspeak-table-column-header table) column))

(defun emacspeak-table-row-header-element (table row)
  (aref (emacspeak-table-row-header table) row))

;;}}}
;;{{{  enumerators

(defun emacspeak-table-enumerate-rows (table callback &rest callback-args)
  "Enumerates the rows of a table.
Calls callback once per row."
  (cl-loop
   for row across (emacspeak-table-elements table)
   collect
   (apply callback row callback-args)))

(defun emacspeak-table-enumerate-columns (table callback &rest callback-args)
  "Enumerate columns of a table.
Calls callback once per column."
  (let ((elements (emacspeak-table-elements table)))
    (cl-loop
     for column   from 0 to (1- (length   elements))
     collect
     (apply callback
            (emacspeak-table-this-column table column)
            callback-args))))

;;}}}
;;{{{ finders 

(defun emacspeak-table-find-match-in-row (table index pattern
                                                &optional predicate)
  "Look for next element matching pattern in  row."
  (or predicate
      (setq predicate 'equal))
  (let ((next(%  (1+  (emacspeak-table-current-column table))
                 (emacspeak-table-num-columns  table)))
        (count   (emacspeak-table-num-columns table))
        (found nil))
    (cl-loop
     for   i from 0   to count
     and column = next then (% (cl-incf column) count)
     if
     (funcall predicate  pattern
              (emacspeak-table-this-element table  index column))
     do
     (setq found t)
     until found
     finally return (and found column))))

(defun emacspeak-table-find-match-in-column (table index pattern
                                                   &optional predicate)
  "Look for element matching pattern in  column."
  (or predicate
      (setq predicate 'equal))
  (let ((next(%  (1+  (emacspeak-table-current-row table))
                 (emacspeak-table-num-rows table)))
        (count   (emacspeak-table-num-rows table))
        (found nil))
    (cl-loop for   i from 0   to count
             and row = next then (% (cl-incf row) count)
             if  (funcall predicate  pattern
                          (emacspeak-table-this-element table  row index))
             do (setq found t)
             until found
             finally return (and found row))))

;;}}}
;;{{{  Moving point:

(defun emacspeak-table-goto-cell (table row column)
  "Move to a cell of the table"
  (let  ((row-count (emacspeak-table-num-rows table))
         (column-count (emacspeak-table-num-columns table)))
    (cond
     ((or (<= 0 row)
          (>= row row-count)
          (<= 0 column)
          (>= column column-count))
      (setf (emacspeak-table-current-row table) row)
      (setf (emacspeak-table-current-column table) column))
     (t (error "Current table has %s rows and %s columns"
               row-count column-count)))))

(defun emacspeak-table-move-up (table &optional count)
  "Move up in the table if possible."
  (setq count (or count 1))
  (let* ((current (emacspeak-table-current-row table))
         (new (- current count)))
    (cond
     ((<= 0 new)
      (setf (emacspeak-table-current-row table) new))
     (t (message "Cannot move up by %s rows from row %s" count
                 current)
        (emacspeak-auditory-icon 'warn-user)))))

(defun emacspeak-table-move-down (table &optional count)
  "Move down in the table if possible."
  (setq count (or count 1))
  (let* ((current (emacspeak-table-current-row table))
         (row-count (emacspeak-table-num-rows table))
         (new (+ current count)))
    (cond
     ((< new  row-count)
      (setf (emacspeak-table-current-row table) new))
     (t (message "Cannot move down by %s rows from row %s"
                 count current)
        (emacspeak-auditory-icon 'warn-user)))))

(defun emacspeak-table-move-left (table &optional count)
  "Move left in the table if possible."
  (setq count (or count 1))
  (let* ((current (emacspeak-table-current-column table))
         (new (- current count)))
    (cond
     ((<= 0 new)
      (setf (emacspeak-table-current-column table) new))
     (t (message "Cannot move left by %s columns from column %s"
                 count current)
        (emacspeak-auditory-icon 'warn-user)))))

(defun emacspeak-table-move-right (table &optional count)
  "Move right in the table if possible."
  (setq count (or count 1))
  (let* ((current (emacspeak-table-current-column table))
         (column-count (emacspeak-table-num-columns table))
         (new (+ current count)))
    (cond
     ((< new  column-count)
      (setf (emacspeak-table-current-column table) new))
     (t (message "Cannot move right by %s columns from column %s"
                 count current)
        (emacspeak-auditory-icon 'warn-user)))))

;;}}}

(provide  'emacspeak-table)
;;{{{  emacs local variables 

;; local variables:
;; folded-file: t
;; end: 

;;}}}

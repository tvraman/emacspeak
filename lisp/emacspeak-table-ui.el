;;; emacspeak-table-ui.el --- Table Navigation UI  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak Table Navigation UI
;;; Keywords: Emacspeak, Table UI ,  Visual layout gives structure
;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2008-06-26 15:46:49 -0700 (Thu, 26 Jun 2008) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2021, T. V. Raman
;;; Copyright (c) 1995 by T. V. Raman
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}

;;{{{  Introduction

;;; Commentary:
;;; User interface to tables
;;; Code:

;;}}}
;;{{{ requires
(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(eval-when-compile
  (require 'derived))
(require 'emacspeak-preamble)
(require 'emacspeak-table)

;;}}}
;;{{{  emacspeak table mode

;;; emacspeak-table-submap makes these available globally.
;;; Forward declaration

(define-derived-mode  emacspeak-table-mode  special-mode
  "Table Navigation On The Emacspeak Audio Desktop"
  "Major mode for browsing tables.
Table mode is designed to allow speech users to browse tabular
data with full contextual feedback while retaining all the power
of the two-dimensional spatial layout of tables.

In table mode, the arrow keys move between cells of the table.
Emacspeak speaks the cell contents in a user-customizable way.  The
visual display is kept in sync with the speech you hear; however
Emacspeak is examining the entire table in order to speak the current
cell content intelligently.

You can interactively specify that emacspeak should speak either the row or
column header (or both) while speaking each cell.  You can also specify a row
or column filter that should be applied when speaking entire rows or columns
--this lets you view slices of a table.  You can move to a specific row or
column by searching the cell contents or by searching the row or column
headers to locate items of interest.

Here is a short description of the special commands provided in this mode.

The next four commands help you move to the edges of the table:

E               emacspeak-table-goto-right
A               emacspeak-table-goto-left
B               emacspeak-table-goto-bottom
T               emacspeak-table-goto-top

The next two commands let you search the table.
The commands ask you if you want to search rows or columns.
When searching headers remember that row 0 is the column header,
and that column 0 is the row header.

h               emacspeak-table-search-headers
s               emacspeak-table-search

The next command lets you specify how cell contents should be spoken.  Specify
one of: `b' for both, `c' for column, `r' for row, `f' for row filtering and
`g' for column filtering. --table cells with then be spoken with both (or
either)row and column headers, or with the filter applied.

a               emacspeak-table-select-automatic-speaking-method

The next set of commands speak the current table cell:

.               emacspeak-table-speak-coordinates
b               emacspeak-table-speak-both-headers-and-element
SPC             emacspeak-table-speak-current-element
c               emacspeak-table-speak-column-header-and-element
r               e macspeak-table-speak-row-header-and-element

The next set of commands navigate the table:

right               emacspeak-table-next-column
left               emacspeak-table-previous-column
down               emacspeak-table-next-row
up               emacspeak-table-previous-row
j               emacspeak-table-goto
S-tab               emacspeak-table-previous-column
TAB               emacspeak-table-next-column

Row and Column Filtering

Filtering is designed to let you view slices of a table.
They are specified as lists of numbers and strings.
The concept is best explained with an example.

A row filter specifies which of the entries in the current row should be
spoken.Entries are numbered starting with 0.  Thus, when working with a table
having 8 columns, a row filter of (1 2 3) will speak only entries 1 2 and 3.
Use the sample tables in etc/tables   to familiarize yourself with this
feature. Note that you can intersperse meaningful strings in the list that
specifies the filter.

Full List Of Keybindings:
\\{emacspeak-table-mode-map}"
  (set (make-local-variable 'voice-lock-mode) t)
  (put-text-property (point-min) (point-max)
                     'point-entered 'emacspeak-table-point-motion-hook)
  (set-buffer-modified-p nil)
  (setq buffer-undo-list t)
  (setq buffer-read-only t)
  (emacspeak-auditory-icon 'select-object)
  (emacspeak-speak-mode-line))

(cl-loop
 for binding in
 '(
   ("M-l" emacspeak-table-ui-filter-load)
   ("M-s" emacspeak-table-ui-filter-save)
   ("S-<tab>" emacspeak-table-previous-column)
   ("#" emacspeak-table-sort-on-current-column)
   ("." emacspeak-table-speak-coordinates)
   ("," emacspeak-table-find-csv-file)
   ("v" emacspeak-table-view-csv-buffer)
   ("<down>" emacspeak-table-next-row)
   ("<left>" emacspeak-table-previous-column)
   ("<right>" emacspeak-table-next-column)
   ("<up>"  emacspeak-table-previous-row)
   ("=" emacspeak-table-speak-dimensions)
   ("<" emacspeak-table-goto-left)
   (">" emacspeak-table-goto-right)
   ("M-<" emacspeak-table-goto-top)
   ("M->" emacspeak-table-goto-bottom)
   ("A" emacspeak-table-goto-left)
   ("B" emacspeak-table-goto-bottom)
   ("C" emacspeak-table-search-column)
   ("C-b" emacspeak-table-previous-column)
   ("C-f" emacspeak-table-next-column)
   ("C-n" emacspeak-table-next-row)
   ("C-p" emacspeak-table-previous-row)
   ("E" emacspeak-table-goto-right)
   ("R" emacspeak-table-search-row)
   ("SPC" emacspeak-table-speak-current-element)
   ("T" emacspeak-table-goto-top)
   ("TAB" emacspeak-table-next-column)
   ("a" emacspeak-table-select-automatic-speaking-method)
   ("b" emacspeak-table-speak-both-headers-and-element)
   ("c" emacspeak-table-speak-column-header-and-element)
   ("f" emacspeak-table-speak-row-filtered)
   ("g" emacspeak-table-speak-column-filtered)
   ("h" emacspeak-table-search-headers)
   ("j" emacspeak-table-goto)
   ("k" emacspeak-table-copy-to-clipboard)
   ("n" emacspeak-table-next-row)
   ("p" emacspeak-table-previous-row)
   ("q" quit-window)
   ("Q" emacspeak-kill-buffer-quietly)
   ("r" emacspeak-table-speak-row-header-and-element)
   ("s" emacspeak-table-search)
   ("w" emacspeak-table-copy-current-element-to-kill-ring)
   ("x" emacspeak-table-copy-current-element-to-register)
   )
 do
 (emacspeak-keymap-update emacspeak-table-mode-map binding)
 (emacspeak-keymap-update emacspeak-table-submap binding))

;;}}}
;;{{{  speaking current entry

(defun emacspeak-table-synchronize-display ()
  "Bring visual display in sync with internal representation"
  (cl-declare (special emacspeak-table ems--positions))
  (let ((row (emacspeak-table-current-row emacspeak-table))
        (column (emacspeak-table-current-column emacspeak-table))
        (width (frame-width)))
    (goto-char
     (or
      (gethash
       (intern
        (format "element:%s:%s" row column))
       ems--positions)
      (point)))
    (scroll-left (- (current-column)
                    (+ (/ width  2)
                       (window-hscroll))))))

(defun  emacspeak-table-speak-coordinates ()
  "Speak current table coordinates."
  (interactive)
  (cl-declare (special emacspeak-table))
  (cl-assert  (boundp 'emacspeak-table) nil "No table here")
  (message "Row %s Column %s"
           (emacspeak-table-current-row emacspeak-table)
           (emacspeak-table-current-column emacspeak-table)))

(defun  emacspeak-table-speak-dimensions ()
  "Speak current table dimensions."
  (interactive)
  (cl-declare (special emacspeak-table))
  (cl-assert  (boundp 'emacspeak-table) nil "No table here")
  (message "%s by %s table"
           (emacspeak-table-num-rows emacspeak-table)
           (emacspeak-table-num-columns emacspeak-table)))

(defun emacspeak-table-speak-current-element ()
  "Speak current table element"
  (interactive)
  (cl-declare (special emacspeak-table))
  (cl-assert  (boundp 'emacspeak-table) nil "No table here")
  (dtk-speak-and-echo
   (format "%s" (emacspeak-table-current-element emacspeak-table))))

(defun emacspeak-table-speak-row-header-and-element ()
  "Speak  row header and table element"
  (interactive)
  (cl-declare (special emacspeak-table))
  (cl-assert  (boundp 'emacspeak-table) nil "No table here")
  (let ((element (emacspeak-table-current-element emacspeak-table))
        (head
         (format
          "%s"
          (emacspeak-table-row-header-element
           emacspeak-table
           (emacspeak-table-current-row emacspeak-table)))))
    (put-text-property 0 (length head) 'face 'italic head)
    (dtk-speak
     (concat head
             (format " %s" element)))))

(defun emacspeak-table-speak-column-header-and-element ()
  "Speak  column header and table element"
  (interactive)
  (cl-declare (special emacspeak-table))
  (cl-assert  (boundp 'emacspeak-table) nil "No table here")
  (let ((head
         (format
          "%s"
          (emacspeak-table-column-header-element
           emacspeak-table
           (emacspeak-table-current-column emacspeak-table)))))
    (put-text-property 0 (length head) 'face 'italic head)
    (dtk-speak-and-echo
     (concat
      head
      (format " %s" (emacspeak-table-current-element emacspeak-table))))))

(defun emacspeak-table-speak-both-headers-and-element ()
  "Speak  both row and column header and table element"
  (interactive)
  (cl-declare (special emacspeak-table))
  (cl-assert  (boundp 'emacspeak-table) nil "No table here")
  (let ((element (emacspeak-table-current-element emacspeak-table))
        (col-head
         (format
          "%s"
          (emacspeak-table-column-header-element
           emacspeak-table
           (emacspeak-table-current-column emacspeak-table))))
        (row-head
         (format
          "%s"
          (emacspeak-table-row-header-element
           emacspeak-table
           (emacspeak-table-current-row emacspeak-table)))))
    (put-text-property
     0 (length row-head) 'face 'italic row-head)
    (put-text-property
     0 (length col-head) 'face 'bold col-head)
    (dtk-speak
     (concat row-head " " col-head
             (format " %s" element)))))

(defun emacspeak-table-get-entry-with-headers  (row column &optional row-head-p col-head-p)
  "Return table element. Optional args specify  if we return any headers."
  (cl-declare (special emacspeak-table))
  (cl-assert  (boundp 'emacspeak-table) nil "No table here")
  (let ((col-head nil)
        (row-head nil))
    (when row-head-p
      (setq row-head
            (format "%s"
                    (emacspeak-table-row-header-element emacspeak-table row)))
      (put-text-property 0 (length row-head)
                         'face 'italic row-head))
    (when  col-head-p
      (setq col-head
            (format
             "%s"
             (emacspeak-table-column-header-element emacspeak-table column)))
      (put-text-property
       0 (length col-head)
       'face 'bold col-head))
    (concat
     row-head " " col-head " "
     (format " %s" (emacspeak-table-this-element emacspeak-table row column)))))

(defvar emacspeak-table-speak-row-filter nil
  "Template specifying how a row is filtered before it is spoken.")

(make-variable-buffer-local 'emacspeak-table-speak-row-filter)

(defun emacspeak-table-handle-row-filter-token  (token)
  "Handle a single token in an Emacspeak table row/column formatter."
  (let ((value nil))
    (cond
     ((stringp token) (format "%s" token))
     ((numberp token)
      (setq value
            (emacspeak-table-get-entry-with-headers
             (emacspeak-table-current-row emacspeak-table) token))
      (put-text-property
       0 (length value)
       'face 'bold  value)
      value)
     ((and (listp token) (numberp (cl-first token)) (numberp (cl-second token)))
      (setq value
            (emacspeak-table-get-entry-with-headers
             (cl-first token) (cl-second token)))
      (put-text-property 0 (length value) 'face 'bold value)
      value)
     ((and (symbolp (cl-first token)) (fboundp  (cl-first token)))
;;; applying a function:
      (setq value
            (funcall
             (cl-first token) ;;; get args
             (cond
              ((and
                (= 2 (length token)) (numberp (cl-second token)))
               (emacspeak-table-get-entry-with-headers
                (emacspeak-table-current-row emacspeak-table)
                (cl-second token)))
              ((and
                (= 3 (length token))
                (numberp (cl-second token))
                (numberp (cl-third token)))
               (emacspeak-table-get-entry-with-headers
                (cl-second token) (cl-third token))))))
      (put-text-property 0 (length value) 'face 'bold  value)
      value)
     (t  (format "%s" token)))))

(defun emacspeak-table-speak-row-filtered  (&optional prefix)
  "Speaks a table row after applying a specified row filter.
Optional prefix arg prompts for a new filter."
  (interactive "P")
  (cl-declare (special emacspeak-table-speak-row-filter emacspeak-table))
  (and emacspeak-table-speak-row-filter(push emacspeak-table-speak-row-filter minibuffer-default))
  (unless (and  emacspeak-table-speak-row-filter
                (listp emacspeak-table-speak-row-filter)
                (not prefix))
    (setq emacspeak-table-speak-row-filter
          (read-minibuffer
           "Specify row filter as a list: "
           (format
            "%s"
            (or
             (emacspeak-table-ui-filter-get (emacspeak-table-ui-generate-key))
             "("))))
    (emacspeak-table-ui-filter-set
     (emacspeak-table-ui-generate-key)
     emacspeak-table-speak-row-filter))
  (dtk-speak-and-echo
   (mapconcat
    #'emacspeak-table-handle-row-filter-token
    emacspeak-table-speak-row-filter
    " ")))

(defvar emacspeak-table-speak-column-filter nil
  "Template specifying how a column is filtered before it is spoken.")

(make-variable-buffer-local 'emacspeak-table-speak-column-filter)

(defun emacspeak-table-handle-column-filter-token (token)
  "Handle token from column filter."
  (let ((value nil))
    (cond
     ((stringp token) token)
     ((numberp token)
      (emacspeak-table-get-entry-with-headers
       token
       (emacspeak-table-current-column emacspeak-table)))
     ((and (listp token)
           (numberp (cl-first token))
           (numberp (cl-second token)))
      (emacspeak-table-get-entry-with-headers (cl-first token) (cl-second token)))
     ((and (symbolp (cl-first token)) (fboundp  (cl-first token)))
;;; applying a function:
      (setq value
            (funcall
             (cl-first token) ;;; get args
             (cond
              ((and
                (= 2 (length token)) (numberp (cl-second token)))
               (emacspeak-table-get-entry-with-headers
                (cl-second token)
                (emacspeak-table-current-column emacspeak-table)))
              ((and
                (= 3 (length token))
                (numberp (cl-second token))
                (numberp (cl-third token)))
               (emacspeak-table-get-entry-with-headers
                (cl-second token) (cl-third token))))))
      (put-text-property 0 (length value) 'face 'bold  value)
      value)
     (t  (format "%s" token)))))
(defun emacspeak-table-speak-column-filtered  (&optional prefix)
  "Speaks a table column after applying a specified column filter.
Optional prefix arg prompts for a new filter."
  (interactive "P")
  (cl-declare (special emacspeak-table-speak-column-filter
                       emacspeak-table))
  (unless (and  emacspeak-table-speak-column-filter
                (listp emacspeak-table-speak-column-filter)
                (not prefix))
    (setq emacspeak-table-speak-column-filter
          (read-minibuffer "Specify column filter as a list: " "(")))
  (dtk-speak-and-echo
   (mapconcat
    #'emacspeak-table-handle-column-filter-token
    emacspeak-table-speak-column-filter
    " ")))

;;}}}
;;{{{  what to do when point moves

(defun emacspeak-table-point-motion-hook (old new)
  "Bring internal representation in sync with visual display"
  (cl-declare (special emacspeak-table))
  (condition-case nil
      (emacspeak-table-goto-cell
       emacspeak-table
       (get-text-property new 'row)
       (get-text-property new 'column))
    (error nil))
  (push-mark old t))

;;}}}
;;{{{  opening a file of table data

;;{{{ csv helpers:

(defun ems-csv-forward-field ()
  "Skip forward over one field."
  (if (and (following-char) (eq (following-char) ?\"))
      (forward-sexp)
    (skip-chars-forward "^,\n")))

(defun ems-csv-backward-field ()
  "Skip backward over one field."
  (if (eq (preceding-char) ?\")
      (backward-sexp)
    (skip-chars-backward "^,\n")))

;;}}}
;;;###autoload
(defun emacspeak-table-prepare-table-buffer (table buffer)
  "Prepare tabular data."
  (cl-declare (special emacspeak-table ems--positions))
  (with-current-buffer buffer
    (emacspeak-table-mode)
    (let ((i 0)
          (j 0)
          (count 0)
          (row-start 1)
          (column-start 1)
          (inhibit-read-only t))
      (setq truncate-lines t)
      (setq buffer-undo-list t)
      (erase-buffer)
      (set (make-local-variable 'emacspeak-table) table)
      (set (make-local-variable 'ems--positions) (make-hash-table))
      (setq count (1-  (emacspeak-table-num-columns table)))
      (cl-loop
       for row across (emacspeak-table-elements table) do
       (cl-loop
        for _element across row do
        (puthash
         (intern (format "element:%s:%s" i j))  ; compute key
         (point) ; insertion point  is the value
         ems--positions)
        (insert
         (format "%s%s"
                 (emacspeak-table-this-element table i j)
                 (if (=  j count)
                     "\n"
                   "\t")))
        (put-text-property column-start (point)
                           'column j)
        (setq column-start (point))
        (cl-incf j))
       (setq j 0)
       (put-text-property row-start (point) 'row i)
       (setq row-start (point))
       (cl-incf i))))
  (switch-to-buffer buffer)
  (emacspeak-table-goto-cell emacspeak-table 0 0)
  (setq truncate-lines t)
  (message "Use Emacspeak Table UI to browse this table."))

;;;###autoload
(defun emacspeak-table-find-file (filename)
  "Open a file containing table data and display it in table mode.
emacspeak table mode is designed to let you browse tabular data using
all the power of the two-dimensional spatial layout while giving you
sufficient contextual information.  The etc/tables subdirectory of the
emacspeak distribution contains some sample tables --these are the
CalTrain schedules.  Execute command `describe-mode' bound to
\\[describe-mode] in a buffer that is in emacspeak table mode to read
the documentation on the table browser."
  (interactive "FEnter filename containing table data: ")
  (cl-declare (special ems--positions))
  (let ((buffer (get-buffer-create (format  "*%s*"
                                            (file-name-nondirectory filename))))
        (data nil)
        (table nil))
    (setq data (find-file-noselect filename))
    (setq table (emacspeak-table-make-table (read data)))
    (kill-buffer data)
    (emacspeak-table-prepare-table-buffer table buffer)))

(defun ems-csv-get-fields ()
  "Return list of fields on this line."
  (let ((fields nil)
        (this-field nil)
        (start (line-beginning-position)))
    (goto-char start)
    (while (not (eolp))
      (ems-csv-forward-field)
      (setq this-field
            (cond
             ((= (preceding-char) ?\")
              (buffer-substring-no-properties (1+ start)
                                              (1- (point))))
             (t (buffer-substring-no-properties start  (point)))))
      (push this-field fields)
      (when(and (char-after) (= (char-after) ?,))
        (forward-char 1))
      (setq start (point)))
    (when (= (preceding-char) ?,)
      (push "" fields))
    (nreverse fields)))

;;;###autoload
(defun emacspeak-table-find-csv-file (filename)
  "Process a csv (comma separated values) file.
The processed  data is presented using emacspeak table navigation. "
  (interactive "FFind CSV file: ")
  (let  ((buffer (find-file-noselect filename)))
    (emacspeak-table-view-csv-buffer buffer)
    (kill-buffer buffer)))

;;;###autoload
(defun emacspeak-table-view-csv-buffer (&optional buffer-name)
  "Process a csv (comma separated values) data.
The processed  data is  presented using emacspeak table navigation. "
  (interactive)
  (or buffer-name
      (setq buffer-name (current-buffer)))
  (let ((scratch (get-buffer-create "*csv-scratch*"))
        (table nil)
        (elements nil)
        (fields nil)
        (buffer (get-buffer-create
                 (format "*%s-table*" buffer-name))))
    (save-current-buffer
      (set-buffer scratch)
      (setq buffer-undo-list t)
      (erase-buffer)
      (insert-buffer-substring buffer-name)
      (goto-char (point-min))
      (flush-lines "^ *$")
      (goto-char (point-min))
      (setq elements
            (make-vector (count-lines (point-min) (point-max))
                         nil))
      (cl-loop for i from 0 to (1- (length elements))
               do
               (setq fields (ems-csv-get-fields))
               (aset elements i (apply 'vector fields))
               (forward-line 1))
      (setq table (emacspeak-table-make-table elements))
      )
    (kill-buffer scratch)
    (emacspeak-table-prepare-table-buffer table buffer)
    (emacspeak-auditory-icon 'open-object)))

(defun emacspeak-table-render-csv-url  (_status result-buffer)
  "Render the result of asynchronously retrieving CSV data from url."
  (let ((inhibit-read-only t)
        (data-buffer (current-buffer))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (with-current-buffer data-buffer
      (goto-char (point-min))
      (search-forward "\n\n")
      (delete-region (point-min) (point))
      (decode-coding-region (point-min) (point-max) 'utf-8)
      (emacspeak-table-view-csv-buffer)
      (rename-buffer result-buffer 'unique)
      (emacspeak-speak-mode-line)
      (emacspeak-auditory-icon 'open-object))))

;;;###autoload
(defun emacspeak-table-view-csv-url  (url &optional buffer-name)
  "Process a csv (comma separated values) data at  `URL'.
The processed  data is  presented using emacspeak table navigation. "
  (interactive "sURL:\nP")
  (unless (or buffer-name (stringp buffer-name))
    (setq buffer-name "CSV Data Table"))
  (cl-declare (special g-curl-program g-curl-common-options))
  (url-retrieve url #'emacspeak-table-render-csv-url  (list buffer-name)))

;;}}}
;;{{{ Processing a region of tabular data
;;;###autoload
(defun emacspeak-table-display-table-in-region (start end)
  "Recognize tabular data in current region and display it in table
browsing mode in a a separate buffer.
emacspeak table mode is designed to let you browse tabular data using
all the power of the two-dimensional spatial layout while giving you
sufficient contextual information.  The tables subdirectory of the
emacspeak distribution contains some sample tables --these are the
CalTrain schedules.  Execute command `describe-mode' bound to
\\[describe-mode] in a buffer that is in emacspeak table mode to read
the documentation on the table browser."
  (interactive "r")
  (cl-declare (special emacspeak-table ems--positions))
  (let ((buffer-undo-list t)
        (workspace (get-buffer-create " table workspace  "))
        (buffer (get-buffer-create
                 (format  "table-%s"
                          (or (buffer-name)
                              "scratch"))))
        (table nil)
        (i 0)
        (j 0)
        (count 0)
        (row-start 1)
        (column-start 1)
        (text (buffer-substring start end)))
    (save-current-buffer
      (when (= 10 (string-to-char (substring text -1)))
        (setq text (substring text 0 -1)))
      (set-buffer workspace)
      (erase-buffer)
      (insert text)
      (goto-char (point-min))
      (flush-lines "^ *$")
      (setq table (emacspeak-table-make-table
                   (ems-tabulate-parse-region
                    (point-min) (point-max)))))
    (kill-buffer workspace)
    (save-current-buffer
      (set-buffer buffer)
      (emacspeak-table-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (set (make-local-variable 'emacspeak-table) table)
        (set (make-local-variable 'ems--positions) (make-hash-table))
        (setq count (1-  (emacspeak-table-num-columns table)))
        (cl-loop for row across (emacspeak-table-elements table)
                 do
                 (cl-loop for _element across row
                          do
                          (setf
                           (gethash
                            (intern (format "element:%s:%s" i j))
                            ems--positions)
                           (point))
                          (insert
                           (format "%s%s"
                                   (emacspeak-table-this-element table i j)
                                   (if (=  j count)
                                       "\n"
                                     "\t")))
                          (put-text-property column-start (point)
                                             'column j)
                          (setq column-start (point))
                          (cl-incf j))
                 (setq j 0)
                 (put-text-property row-start (point) 'row i)
                 (setq row-start (point))
                 (cl-incf i))
        (goto-char (point-min))))
    (switch-to-buffer buffer)
    (rename-buffer
     (format "%sX%s-%s"
             (emacspeak-table-num-rows emacspeak-table)
             (emacspeak-table-num-columns emacspeak-table)
             (buffer-name buffer)))))

;;}}}
;;{{{ select default speaking action

(defvar emacspeak-table-select-automatic-speaking-method-prompt
  "Select: b both c column d default r row f filter row g filter column "
  "Prompt to display when selecting automatic speaking method for
table elements")

(defun emacspeak-table-select-automatic-speaking-method ()
  "Interactively select the kind of automatic speech to produce when
browsing table elements"
  (interactive)
  (cl-declare (special emacspeak-table-speak-element))
  (message emacspeak-table-select-automatic-speaking-method-prompt)
  (let ((key (read-char)))
    (setq emacspeak-table-speak-element
          (cl-case  key
            (?b 'emacspeak-table-speak-both-headers-and-element)
            (?c 'emacspeak-table-speak-column-header-and-element)
            (?r 'emacspeak-table-speak-row-header-and-element)
            (?d 'emacspeak-table-speak-current-element)
            (?f 'emacspeak-table-speak-row-filtered)
            (?g 'emacspeak-table-speak-column-filtered)
            (?. 'emacspeak-table-speak-coordinates)
            (otherwise (message "Invalid method specified")
                       emacspeak-table-speak-element)))
    (emacspeak-auditory-icon 'button)))

;;}}}
;;{{{ Navigating the table:

(defvar emacspeak-table-speak-element
  'emacspeak-table-speak-current-element
  "Function to call when automatically speaking table elements.")

(make-variable-buffer-local 'emacspeak-table-speak-element)


(defun emacspeak-table-next-row (&optional count)
  "Move to the next row if possible"
  (interactive "p")
  (cl-declare (special emacspeak-table))
  (cl-assert  (boundp 'emacspeak-table) nil "No table here")
  (setq count (or count 1))
  (emacspeak-table-move-down emacspeak-table count)
  (emacspeak-table-synchronize-display)
  (funcall emacspeak-table-speak-element))


(defun emacspeak-table-previous-row (&optional count)
  "Move to the previous row if possible"
  (interactive "p")
  (cl-declare (special emacspeak-table))
  (cl-assert  (boundp 'emacspeak-table) nil "No table here")
  (setq count (or count 1))
  (emacspeak-table-move-up emacspeak-table count)
  (emacspeak-table-synchronize-display)
  (funcall emacspeak-table-speak-element))


(defun emacspeak-table-next-column (&optional count)
  "Move to the next column if possible"
  (interactive "p")
  (cl-declare (special emacspeak-table))
  (cl-assert  (boundp 'emacspeak-table) nil "No table here")
  (setq count (or count 1))
  (emacspeak-table-move-right emacspeak-table count)
  (emacspeak-table-synchronize-display)
  (funcall emacspeak-table-speak-element))


(defun emacspeak-table-previous-column (&optional count)
  "Move to the previous column  if possible"
  (interactive "p")
  (cl-declare (special emacspeak-table))
  (cl-assert  (boundp 'emacspeak-table) nil "No table here")
  (setq count (or count 1))
  (emacspeak-table-move-left emacspeak-table count)
  (emacspeak-table-synchronize-display)
  (funcall emacspeak-table-speak-element))

(defun emacspeak-table-goto (row column)
  "Prompt for a table cell coordinates and jump to it."
  (interactive "nRow:\nNColumn:")
  (cl-declare (special emacspeak-table))
  (cl-assert  (boundp 'emacspeak-table) nil "No table here")
  (emacspeak-table-goto-cell emacspeak-table row column)
  (emacspeak-table-synchronize-display)
  (funcall emacspeak-table-speak-element)
  (emacspeak-auditory-icon 'large-movement))

(defun emacspeak-table-goto-top ()
  "Goes to the top of the current column."
  (interactive)
  (cl-declare (special emacspeak-table))
  (cl-assert  (boundp 'emacspeak-table) nil "No table here")
  (emacspeak-table-goto-cell
   emacspeak-table
   0 (emacspeak-table-current-column emacspeak-table))
  (emacspeak-table-synchronize-display)
  (funcall emacspeak-table-speak-element)
  (emacspeak-auditory-icon 'large-movement))

(defun emacspeak-table-goto-bottom ()
  "Goes to the bottom of the current column."
  (interactive)
  (cl-declare (special emacspeak-table))
  (cl-assert  (boundp 'emacspeak-table) nil "No table here")
  (emacspeak-table-goto-cell
   emacspeak-table

   (1- (emacspeak-table-num-rows emacspeak-table))
   (emacspeak-table-current-column
    emacspeak-table))
  (emacspeak-table-synchronize-display)
  (funcall emacspeak-table-speak-element)
  (emacspeak-auditory-icon 'large-movement))

(defun emacspeak-table-goto-left ()
  "Goes to the left of the current row."
  (interactive)
  (cl-declare (special emacspeak-table))
  (cl-assert  (boundp 'emacspeak-table) nil "No table here")
  (emacspeak-table-goto-cell
   emacspeak-table
   (emacspeak-table-current-row emacspeak-table) 0)
  (emacspeak-table-synchronize-display)
  (funcall emacspeak-table-speak-element)
  (emacspeak-auditory-icon 'left))

(defun emacspeak-table-goto-right ()
  "Goes to the right of the current row."
  (interactive)
  (cl-declare (special emacspeak-table))
  (cl-assert  (boundp 'emacspeak-table) nil "No table here")
  (emacspeak-table-goto-cell
   emacspeak-table
   (emacspeak-table-current-row emacspeak-table)
   (1- (emacspeak-table-num-columns emacspeak-table)))
  (emacspeak-table-synchronize-display)
  (funcall emacspeak-table-speak-element)
  (emacspeak-auditory-icon 'right))

;;}}}
;;{{{ searching and finding:

(defun emacspeak-table-search (&optional what)
  "Search the table for matching elements.  Interactively prompts for
row or column to search and pattern to look for.    If there is a match, makes
the matching cell current. When called from a program, `what' can
  be either `row' or `column'."
  (interactive "P")
  (cl-declare (special emacspeak-table))
  (cl-assert  (boundp 'emacspeak-table) nil "No table here")
  (message "Search   in: r row c column")
  (let* ((row (emacspeak-table-current-row emacspeak-table))
         (column (emacspeak-table-current-column emacspeak-table))
         (found nil)
         (slice
          (or what
              (cl-case (read-char)
                (?r 'row)
                (?c 'column)
                (otherwise (error "Can only search in either row or column")))))
         (pattern
          (read-string
           (format "Search in current  %s for: " slice))))
    (cond
     ((eq slice 'row)
      (setq found
            (emacspeak-table-find-match-in-row
             emacspeak-table row pattern 'string-match)))
     ((eq slice 'column)
      (setq found
            (emacspeak-table-find-match-in-column
             emacspeak-table column pattern 'string-match)))
     (t (error "Invalid search")))
    (cond
     (found
      (cond
       ((eq slice 'row)
        (emacspeak-table-goto-cell emacspeak-table row found))
       ((eq slice 'column)
        (emacspeak-table-goto-cell emacspeak-table found column)))
      (emacspeak-table-synchronize-display)
      (emacspeak-auditory-icon 'search-hit))
     (t (emacspeak-auditory-icon 'search-miss)))
    (funcall emacspeak-table-speak-element)))

(defun emacspeak-table-search-row ()
  "Search in current table row."
  (interactive)
  (emacspeak-table-search 'row))

(defun emacspeak-table-search-column ()
  "Search in current table column."
  (interactive)
  (emacspeak-table-search 'column))

(defun emacspeak-table-search-headers ()
  "Search the table row or column headers.  Interactively prompts for
row or column to search and pattern to look for.  If there is a
match, makes the matching row or column current."
  (interactive)
  (cl-declare (special emacspeak-table))
  (cl-assert  (boundp 'emacspeak-table) nil "No table here")
  (message
   "Search headers : r row c column")
  (let* ((row (emacspeak-table-current-row emacspeak-table))
         (column (emacspeak-table-current-column emacspeak-table))
         (found nil)
         (slice
          (cl-case (read-char "Search headers : r row c column")
            (?r 'row)
            (?c 'column)
            (otherwise (error "Can only search in either row or column"))))
         (pattern
          (completing-read
           (format "Search %s headers for: " slice)
           (cond
            ((eq slice 'row)
             (append (emacspeak-table-row-header emacspeak-table) nil))
            ((eq slice 'column)
             (append (emacspeak-table-column-header emacspeak-table) nil)))
           nil 'must-match)))
    (cond
     ((eq slice 'row)
      (setq found
            (emacspeak-table-find-match-in-column
             emacspeak-table 0 pattern 'string-match)))
     ((eq slice 'column)
      (setq found
            (emacspeak-table-find-match-in-row
             emacspeak-table 0 pattern 'string-match)))
     (t (error "Invalid search")))
    (cond
     (found
      (cond
       ((eq slice 'row)
        (emacspeak-table-goto-cell emacspeak-table  found column))
       ((eq slice 'column)
        (emacspeak-table-goto-cell emacspeak-table row found)))
      (emacspeak-table-synchronize-display)
      (emacspeak-auditory-icon 'search-hit))
     (t (emacspeak-auditory-icon 'search-miss)))
    (emacspeak-table-speak-both-headers-and-element)))

;;}}}
;;{{{ cutting and pasting tables:

(defun emacspeak-table-copy-current-element-to-kill-ring ()
  "Copy current table element to kill ring."
  (interactive)
  (cl-declare (special emacspeak-table))
  (cl-assert  (boundp 'emacspeak-table) nil "No table here")
  (kill-new  (emacspeak-table-current-element emacspeak-table))
  (when (called-interactively-p 'interactive) 
    (emacspeak-auditory-icon 'yank-object)
    (message "Copied element to kill ring")))
(defun emacspeak-table-copy-current-element-to-register (register)
  "Copy current table element to specified register."
  (interactive (list (register-read-with-preview "Copy to register: ")))
  (cl-declare (special emacspeak-table))
  (cl-assert  (boundp 'emacspeak-table) nil "No table here")
  (set-register register (emacspeak-table-current-element
                          emacspeak-table))
  (when (called-interactively-p 'interactive)
    (emacspeak-auditory-icon 'select-object)
    (message "Copied element to register %c" register)))
;;}}}
;;{{{ variables

;;; Implementing table editing and table clipboard.
(defvar emacspeak-table-clipboard nil
  "Variable to hold table copied to the clipboard.")

;;}}}
;;{{{  define table markup structure and accessors

(cl-defstruct (emacspeak-table-markup
               (:constructor
                emacspeak-table-make-markup))
  table-start
  table-end
  row-start
  row-end
  col-start
  col-end
  col-separator)

(defvar emacspeak-table-markup-table  (make-hash-table)
  "Hash table to hold mapping between major modes and mode specific
table markup.")

(defun emacspeak-table-markup-set-table (mode markup)
  (cl-declare (special emacspeak-table-markup-table))
  (setf  (gethash mode emacspeak-table-markup-table) markup))

(defun emacspeak-table-markup-get-table (mode)
  (cl-declare (special emacspeak-table-markup-table))
  (or (gethash mode emacspeak-table-markup-table)
      (gethash 'fundamental-mode emacspeak-table-markup-table)))

;;}}}
;;{{{  define table markup for the various modes of interest
(let ((html-table
       (emacspeak-table-make-markup
        :table-start "<TABLE>\n"
        :table-end "</TABLE>\n"
        :row-start "<TR>\n"
        :row-end "</TR>\n"
        :col-start "<TD>\n"
        :col-end "</TD>\n"
        :col-separator "")))
  (emacspeak-table-markup-set-table 'xml-mode html-table)
  (emacspeak-table-markup-set-table 'nxml-mode html-table)
  (emacspeak-table-markup-set-table 'html-helper-mode html-table))

(emacspeak-table-markup-set-table 'latex2e-mode
                                  (emacspeak-table-make-markup
                                   :table-start "\\begin{tabular}{}\n"
                                   :table-end "\\end{tabular}\n"
                                   :row-start ""
                                   :row-end "\\\\\n"
                                   :col-start ""
                                   :col-end ""
                                   :col-separator " & "))
(emacspeak-table-markup-set-table 'latex-mode
                                  (emacspeak-table-markup-get-table
                                   'latex2e-mode))

(emacspeak-table-markup-set-table 'LaTeX-mode
                                  (emacspeak-table-markup-get-table
                                   'latex2e-mode))

(emacspeak-table-markup-set-table 'TeX-mode
                                  (emacspeak-table-markup-get-table
                                   'latex2e-mode))

(emacspeak-table-markup-set-table
 'org-mode
 (emacspeak-table-make-markup
  :table-start ""
  :table-end ""
  :row-start "|"
  :row-end "|\n"
  :col-start ""
  :col-end ""
  :col-separator "|"))

(emacspeak-table-markup-set-table 'fundamental-mode
                                  (emacspeak-table-make-markup
                                   :table-start ""
                                   :table-end ""
                                   :row-start ""
                                   :row-end "\n"
                                   :col-start "\""
                                   :col-end "\""
                                   :col-separator ", "))

(emacspeak-table-markup-set-table 'text-mode
                                  (emacspeak-table-make-markup
                                   :table-start
                                   "\n------------------------------------------------------------\n"
                                   :table-end
                                   "\n------------------------------------------------------------\n"
                                   :row-start ""
                                   :row-end "\n"
                                   :col-start ""
                                   :col-end ""
                                   :col-separator "\t"))

;;}}}
;;{{{ copy and paste tables

(defun emacspeak-table-copy-to-clipboard ()
  "Copy table in current buffer to the table clipboard.
Current buffer must be in emacspeak-table mode."
  (interactive)
  (cl-declare (special emacspeak-table-clipboard emacspeak-table))
  (cl-assert (eq   major-mode 'emacspeak-table-mode)  nil "Not in table mode.")
  (cl-assert  (boundp 'emacspeak-table) nil "No table here")
  (setq emacspeak-table-clipboard emacspeak-table)
  (message "Copied current table to emacspeak table clipboard."))

(defun emacspeak-table-paste-from-clipboard ()
  "Paste the emacspeak table clipboard into the current buffer.
Use the major  mode of this buffer to  decide what kind of table
markup to use."
  (interactive)
  (cl-declare (special emacspeak-table-clipboard))
  (let ((mode  major-mode)
        (markup nil)
        (table (emacspeak-table-elements emacspeak-table-clipboard))
        (read-only buffer-read-only)
        (table-start nil)
        (table-end nil)
        (row-start nil)
        (row-end nil)
        (col-start nil)
        (col-end nil)
        (col-separator nil))
    (cond
     (read-only (error "Cannot paste into read only buffer."))
     (t
      (setq markup  (emacspeak-table-markup-get-table mode))
      (setq table-start (emacspeak-table-markup-table-start markup)
            table-end (emacspeak-table-markup-table-end markup)
            row-start (emacspeak-table-markup-row-start markup)
            row-end (emacspeak-table-markup-row-end markup)
            col-start (emacspeak-table-markup-col-start markup)
            col-end (emacspeak-table-markup-col-end markup)
            col-separator (emacspeak-table-markup-col-separator markup))
      (insert (format "%s" table-start))
      (cl-loop for row across table
               do
               (insert (format "%s" row-start))
               (let
                   ((current 0)
                    (final (length row)))
                 (cl-loop
                  for column across row do
                  (insert (format "%s %s %s"
                                  col-start column col-end))
                  (cl-incf current)
                  (unless (= current final)
                    (insert (format "%s" col-separator)))))              (insert (format "%s" row-end)))
      (insert (format "%s" table-end))))))

;;}}}
;;{{{  table sorting:

(defun emacspeak-table-sort-on-current-column ()
  "Sort table on current column. "
  (interactive)
  (cl-declare (special major-mode emacspeak-table
                       emacspeak-table-speak-row-filter))
  (cl-assert (eq major-mode  'emacspeak-table-mode) nil "Not in table mode.")
  (let* ((column  (emacspeak-table-current-column emacspeak-table))
         (row-head   nil)
         (row-filter emacspeak-table-speak-row-filter)
         (rows (append
                (emacspeak-table-elements emacspeak-table) nil))
         (sorted-table nil)
         (sorted-row-list nil)
         (buffer(get-buffer-create  (format "sorted-on-%d" column))))
    (setq row-head (pop rows)) ;;; header does not play in sort
    (setq  rows
           (cl-remove-if
            #'(lambda (row)
                (null (aref row column)))
            rows))
    (setq
     sorted-row-list
     (sort
      rows
      #'(lambda (x y)
          (cond
           ((and (numberp (read  (aref x column)))
                 (numberp (read  (aref y column))))
            (< (read  (aref x column))
               (read  (aref y column))))
           ((and (stringp  (aref x column))
                 (stringp (aref y column)))
            (string-lessp (aref x column)
                          (aref y column)))
           (t (string-lessp
               (format "%s" (aref x column))
               (format "%s" (aref y column))))))))
    (push row-head sorted-row-list)
    (setq sorted-table (make-vector (length sorted-row-list) nil))
    (cl-loop
     for i from 0 to (1- (length sorted-row-list)) do
     (aset sorted-table i (nth i sorted-row-list)))
    (emacspeak-table-prepare-table-buffer
     (emacspeak-table-make-table  sorted-table) buffer)
    (switch-to-buffer buffer)
    (setq emacspeak-table-speak-row-filter row-filter)
    (emacspeak-table-goto  0 column)
    (call-interactively #'emacspeak-table-next-row)))

;;}}}
;;{{{  persistent store

(defun emacspeak-table-ui-generate-key ()
  "Generates a key for current context.
The key is used when persisting out the filter setting for
future  use."
  (cl-declare (special  major-mode))
  (or (buffer-file-name)
      (format "%s:%s" (buffer-name) major-mode)))

(defvar emacspeak-table-ui-filter-table (make-hash-table :test 'equal)
  "Stores table filter  settings.")

(defun emacspeak-table-ui-filter-set (key filter)
  "Map filter to key."
  (cl-declare (special emacspeak-table-ui-filter-table))
  (setf (gethash key emacspeak-table-ui-filter-table) filter))

(defun emacspeak-table-ui-filter-get (key)
  "Lookup key and return corresponding filter. "
  (cl-declare (special emacspeak-table-ui-filter-table))
  (gethash key emacspeak-table-ui-filter-table))

(defun emacspeak-table-ui-filter-load (file)
  "Load saved filter settings."
  (interactive
   (list
    (read-file-name "Load filter settings  from file: "
                    emacspeak-user-directory
                    ".table-ui-filter")))
  (condition-case nil
      (progn
        (load
         (expand-file-name  file emacspeak-user-directory)))
    (error (message "Error loading resources from %s "
                    file))))

(defun emacspeak-table-ui-filter-save (file)
  "Save out filter settings."
  (interactive
   (list
    (read-file-name "Save table-ui-filter settings  to file: "
                    emacspeak-user-directory
                    ".table-ui-filter")))
  (cl-declare (special emacspeak-user-directory))
  (let ((buffer (find-file-noselect
                 (expand-file-name file
                                   emacspeak-user-directory))))
    (save-current-buffer
      (set-buffer buffer)
      (erase-buffer)
      (cl-loop for key being the hash-keys of
               emacspeak-table-ui-filter-table
               do
               (insert
                (format
                 "\n(setf
 (gethash %s emacspeak-table-ui-filter-table)
 (quote %s))"
                 (prin1-to-string key)
                 (prin1-to-string (emacspeak-table-ui-filter-get
                                   key)))))
      (basic-save-buffer)
      (kill-buffer buffer))))

;;}}}
(provide  'emacspeak-table-ui)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

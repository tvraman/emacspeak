;;; emacspeak-table-ui.el --- Emacspeak's current notion of an ideal table UI
;;; $Id$
;;; $Author$
;;; Description: Emacspeak table handling module
;;; Keywords:emacspeak, audio interface to emacs tables are structured
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
;;;Copyright (C) 1995 -- 2006, T. V. Raman
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
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}

;;{{{  Introduction

;;; User interface to tables

;;}}}
;;{{{ requires
(require 'emacspeak-preamble)
(require 'emacspeak-table)

;;}}}
;;{{{ define personalities

(defgroup emacspeak-table nil
  "Table browsing on the Emacspeak desktop."
  :group 'emacspeak
  :prefix "emacspeak-table-")

(defcustom emacspeak-table-column-header-personality voice-smoothen
  "personality for speaking column headers."
  :type 'symbol
  :group 'emacspeak-table)

(defcustom emacspeak-table-row-header-personality voice-bolden
  "Personality for speaking row headers"
  :type 'symbol
  :group 'emacspeak-table)

;;}}}
;;{{{  emacspeak table mode

(defvar emacspeak-table-keymap (make-sparse-keymap)
  "Keymap for using in table browsing mode")

(loop for binding in
      '(
        ( "\M-l" emacspeak-table-ui-filter-load)
        ( "\M-s" emacspeak-table-ui-filter-save)
        ("#"
         emacspeak-table-sort-on-current-column)
        ("q" emacspeak-kill-buffer-quietly)
        ("x" emacspeak-table-copy-current-element-to-register)
        ("w" emacspeak-table-copy-current-element-to-kill-ring)
        ("\t" emacspeak-table-next-column)
        ( [<shift> tab] emacspeak-table-previous-column)
        ("j" emacspeak-table-goto)
        ([up] emacspeak-table-previous-row)
        ("p" emacspeak-table-previous-row)
        ("\C-p" emacspeak-table-previous-row)
        ([down] emacspeak-table-next-row)
        ("n" emacspeak-table-next-row)
        ("\C-n" emacspeak-table-next-row)
        ([left] emacspeak-table-previous-column)
        ("\C-b" emacspeak-table-previous-column)
        ([right] emacspeak-table-next-column)
        ("\C-f" emacspeak-table-next-column)
        ("r" emacspeak-table-speak-row-header-and-element )
        ("c" emacspeak-table-speak-column-header-and-element)
        (" " emacspeak-table-speak-current-element)
        ("b" emacspeak-table-speak-both-headers-and-element)
        ("." emacspeak-table-speak-coordinates)
        ("=" emacspeak-table-speak-dimensions)
        ("a" emacspeak-table-select-automatic-speaking-method)
        ("s" emacspeak-table-search)
        ("C" emacspeak-table-search-column)
        ("R" emacspeak-table-search-row)
        ("f" emacspeak-table-speak-row-filtered)
        ("g" emacspeak-table-speak-column-filtered)
        ("h" emacspeak-table-search-headers)
        ("k" emacspeak-table-copy-to-clipboard)
        ("T" emacspeak-table-goto-top)
        ("B" emacspeak-table-goto-bottom)
        ("L" emacspeak-table-goto-left)
        ("E" emacspeak-table-goto-right))
      do
      (emacspeak-keymap-update emacspeak-table-keymap binding))

(defun emacspeak-table-mode ()
  "Major mode for browsing tables.
Table mode is designed to allow speech users to browse tabular data
with full contextual feedback while retaining all the power of the
two-dimensional spatial layout of tables.

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

R               emacspeak-table-goto-right
L               emacspeak-table-goto-left
B               emacspeak-table-goto-bottom
T               emacspeak-table-goto-top

The next two commands let you search the table.
The commands ask you if you want to search rows or columns.
When searching headers remember that row 0 is the column header, and that column 0 is the row header.

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
having 8 columns, a row filter of ( 1 2 3) will speak only entries 1 2 and 3.
Use the sample table matrix.dat to familiarize yourself with this
feature. Note that you can intersperse meaningful strings in the list that
specifies the filter"
  (declare (special emacspeak-table-keymap))
  (use-local-map emacspeak-table-keymap)
  (set (make-local-variable 'voice-lock-mode) t)
  (setq major-mode 'emacspeak-table-mode
        mode-name "table")
  (put-text-property
   (point-min)
   (point-max)
   'point-entered
   'emacspeak-table-point-motion-hook)
  (not-modified)
  (setq buffer-undo-list t)
  (setq buffer-read-only t)
  (emacspeak-auditory-icon 'select-object)
  (emacspeak-speak-mode-line))

;;}}}
;;{{{  speaking current entry

(defsubst emacspeak-table-synchronize-display ()
  "Bring visual display in sync with internal representation"
  (declare (special emacspeak-table positions))
  (let ((row (emacspeak-table-current-row emacspeak-table))
        (column (emacspeak-table-current-column
                 emacspeak-table))
        (width (frame-width)))
    (goto-char
     (or
      (gethash
       (intern
        (format "element:%s:%s" row column))
       positions)
      (point)))
    (scroll-left (- (current-column)
                    (+ (/ width  2)
                       (window-hscroll))))))

(defsubst  emacspeak-table-speak-coordinates ()
  "Speak current table coordinates."
  (interactive)
  (declare (special emacspeak-table))
  (and (boundp 'emacspeak-table)
       (message "Row %s Column %s"
                (emacspeak-table-current-row emacspeak-table)
                (emacspeak-table-current-column
                 emacspeak-table))))

(defsubst  emacspeak-table-speak-dimensions ()
  "Speak current table dimensions."
  (interactive)
  (declare (special emacspeak-table))
  (and (boundp 'emacspeak-table)
       (message "%s by %s table"
                (emacspeak-table-num-rows emacspeak-table)
                (emacspeak-table-num-columns emacspeak-table))))

(defun emacspeak-table-speak-current-element ()
  "Speak current table element"
  (interactive)
  (declare (special emacspeak-table ))
  (and (boundp 'emacspeak-table)
       (dtk-speak (emacspeak-table-current-element emacspeak-table))))

(defun emacspeak-table-speak-row-header-and-element ()
  "Speak  row header and table element"
  (interactive)
  (declare (special emacspeak-table
                    emacspeak-table-row-header-personality))
  (and (boundp 'emacspeak-table)
       (let ((head (format "%s"
                           (emacspeak-table-row-header-element emacspeak-table
                                                               (emacspeak-table-current-row emacspeak-table )))))
         (put-text-property 0 (length head)
                            'personality
                            emacspeak-table-row-header-personality head)
         (dtk-speak
          (concat head
                  (format " %s"
                          (emacspeak-table-current-element
                           emacspeak-table)))))))

(defun emacspeak-table-speak-column-header-and-element ()
  "Speak  column header and table element"
  (interactive)
  (declare (special emacspeak-table
                    emacspeak-table-column-header-personality))
  (and (boundp 'emacspeak-table)
       (let ((head (format "%s"
                           (emacspeak-table-column-header-element emacspeak-table
                                                                  (emacspeak-table-current-column emacspeak-table )))))
         (put-text-property 0 (length head)
                            'personality
                            emacspeak-table-column-header-personality head)
         (dtk-speak
          (concat head
                  (format " %s"
                          (emacspeak-table-current-element
                           emacspeak-table)))))))

(defun emacspeak-table-speak-both-headers-and-element ()
  "Speak  both row and column header and table element"
  (interactive)
  (declare (special emacspeak-table
                    emacspeak-table-column-header-personality
                    emacspeak-table-row-header-personality))
  (and (boundp 'emacspeak-table)
       (let ((column-head (format "%s"
                                  (emacspeak-table-column-header-element emacspeak-table
                                                                         (emacspeak-table-current-column emacspeak-table ))))
             (row-head (format "%s"
                               (emacspeak-table-row-header-element emacspeak-table
                                                                   (emacspeak-table-current-row emacspeak-table )))))
         (put-text-property 0 (length row-head)
                            'personality
                            emacspeak-table-row-header-personality
                            row-head)
         (put-text-property 0 (length column-head)
                            'personality
                            emacspeak-table-column-header-personality column-head)
         (dtk-speak
          (concat row-head" "  column-head
                  (format " %s"
                          (emacspeak-table-current-element
                           emacspeak-table)))))))

(defsubst emacspeak-table-get-entry-with-headers  (row column &optional row-head-p col-head-p)
  "Return   both row and column header and table element"
  (interactive)
  (declare (special emacspeak-table
                    emacspeak-table-column-header-personality
                    emacspeak-table-row-header-personality))
  (and (boundp 'emacspeak-table)
       (let ((column-head (format "%s"
                                  (emacspeak-table-column-header-element emacspeak-table column)))
             (row-head (format "%s"
                               (emacspeak-table-row-header-element
                                emacspeak-table row))))
         (and row-head-p
              (put-text-property 0 (length row-head)
                                 'personality
                                 emacspeak-table-row-header-personality
                                 row-head))
         (and col-head-p
              (put-text-property 0 (length column-head)
                                 'personality
                                 emacspeak-table-column-header-personality column-head))
         (concat
          (if row-head-p
              row-head
            "")
          " "
          (if col-head-p column-head  "")
          (format " %s"
                  (emacspeak-table-this-element emacspeak-table row column))))))

(defvar emacspeak-table-speak-row-filter nil
  "Template specifying how a row is filtered before it is spoken.")

(make-variable-buffer-local 'emacspeak-table-speak-row-filter)

(defun emacspeak-table-speak-row-filtered  (&optional prefix)
  "Speaks a table row after applying a specified row filter.
Optional prefix arg prompts for a new filter."
  (interactive "P")
  (declare (special emacspeak-table-speak-row-filter
                    voice-animate
                    voice-smoothen
                    emacspeak-table))
  (unless (and  emacspeak-table-speak-row-filter
                (listp emacspeak-table-speak-row-filter)
                (not prefix))
    (setq emacspeak-table-speak-row-filter
          (read-minibuffer "Specify row filter as a list: "
                           (format "%s"
                                   (or (emacspeak-table-ui-filter-get
                                        (emacspeak-table-ui-generate-key))
                                       "("))))
    (emacspeak-table-ui-filter-set
     (emacspeak-table-ui-generate-key)
     emacspeak-table-speak-row-filter))
  (let ((voice-lock-mode t))
    (dtk-speak
     (mapconcat
      #'(lambda (token)
          (let ((value nil))
            (cond
             ((stringp token) token)
             ((numberp token)
              (setq value
                    (emacspeak-table-get-entry-with-headers
                     (emacspeak-table-current-row emacspeak-table)
                     token))
              (put-text-property 0 (length value)
                                 'personality voice-smoothen  value)
              value)
             ((and (listp token)
                   (numberp (first token))
                   (numberp (second token )))
              (setq value
                    (emacspeak-table-get-entry-with-headers
                     (first token)
                     (second token)))
              (put-text-property 0 (length value)
                                 'personality voice-smoothen value)
              value)
             (t  (format "%s" token)))))
      emacspeak-table-speak-row-filter
      " "))))

(defvar emacspeak-table-speak-column-filter nil
  "Template specifying how a column is filtered before it is spoken.")

(make-variable-buffer-local 'emacspeak-table-speak-column-filter)

(defun emacspeak-table-speak-column-filtered  (&optional prefix)
  "Speaks a table column after applying a specified column filter.
Optional prefix arg prompts for a new filter."
  (interactive "P")
  (declare (special emacspeak-table-speak-column-filter
                    emacspeak-table))
  (unless (and  emacspeak-table-speak-column-filter
                (listp emacspeak-table-speak-column-filter)
                (not prefix))
    (setq emacspeak-table-speak-column-filter
          (read-minibuffer "Specify column filter as a list: " "(")))
  (dtk-speak
   (mapconcat
    #'(lambda (token)
        (cond
         ((stringp token) token)
         ((numberp token)
          (emacspeak-table-get-entry-with-headers
           token
           (emacspeak-table-current-column emacspeak-table)))
         ((and (listp token)
               (numberp (first token))
               (numberp (second token )))
          (emacspeak-table-get-entry-with-headers
           (first token)
           (second token)))
         (t  (format "%s" token))))
    emacspeak-table-speak-column-filter
    " ")))

;;}}}
;;{{{  what to do when point moves

(defun emacspeak-table-point-motion-hook (old new )
  "Bring internal representation in sync with visual display"
  (declare (special emacspeak-table))
  (condition-case nil
      (emacspeak-table-goto-cell emacspeak-table
                                 (get-text-property new 'row)
                                 (get-text-property new 'column))
    (error nil ))
  (push-mark old t))

;;}}}
;;{{{  opening a file of table data

;;{{{ csv helpers:

(defsubst ems-csv-forward-field ()
  "Skip forward over one field."
  (if (eq (following-char) ?\")
      (forward-sexp)
    (skip-chars-forward "^,\n")))

(defsubst ems-csv-backward-field ()
  "Skip backward over one field."
  (if (eq (preceding-char) ?\")
      (backward-sexp)
    (skip-chars-backward "^,\n")))

;;}}}


(defsubst emacspeak-table-prepare-table-buffer (table buffer
                                                      &optional filename)
  "Prepare tabular data."
  (declare (special emacspeak-table positions ))
  (save-excursion
    (set-buffer buffer)
    (let ((i 0)
          (j 0)
          (count 0)
          (row-start 1)
          (column-start 1)
          (inhibit-read-only t))
      (setq truncate-lines t)
      (erase-buffer)
      (set (make-local-variable 'emacspeak-table) table)
      (set (make-local-variable 'positions)
           (make-hash-table))
      (when filename (setq buffer-file-name filename))
      (setq count (1-  (emacspeak-table-num-columns table)))
      (loop for row across (emacspeak-table-elements table)
            do
            (loop for element across row
                  do
                  (setf
                   (gethash
                    (intern (format "element:%s:%s" i j ))
                    positions)
                   (point))
                  (insert
                   (format "%s%s"
                           (emacspeak-table-this-element table i j )
                           (if (=  j count)
                               "\n"
                             "\t")))
                  (put-text-property column-start (point)
                                     'column j)
                  (setq column-start (point))
                  (incf j))
            (setq j 0)
            (put-text-property row-start (point) 'row i)
            (setq row-start (point))
            (incf i))
      (emacspeak-table-mode)
      (goto-char (point-min))))
  (switch-to-buffer buffer)
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
  (declare (special positions))
  (let ((buffer (get-buffer-create (format  "*%s*"
                                            (file-name-nondirectory filename))))
        (data nil)
        (table nil))
    (setq data (find-file-noselect filename))
    (setq table (emacspeak-table-make-table (read data)))
    (kill-buffer data )
    (emacspeak-table-prepare-table-buffer table buffer filename )))

(defsubst ems-csv-get-fields ()
  "Return list of fields on this line."
  (let ((fields nil)
        (this-field nil)
        (start (line-beginning-position)))
    (save-excursion
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
        (when (= (char-after) ?,)
          (forward-char 1))
        (setq start (point))))
    (nreverse fields)))
        
;;;###autoload
(defun emacspeak-table-find-csv-file (filename)
  "Process a csv (comma separated values) file.
The processed  data and presented using emacspeak table navigation. "
  (interactive "FFind CSV file: ")
  (let ((scratch (get-buffer-create "*csv-scratch*"))
        (table nil)
        (elements nil)
        (this-row nil)
        (fields nil)
        (buffer (get-buffer-create
                 (format "*%s-table*"
                         (file-name-nondirectory filename)))))
    (save-excursion
      (set-buffer scratch)
      (fundamental-mode)
      (setq buffer-undo-list t)
      (erase-buffer)
      (insert-file-contents filename)
      (flush-lines "^ *$")
      (goto-char (point-min))
      (setq elements
            (make-vector (count-lines (point-min) (point-max))
                         nil))
      (loop for i from 0 to (1- (length elements))
            do
            (setq fields (ems-csv-get-fields))
            (aset elements i (apply 'vector fields))
            (forward-line 1))
      (setq table (emacspeak-table-make-table elements)))
    (kill-buffer scratch)
    (emacspeak-table-prepare-table-buffer table buffer
                                          filename )))

;;;###autoload
(defun emacspeak-table-view-csv-buffer (&optional buffer-name)
  "Process a csv (comma separated values) data.
The processed  data and presented using emacspeak table navigation. "
  (interactive)
  (or buffer-name
      (setq buffer-name (current-buffer)))
  (let ((scratch (get-buffer-create "*csv-scratch*"))
        (table nil)
        (buffer (get-buffer-create
                 (format "*%s-table*" buffer-name))))
    (save-excursion
      (set-buffer scratch)
      (setq buffer-undo-list t)
      (erase-buffer)
      (insert-buffer buffer-name)
      (goto-char (point-min))
      (flush-lines "^ *$")
      (insert "[\n")
      (while (re-search-forward "^" nil t)
        (replace-match "["))
      (goto-char (point-min))
      (while (re-search-forward "," nil t)
        (replace-match " "))
      (goto-char (point-min))
      (forward-line 1)
      (while (and (not (eobp))
                  (re-search-forward "$" nil t))
        (replace-match "]")
        (forward-line 1))
      (goto-char (point-min))
      (when (search-forward "[]" nil t)
        (replace-match ""))
      (goto-char (point-max))
      (insert "]\n")
      (goto-char (point-min))
      (setq table (emacspeak-table-make-table (read scratch))))
    (kill-buffer scratch)
    (emacspeak-table-prepare-table-buffer table buffer)))

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
  (declare (special emacspeak-table positions))
  (let ((buffer-undo-list t)
        (workspace (get-buffer-create " table workspace  "))
        (buffer (get-buffer-create
                 (format  "table-%s"
                          (or (buffer-name)
                              "scratch"))))
        (table nil)
        (data nil)
        (i 0)
        (j 0)
        (count 0)
        (row-start 1)
        (column-start 1)
        (text (buffer-substring start end)))
    (save-excursion
      (when (= 10 (string-to-char (substring text -1)))
        (setq text (substring text 0 -1)))
      (set-buffer workspace)
      (erase-buffer)
      (insert text)
      (goto-char (point-min))
      (flush-lines "^ *$")
      (setq table (emacspeak-table-make-table
                   (ems-tabulate-parse-region
                    (point-min)
                    (point-max)))))
    (kill-buffer workspace)
    (save-excursion
      (set-buffer buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (set (make-local-variable 'emacspeak-table) table)
        (set (make-local-variable 'positions) (make-hash-table))
        (setq count (1-  (emacspeak-table-num-columns table)))
        (loop for row across (emacspeak-table-elements table)
              do
              (loop for element across row
                    do
                    (setf
                     (gethash
                      (intern (format "element:%s:%s" i j ))
                      positions)
                     (point))
                    (insert
                     (format "%s%s"
                             (emacspeak-table-this-element table i j )
                             (if (=  j count)
                                 "\n"
                               "\t")))
                    (put-text-property column-start (point)
                                       'column j)
                    (setq column-start (point))
                    (incf j))
              (setq j 0)
              (put-text-property row-start (point) 'row i)
              (setq row-start (point))
              (incf i))
        (emacspeak-table-mode)
        (goto-char (point-min))))
    (switch-to-buffer buffer)
    (rename-buffer
     (format "%sX%s-%s"
             (emacspeak-table-num-rows emacspeak-table)
             (emacspeak-table-num-columns emacspeak-table)
             (buffer-name buffer) ))))

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
  (declare (special emacspeak-table-speak-element))
  (message emacspeak-table-select-automatic-speaking-method-prompt)
  (let ((key (read-char)))
    (setq emacspeak-table-speak-element
          (case  key
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

(defun emacspeak-table-next-row (&optional count)
  "Move to the next row if possible"
  (interactive "p")
  (declare (special emacspeak-table ))
  (setq count (or count 1 ))
  (cond
   ((not (boundp 'emacspeak-table))
    (error "Cannot find table associated with this buffer"))
   (t (emacspeak-table-move-down emacspeak-table count )
      (emacspeak-table-synchronize-display)
      (funcall emacspeak-table-speak-element))))

(defun emacspeak-table-previous-row (&optional count)
  "Move to the previous row if possible"
  (interactive "p")
  (declare (special emacspeak-table ))
  (setq count (or count 1 ))
  (cond
   ((not (boundp 'emacspeak-table))
    (error "Cannot find table associated with this buffer"))
   (t (emacspeak-table-move-up emacspeak-table count )
      (emacspeak-table-synchronize-display)
      (funcall emacspeak-table-speak-element))))

(defun emacspeak-table-next-column (&optional count)
  "Move to the next column if possible"
  (interactive "p")
  (declare (special emacspeak-table ))
  (setq count (or count 1 ))
  (cond
   ((not (boundp 'emacspeak-table))
    (error "Cannot find table associated with this buffer"))
   (t(emacspeak-table-move-right emacspeak-table count )
     (emacspeak-table-synchronize-display)
     (funcall emacspeak-table-speak-element))))

(defun emacspeak-table-previous-column (&optional count)
  "Move to the previous column  if possible"
  (interactive "p")
  (declare (special emacspeak-table ))
  (setq count (or count 1 ))
  (cond
   ((not (boundp 'emacspeak-table))
    (error "Cannot find table associated with this buffer"))
   (t (emacspeak-table-move-left emacspeak-table count )
      (emacspeak-table-synchronize-display)
      (funcall emacspeak-table-speak-element))))

(defun emacspeak-table-goto (row column)
  "Prompt for a table cell coordinates and jump to it."
  (interactive "nRow:\nNColumn:")
  (declare (special emacspeak-table))
  (cond
   ((not (boundp 'emacspeak-table))
    (error "Cannot find table associated with this buffer."))
   (t (emacspeak-table-goto-cell emacspeak-table row column)
      (emacspeak-table-synchronize-display)
      (funcall emacspeak-table-speak-element)
      (emacspeak-auditory-icon 'large-movement))))

(defun emacspeak-table-goto-top ()
  "Goes to the top of the current column."
  (interactive)
  (declare (special emacspeak-table))
  (unless (boundp 'emacspeak-table)
    (error "Cannot find table associated with this buffer"))
  (emacspeak-table-goto-cell emacspeak-table
                             0 (emacspeak-table-current-column emacspeak-table))
  (emacspeak-table-synchronize-display)
  (funcall emacspeak-table-speak-element)
  (emacspeak-auditory-icon 'large-movement))

(defun emacspeak-table-goto-bottom ()
  "Goes to the bottom of the current column."
  (interactive)
  (declare (special emacspeak-table))
  (unless (boundp 'emacspeak-table)
    (error "Cannot find table associated with this buffer"))
  (emacspeak-table-goto-cell emacspeak-table
                             (1- (emacspeak-table-num-rows emacspeak-table))
                             (emacspeak-table-current-column
                              emacspeak-table))
  (emacspeak-table-synchronize-display)
  (funcall emacspeak-table-speak-element)
  (emacspeak-auditory-icon 'large-movement))

(defun emacspeak-table-goto-left ()
  "Goes to the left of the current row."
  (interactive)
  (declare (special emacspeak-table))
  (unless (boundp 'emacspeak-table)
    (error "Cannot find table associated with this buffer"))
  (emacspeak-table-goto-cell emacspeak-table
                             (emacspeak-table-current-row emacspeak-table)
                             0)
  (emacspeak-table-synchronize-display)
  (funcall emacspeak-table-speak-element)
  (emacspeak-auditory-icon 'large-movement))

(defun emacspeak-table-goto-right ()
  "Goes to the right of the current row."
  (interactive)
  (declare (special emacspeak-table))
  (unless (boundp 'emacspeak-table)
    (error "Cannot find table associated with this buffer"))
  (emacspeak-table-goto-cell emacspeak-table
                             (emacspeak-table-current-row emacspeak-table)
                             (1- (emacspeak-table-num-columns
                                  emacspeak-table)))
  (emacspeak-table-synchronize-display)
  (funcall emacspeak-table-speak-element)
  (emacspeak-auditory-icon 'large-movement))

;;}}}
;;{{{ searching and finding:

(defun emacspeak-table-search (&optional what)
  "Search the table for matching elements.  Interactively prompts for
row or column to search and pattern to look for.    If there is a match, makes
the matching cell current. When called from a program, `what' can
  be either `row' or `column'."
  (interactive "P")
  (declare (special emacspeak-table))
  (unless (boundp 'emacspeak-table)
    (error "Cannot find table associated with this buffer"))
  (message "Search   in: r row c column")
  (let* ((row (emacspeak-table-current-row emacspeak-table))
         (column (emacspeak-table-current-column emacspeak-table))
         (found nil)
         (slice
          (or what
              (case (read-char)
                (?r 'row)
                (?c 'column)
                (otherwise (error "Can only search in either row or column")))))
         (pattern (read-string
                   (format "Search in current  %s for: " slice ))))
    (cond
     ((eq slice 'row)
      (setq found (emacspeak-table-find-match-in-row emacspeak-table
                                                     row pattern 'string-match)))
     ((eq slice 'column)
      (setq found (emacspeak-table-find-match-in-column
                   emacspeak-table column pattern 'string-match)))
     (t (error "Invalid search")))
    (cond
     (found
      (cond
       ((eq slice 'row )
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
  (interactive )
  (declare (special emacspeak-table))
  (unless (boundp 'emacspeak-table)
    (error "Cannot find table associated with this buffer"))
  (message "Search headers : r row c column")
  (let* ((row (emacspeak-table-current-row emacspeak-table))
         (column (emacspeak-table-current-column emacspeak-table))
         (found nil)
         (slice
          (case (read-char)
            (?r 'row)
            (?c 'column)
            (otherwise (error "Can only search in either row or column"))))
         (pattern (read-string
                   (format "Search %s headers for: " slice ))))
    (cond
     ((eq slice 'row)
      (setq found (emacspeak-table-find-match-in-column
                   emacspeak-table 0 pattern 'string-match)))
     ((eq slice 'column)
      (setq found (emacspeak-table-find-match-in-row
                   emacspeak-table 0 pattern 'string-match)))
     (t (error "Invalid search")))
    (cond
     (found
      (cond
       ((eq slice 'row )
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
  (declare (special emacspeak-table ))
  (when (boundp 'emacspeak-table)
    (kill-new  (emacspeak-table-current-element emacspeak-table))
    (when (interactive-p)
      (emacspeak-auditory-icon 'delete-object)
      (message "Copied element to kill ring"))))

(defun emacspeak-table-copy-current-element-to-register (register)
  "Copy current table element to specified register."
  (interactive "cCopy to register: ")
  (declare (special emacspeak-table ))
  (when  (boundp 'emacspeak-table)
    (set-register register (emacspeak-table-current-element
                            emacspeak-table))
    (when (interactive-p)
      (emacspeak-auditory-icon 'select-object)
      (message "Copied element to register %c" register))))

;;; Implementing table editing and table clipboard.
;;{{{ variables

(defvar emacspeak-table-clipboard nil
  "Variable to hold table copied to the clipboard.")

;;}}}
;;{{{  define table markup structure and accessors

(defstruct (emacspeak-table-markup
            (:constructor
             emacspeak-table-make-markup))
  table-start
  table-end
  row-start
  row-end
  col-start
  col-end
  col-separator )

(defvar emacspeak-table-markup-table  (make-hash-table)
  "Hash table to hold mapping between major modes and mode specific
table markup.")

(defsubst emacspeak-table-markup-set-table (mode markup)
  (declare (special emacspeak-table-markup-table))
  (setf  (gethash mode emacspeak-table-markup-table) markup ))

(defsubst emacspeak-table-markup-get-table (mode )
  (declare (special emacspeak-table-markup-table))
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

(emacspeak-table-markup-set-table 'fundamental-mode
                                  (emacspeak-table-make-markup
                                   :table-start ""
                                   :table-end ""
                                   :row-start ""
                                   :row-end "\n"
                                   :col-start ""
                                   :col-end ""
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
;;;###autoload
(defun emacspeak-table-copy-to-clipboard ()
  "Copy table in current buffer to the table clipboard.
Current buffer must be in emacspeak-table mode."
  (interactive)
  (declare (special emacspeak-table-clipboard
                    emacspeak-table))
  (unless (eq major-mode 'emacspeak-table-mode)
    (error
     "emacspeak-table-copy-to-clipboard can be used only in
emacspeak-table-mode. "))
  (cond
   ((boundp 'emacspeak-table)
    (setq emacspeak-table-clipboard emacspeak-table)
    (message "Copied current table to emacspeak table clipboard."))
   (t (error "Cannot find table in current buffer"))))

(defun emacspeak-table-paste-from-clipboard ()
  "Paste the emacspeak table clipboard into the current buffer.
Use the major  mode of this buffer to  decide what kind of table
markup to use."
  (interactive)
  (declare (special emacspeak-table-clipboard emacspeak-table))
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
     (read-only
      (error "Cannot paste into read only buffer."))
     (t (setq markup  (emacspeak-table-markup-get-table mode))
        (setq table-start (emacspeak-table-markup-table-start markup)
              table-end (emacspeak-table-markup-table-end markup)
              row-start (emacspeak-table-markup-row-start markup)
              row-end (emacspeak-table-markup-row-end markup)
              col-start (emacspeak-table-markup-col-start markup)
              col-end (emacspeak-table-markup-col-end markup)
              col-separator (emacspeak-table-markup-col-separator markup))
        (insert (format "%s" table-start))
        (loop for row across table
              do
              (insert (format "%s" row-start))
              (let ((current 0)
                    (final (length row)))
                (loop for column across row
                      do
                      (insert (format "%s %s %s"
                                      col-start column col-end  ))
                      (incf current)
                      (unless (= current final)
                        (insert (format "%s" col-separator)))))              (insert (format "%s" row-end)))
        (insert (format "%s" table-end))))))

;;}}}

;;}}}
;;{{{  table sorting:

(defun emacspeak-table-sort-on-current-column ()
  "Sort table on current column. "
  (interactive )
  (declare (special major-mode emacspeak-table
                    emacspeak-table-speak-row-filter))
  (unless (eq major-mode  'emacspeak-table-mode )
    (error "This command should be used in emacspeak table mode."))
  (let* ((column  (emacspeak-table-current-column emacspeak-table))
         (row-filter emacspeak-table-speak-row-filter)
         (elements
          (loop for e across (emacspeak-table-elements emacspeak-table)
                collect e))
         (sorted-table nil)
         (sorted-list nil)
         (buffer(get-buffer-create  (format "sorted-on-%d" column ))))
    (setq sorted-list
          (sort
           elements
           #'(lambda (x y)
               (cond
                ((and (numberp (read (aref x column)))
                      (numberp (read (aref y column))))
                 (< (read (aref x column))
                    (read (aref y column))))
                ((and (stringp  (aref x column))
                      (stringp (aref y column)))
                 (string-lessp (aref x column)
                               (aref y column)))
                (t (string-lessp
                    (format "%s" (aref x column))
                    (format "%s" (aref y column))))))))
    (setq sorted-table (make-vector (length sorted-list) nil))
    (loop for i from 0 to (1- (length sorted-list))
          do
          (aset sorted-table i (nth i sorted-list)))
    (emacspeak-table-prepare-table-buffer
     (emacspeak-table-make-table  sorted-table)
     buffer)
    (save-excursion
      (set-buffer buffer)
      (setq emacspeak-table-speak-row-filter row-filter))
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{  persistent store

(defun emacspeak-table-ui-generate-key ()
  "Generates a key for current context.
The key is used when persisting out the filter setting for
future  use."
  (declare (special  major-mode))
  (or (buffer-file-name)
      (format "%s:%s" (buffer-name) major-mode)))

(defvar emacspeak-table-ui-filter-table (make-hash-table :test 'equal)
  "Stores table filter  settings.")

(defun emacspeak-table-ui-filter-set (key filter)
  "Map filter to key."
  (declare (special emacspeak-table-ui-filter-table))
  (setf (gethash key emacspeak-table-ui-filter-table ) filter))

(defun emacspeak-table-ui-filter-get (key)
  "Lookup key and return corresponding filter. "
  (declare (special emacspeak-table-ui-filter-table))
  (gethash key emacspeak-table-ui-filter-table))

(defun emacspeak-table-ui-filter-load (file)
  "Load saved filter settings."
  (interactive
   (list
    (read-file-name "Load filter settings  from file: "
                    emacspeak-resource-directory
                    ".table-ui-filter")))
  (condition-case nil
      (progn
        (load
         (expand-file-name  file emacspeak-resource-directory)))
    (error (message "Error loading resources from %s "
                    file))))

(defun emacspeak-table-ui-filter-save (file)
  "Save out filter settings."
  (interactive
   (list
    (read-file-name "Save table-ui-filter settings  to file: "
                    emacspeak-resource-directory
                    ".table-ui-filter")))
  (declare (special emacspeak-resource-directory))
  (let ((buffer (find-file-noselect
                 (expand-file-name file
                                   emacspeak-resource-directory))))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (loop for key being the hash-keys of
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
;;; byte-compile-dynamic: nil
;;; end:

;;}}}

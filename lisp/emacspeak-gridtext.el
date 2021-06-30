;;; emacspeak-gridtext.el --- Overlay Grids To filter columnar text  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak module for laying grids on text
;;; Keywords: Emacspeak, gridtext
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
;;;  $Revision: 4150 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1995 -- 2018, T. V. Raman
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Introduction:

;;; Commentary:

;;; Emacspeak's table browsing mode allows one to
;;; efficiently access  content that is tabular in nature.
;;; That module also provides functions for inferring table
;;; structure where possible.
;;; Often, such structure is hard to infer automatically
;;; --but might be known to the user 
;;; e.g. treat columns 1 through 30 as one column of a table
;;; and so on.
;;; This module allows the user to specify a conceptual grid
;;; that is "overlaid" on the region of text to turn it into
;;; a table for tabular browsing. For now, elements of the
;;; grid are "one line" high --but that may change in the
;;; future if necessary. This module is useful for browsing
;;; structured text files and the output from programs that
;;; tabulate their output.
;;; It's also useful for handling multicolumn text.
;;; The "grid" is specified as a list of (start end) tuples..
;;; Code:

;;}}}
;;{{{ required modules

(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'emacspeak-table)
(require 'emacspeak-table-ui)
;;}}}
;;{{{  variables

(defvar emacspeak-gridtext-current-grid nil
  "List that records currently active grid for this
buffer.")

(make-variable-buffer-local 'emacspeak-gridtext-current-grid)

;;}}}
;;{{{  helpers

(defun emacspeak-gridtext-generate-key ()
  "Generates a key for current context.
The key is used when persisting out the grid setting for
future  use."
  (cl-declare (special  major-mode))
  (or (buffer-file-name)
      (format "%s:%s" (buffer-name) major-mode)))

(defun emacspeak-gridtext-vector-region (start end grid)
  "Returns a vector containing the text bounded by start and
end   as specified by grid."
  (let ((result-grid (make-vector (count-lines start end) nil))
        (this-line nil)
        (this-length 0)
        (this-row nil)
        (num-rows (count-lines start end))
        (num-columns(1+  (length grid))))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (if (< start end)
            (goto-char start)
          (goto-char end))
        (cl-loop for i from 0 to (1- num-rows)
                 do
                 (beginning-of-line)
                 (setq this-line
                       (buffer-substring (line-beginning-position) (line-end-position)))
                 (setq this-length (length this-line))
                 (setq this-row (make-vector num-columns ""))
                 (cl-loop for j from 0 to (1- (length grid))
                          do
                          (when (< (1- (nth j grid)) this-length)
;;; within bounds 
                            (aset  this-row j
                                   (substring
                                    this-line
                                    (if (= j 0) 
                                        0
                                      (nth  (1- j) grid))
                                    (1- (nth j grid))))))
                 (aset this-row (length grid)
                       (if (< (nth (1- (length grid)) grid) this-length)
                           (substring this-line
                                      (nth (1- (length grid)) grid))
                         ""))
                 (aset result-grid i this-row)
                 (forward-line 1))
        result-grid))))

;;}}}
;;{{{  persistent store 

(defvar emacspeak-gridtext-table (make-hash-table :test 'equal)
  "Stores grid settings.")

(defun emacspeak-gridtext-set (key grid)
  "Map grid to key."
  (cl-declare (special emacspeak-gridtext-table))
  (setf (gethash key emacspeak-gridtext-table) grid))

(defun emacspeak-gridtext-get (key)
  "Lookup key and return corresponding grid. "
  (cl-declare (special emacspeak-gridtext-table))
  (gethash key emacspeak-gridtext-table))

(defun emacspeak-gridtext-load (file)
  "Load saved grid settings."
  (interactive
   (list
    (read-file-name "Load grid settings  from file: "
                    emacspeak-user-directory
                    ".gridtext")))
  (condition-case nil
      (progn
        (load
         (expand-file-name  file emacspeak-user-directory)))
    (error (message "Error loading resources from %s "
                    file))))

(defun emacspeak-gridtext-save (file)
  "Save out grid settings."
  (interactive
   (list
    (read-file-name "Save gridtext settings  to file: "
                    emacspeak-user-directory
                    ".gridtext")))
  (cl-declare (special emacspeak-user-directory))
  (let ((print-level nil)
        (print-length nil)
        (buffer (find-file-noselect
                 (expand-file-name file
                                   emacspeak-user-directory))))
    (save-current-buffer
      (set-buffer buffer)
      (erase-buffer)
      (cl-loop for key being the hash-keys of
               emacspeak-gridtext-table
               do
               (insert
                (format
                 "\n(setf
 (gethash %s emacspeak-gridtext-table)
 (quote %s))"
                 (prin1-to-string key)
                 (prin1-to-string (emacspeak-gridtext-get
                                   key)))))
      (basic-save-buffer)
      (kill-buffer buffer))))

;;}}}
;;{{{ interactive commands

(defun emacspeak-gridtext-apply (start end grid)
  "Apply grid to region."
  (interactive
   (list
    (point) (mark)
    (read-minibuffer "Specify grid as a list of tuples: "
                     (format "%s" (emacspeak-gridtext-get (emacspeak-gridtext-generate-key))))))
  (let ((grid-table
         (emacspeak-table-make-table
          (emacspeak-gridtext-vector-region start end grid)))
        (buffer (get-buffer-create (format "*%s-grid*" (buffer-name)))))
    (emacspeak-gridtext-set
     (emacspeak-gridtext-generate-key) grid)
    (emacspeak-table-prepare-table-buffer grid-table buffer)))

;;}}}
;;{{{  keymaps 
(defvar emacspeak-gridtext-keymap nil
  "Prefix keymap used by gridtext.")
;;;###autoload
(define-prefix-command  'emacspeak-gridtext 'emacspeak-gridtext-keymap)
(define-key emacspeak-gridtext-keymap "a" 'emacspeak-gridtext-apply)
(define-key emacspeak-gridtext-keymap "l"
  'emacspeak-gridtext-load)
(define-key emacspeak-gridtext-keymap "s" 'emacspeak-gridtext-save)
;;}}}
(provide 'emacspeak-gridtext)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

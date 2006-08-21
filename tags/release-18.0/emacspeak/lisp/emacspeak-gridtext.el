;;; emacspeak-gridtext.el --- gridtext
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak module for laying grids on text
;;; Keywords: Emacspeak, gridtext
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

;;{{{ required modules

(require 'emacspeak-preamble)
(require 'emacspeak-table)
(require 'emacspeak-table-ui)
;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; Emacspeak's table browsing mode allows one to
;;; efficiently access  content that is tabular in nature.
;;; That module also provides functions for infering table
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
  (declare (special  major-mode))
  (or (buffer-file-name)
      (format "%s:%s" (buffer-name) major-mode)))
   
(defun emacspeak-gridtext-vector-region (start end grid)
  "Returns a vector containing the text bounded by start and
end   as specified by grid."
  (let ((result-grid (make-vector (count-lines start end) nil))
        (this-line nil)
        (this-row nil)
        (num-rows (count-lines start end ))
        (num-columns(1+  (length grid))))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (if (< start end )
	    (goto-char start)
	  (goto-char end ))
        (loop for i from 0 to (1- num-rows)
              do
              (beginning-of-line)
              (setq this-line (thing-at-point 'line))
              (setq this-row (make-vector num-columns ""))
              (loop for j from 0 to (1- (length grid))
                    do 
                    (aset  this-row j
                           (substring
                            this-line
                            (if (= j 0 ) 
                                0
			      (nth  (1- j) grid))
                            (1- (nth j grid )))))
              (aset this-row (length grid)
                    (substring this-line
                               (nth (1- (length grid)) grid)))
              (aset result-grid i this-row)
              (forward-line 1))
        result-grid))))

          
;;}}}
;;{{{  persistent store 

(defvar emacspeak-gridtext-table (make-hash-table :test 'equal)
  "Stores grid settings.")

(defun emacspeak-gridtext-set (key grid)
  "Map grid to key."
  (declare (special emacspeak-gridtext-table))
  (setf (gethash key emacspeak-gridtext-table ) grid))

(defun emacspeak-gridtext-get (key)
  "Lookup key and return corresponding grid. "
  (declare (special emacspeak-gridtext-table))
  (gethash key emacspeak-gridtext-table))
;;;###autoload
(defun emacspeak-gridtext-load (file)
  "Load saved grid settings."
  (interactive
   (list
    (read-file-name "Load grid settings  from file: "
                    emacspeak-resource-directory
                    ".gridtext")))
  (condition-case nil
      (progn
	(load
	 (expand-file-name  file emacspeak-resource-directory)))
    (error (message "Error loading resources from %s "
		    file))))
;;;###autoload
(defun emacspeak-gridtext-save (file)
  "Save out grid settings."
  (interactive
   (list
    (read-file-name "Save gridtext settings  to file: "
                    emacspeak-resource-directory
                    ".gridtext")))
  (declare (special emacspeak-resource-directory))
  (let ((buffer (find-file-noselect
                 (expand-file-name file
                                   emacspeak-resource-directory))))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (loop for key being the hash-keys of
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
;;;###autoload
(defun emacspeak-gridtext-apply (start end grid )
  "Apply grid to region."
  (interactive
   (list
    (point)
    (mark)
    (read-minibuffer "Specify grid as a list: "
		     (format "%s" (emacspeak-gridtext-get (emacspeak-gridtext-generate-key))))))
  (let ((grid-table (emacspeak-table-make-table
                     (emacspeak-gridtext-vector-region start
                                                       end
                                                       grid)))
        (buffer (get-buffer-create
                 (format "*%s-grid*"
                         (buffer-name)))))
    (emacspeak-gridtext-set
     (emacspeak-gridtext-generate-key) grid)
    (emacspeak-table-prepare-table-buffer grid-table buffer)))

                         
;;}}}
;;{{{  keymaps 
(defvar emacspeak-gridtext-keymap nil
  "Prefix keymap used by gridtext.")
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
;;; byte-compile-dynamic: t
;;; end:

;;}}}

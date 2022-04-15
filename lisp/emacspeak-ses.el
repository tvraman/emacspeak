;; emacspeak-ses.el --- Speech-enable ses spread-sheet  -*- lexical-binding: t; -*-
;; $Id$
;; $Author: tv.raman.tv $
;; Description:  Emacspeak front-end for SES 
;; Keywords: Emacspeak, ses 
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
;;  $Revision: 4074 $ |
;; Location undetermined
;; 

;;}}}
;;{{{  Copyright:

;; Copyright (C) 1995 -- 2021, T. V. Raman
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
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;{{{  Introduction:
;; Commentary:
;; ses implements a simple spread sheet and is part of Emacs
;; This module speech-enables ses
;; Code:
;;}}}
;;{{{ required modules

;; Code:
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'ses)
;;}}}
;;{{{ emacspeak ses accessors 

;; these additional accessors are defined in terms of the
;; earlier helpers by Emacspeak.
(defun emacspeak-ses-current-cell-symbol ()
  "Return symbol for current cell."
  (or 
   (get-text-property (point) 'cursor-intangible)
   (get-text-property (point) 'intangible)))

(defun emacspeak-ses-current-cell-value ()
  "Return current cell value."
  (cl-declare (special ses--named-cell-hashmap ses--cells))
  (ses-cell-value
   (car (ses-sym-rowcol (emacspeak-ses-current-cell-symbol)))
   (cdr (ses-sym-rowcol (emacspeak-ses-current-cell-symbol)))))

(defun emacspeak-ses-get-cell-value-by-name (cell-name)
  "Return current  value of cell specified by name."
  (cl-declare (special ses--named-cell-hashmap ses--cells))
  (ses-cell-value
   
   (car (ses-sym-rowcol cell-name))
   (cdr (ses-sym-rowcol cell-name))))

;;}}}
;;{{{ emacspeak ses summarizers 

(defun emacspeak-ses-summarize-cell (cell-name)
  "Summarize specified  cell."
  (interactive
   (list
    (read-minibuffer "Cell: ")))
  (cond
   (cell-name
    (dtk-speak
     (format "%s: %s"
             cell-name
             (emacspeak-ses-get-cell-value-by-name cell-name))))
   (t (message "No cell here"))))

(defun emacspeak-ses-summarize-current-cell (&rest _ignore)
  "Summarize current cell."
  (interactive)
  (emacspeak-ses-summarize-cell
   (emacspeak-ses-current-cell-symbol)))

;;}}}
;;{{{ advice internals

;;}}}
;;{{{ new navigation commands 

;; ses uses intangible properties to enable cell navigation
;; here we define navigation primitives that call built-ins and
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
  (forward-line 1)
  (emacspeak-ses-summarize-current-cell))

(defun emacspeak-ses-backward-row-and-summarize ()
  "Move to previous row  and summarize."
  (interactive)
  (forward-line -1)
  (emacspeak-ses-summarize-current-cell))

;;}}}
;;{{{ advice interactive commands

(defun emacspeak-ses-setup ()
  "Setup SES for use with emacspeak."
  (cl-declare (special ses-mode-map))
  )

(defadvice ses-forward-or-insert (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-ses-summarize-current-cell)))

(defadvice ses-recalculate-cell (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-ses-summarize-current-cell)
    (emacspeak-auditory-icon 'task-done)))

(defadvice ses-jump (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-ses-summarize-current-cell)))

;;}}}
;;{{{ Setup:

(emacspeak-ses-setup)

;;}}}

(provide 'emacspeak-ses)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}

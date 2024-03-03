;;; emacspeak-tabulated-list.el --- Speech-enable   -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable TABULATED-LIST 
;;; Keywords: Emacspeak,  Audio Desktop tabulated-list
;;;   LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;;  $Revision: 4532 $ |
;;; Location https://github.com/tvraman/emacspeak
;;;

;;;   Copyright:

;; Copyright (C) 1995 -- 2024, T. V. Raman
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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Commentary:
;;; TABULATED-LIST ==  tabulated list mode
;; Speech-enable tabulated lists and provide commands for intelligent
;; spoken output 

;;; Code:

;;   Required modules:

(eval-when-compile (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;;  Map Faces:

(voice-setup-add-map 
 '(
   (tabulated-list-fake-header voice-bolden)))

;;;  Interactive Commands:
;;;###autoload
(defun emacspeak-tabulated-list-speak-cell ()
  "Speak current cell. "
  (interactive)
  (cl-declare (tabulated-list-format))
  (when (bobp) (error "Beginning  of buffer"))
  (when (eobp) (error "End of buffer"))
  (save-excursion
    (when-let*
        ((name (get-text-property (point) 'tabulated-list-column-name))
         (col
          (cl-position name tabulated-list-format
                       :test #'string= :key #'car))
         (value (elt (tabulated-list-get-entry)  col)))
      (when (= 0 col) (emacspeak-icon 'left))
      (when (= (1- (length tabulated-list-format)) col)
        (emacspeak-icon 'right))
      (when (listp value) (setq value (car value)))
      (when (zerop (length (string-trim value)))
        (dtk-tone 261.6 150 'force))    ;blank
      (if (called-interactively-p 'interactive) 
          (dtk-speak (concat name " " value))
        (dtk-speak  value)))))

(cl-loop
 for f in 
 '(tabulated-list-next-column tabulated-list-previous-column)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-icon 'select-objet)
       (emacspeak-tabulated-list-speak-cell)))))

(defun emacspeak-tabulated-list-next-row ()
  "Move to next row and speak that cell"
  (interactive)
  (let ((col
         (cl-position
          (get-text-property (point) 'tabulated-list-column-name)
          tabulated-list-format
          :test #'string= :key #'car)))
    (forward-line 1)
    (tabulated-list-next-column  col)
    (when-let ((goal (next-single-property-change (point)
                                                  'tabulated-list-column-name)))
      (goto-char goal))
    (emacspeak-tabulated-list-speak-cell)))

(defun emacspeak-tabulated-list-previous-row ()
  "Move to previous row and speak that cell."
  (interactive)
  (let ((col
         (cl-position
          (get-text-property (point) 'tabulated-list-column-name)
          tabulated-list-format
          :test #'string= :key #'car)))
    (forward-line -1)
    (tabulated-list-next-column  col)
    (when-let ((goal (next-single-property-change
                      (point) 'tabulated-list-column-name)))
      (goto-char goal))
    (emacspeak-tabulated-list-speak-cell)))

(defun emacspeak-tabulated-list-setup ()
  "Setup Emacspeak"
  (cl-declare (special tabulated-list-mode-map))
  (cl-loop
   for b in
   '(
     ("<left>" tabulated-list-previous-column)
     ("<right>" tabulated-list-next-column)
     ( "." emacspeak-tabulated-list-speak-cell)
     ("<down>"  emacspeak-tabulated-list-next-row)
     ("<up>" emacspeak-tabulated-list-previous-row)
     ( "M-." emacspeak-tabulated-list-speak-cell)
     ("M-<down>"  emacspeak-tabulated-list-next-row)
     ("M-<up>" emacspeak-tabulated-list-previous-row))
   do
   
   (emacspeak-keymap-update tabulated-list-mode-map b)))

(emacspeak-tabulated-list-setup)

(provide 'emacspeak-tabulated-list)
;;;  end of file

                                        ; 
                                        ; 
                                        ; 


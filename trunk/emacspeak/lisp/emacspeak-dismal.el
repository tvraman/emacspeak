;;; emacspeak-dismal.el --- Speech enable Dismal -- An Emacs Spreadsheet program
;;; $Id$
;;; $Author$ 
;;; Description: spread sheet extension
;;; Keywords:emacspeak, audio interface to emacs spread sheets
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
;;;Copyright (C) 1995 -- 2001, T. V. Raman 
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

;;; emacspeak extensions to the dismal spreadsheet. 
;;; Dismal can be found at ftp://cs.nyu.edu/pub/local/fox/dismal

;;}}}
;;{{{  requires 

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'custom)
(require 'emacspeak-sounds)
(require 'emacspeak-speak)
(require 'emacspeak-keymap)

;;}}}
;;{{{ custom

(defgroup emacspeak-dismal nil
  "Spread-sheet for the Emacspeak Desktop."
  :group 'emacspeak
  :prefix "emacspeak-dismal-")


;;}}}
;;{{{  helper functions:

;;; return cell value as a string

(defsubst emacspeak-dismal-cell-value (row col)
  (let ((value (dismal-get-val row col)))
    (if (floatp value)
        (format "%.2f" value)
      (dismal-convert-cellexpr-to-string value)value)))

(defsubst emacspeak-dismal-current-cell-value ()
  (declare (special dismal-current-row dismal-current-col))
  (emacspeak-dismal-cell-value dismal-current-row dismal-current-col))

;;; return entry in col 0 of current row as a string:

(defsubst emacspeak-dismal-current-row-header ()
  (declare (special dismal-current-row ))
  (dismal-convert-cellexpr-to-string
   (dismal-get-exp dismal-current-row  0)))

(defsubst emacspeak-dismal-current-col-header ()
  (declare (special dismal-current-col ))
  (dismal-convert-cellexpr-to-string
   (dismal-get-exp 0  dismal-current-col)))

;;}}}
;;{{{  Additional interactive commands

(defun emacspeak-dismal-display-cell-expression ()
  "Display the expression in the message area"
  (interactive)
  (declare (special dismal-current-row dismal-current-col))
(dismal-display-current-cell-expr dismal-current-row dismal-current-col))

(defun emacspeak-dismal-display-cell-value ()
  "Display the cell value in the message area"
  (interactive)
  (declare (special dismal-current-cell))
  (message "%s = %s"
           dismal-current-cell
           (emacspeak-dismal-current-cell-value)))

(defun emacspeak-dismal-display-cell-with-row-header ()
  "Displays current cell along with its row header.
The `row header' is the entry in column 0."
  (interactive)
  (declare (special))
(let ((row-head  (emacspeak-dismal-current-row-header))
      (value (emacspeak-dismal-current-cell-value)))
      (message "%s is %s"
               row-head value)))

(defun emacspeak-dismal-display-cell-with-col-header ()
  "Display current cell along with its column header.
The `column header' is the entry in row 0."
  (interactive)
  (let ((col-head  (emacspeak-dismal-current-col-header))
        (value (emacspeak-dismal-current-cell-value)))
    (message "%s is %s"
             col-head value)))

(defun emacspeak-dismal-forward-row-and-summarize (rows)
  "Move forward by arg rows
 (the next row by default)and summarize it."
  (interactive "p")
  (let ((dismal-interactive-p nil))
    (dis-forward-row rows)
    (emacspeak-dismal-row-summarize)))

(defun emacspeak-dismal-backward-row-and-summarize (rows)
  "Move backward by arg rows
 (the previous row by default)and summarize it."
  (interactive "p")
  (let ((dismal-interactive-p nil))
    (dis-backward-row rows)
    (emacspeak-dismal-row-summarize)))

(defun emacspeak-dismal-forward-col-and-summarize (cols)
  "Move forward by arg columns
 (the next column by default)and summarize it."
  (interactive "p")
  (let ((dismal-interactive-p nil))
    (dis-forward-column cols)
    (emacspeak-dismal-col-summarize)))

(defun emacspeak-dismal-backward-col-and-summarize (cols)
  "Move backward by arg columns
 (the previous column by default)and summarize it."
  (interactive "p")
  (let ((dismal-interactive-p nil))
    (dis-backward-column cols)
    (emacspeak-dismal-col-summarize)))

;;}}}
;;{{{  Intelligent summaries

(defvar emacspeak-dismal-sheet-summarizer-list nil
  "Specifies how the entire sheet  should be summarized. ")

(make-variable-buffer-local 'emacspeak-dismal-sheet-summarizer-list)


(defvar emacspeak-dismal-row-summarizer-list nil
  "Specifies how rows should be summarized. ")

(make-variable-buffer-local 'emacspeak-dismal-row-summarizer-list)

(defvar emacspeak-dismal-col-summarizer-list nil
  "Specifies how cols should be summarized. ")

(make-variable-buffer-local 'emacspeak-dismal-col-summarizer-list)

(setq-default emacspeak-dismal-row-summarizer-list nil)
(setq-default emacspeak-dismal-col-summarizer-list nil)
(setq-default emacspeak-dismal-sheet-summarizer-list nil)
(defcustom emacspeak-dismal-value-personality 'paul-animated
  "Personality used for speaking cell values in summaries."
  :group 'emacspeak-dismal
  :type 'symbol)

(defun emacspeak-dismal-row-summarize  ()
  "Summarizes a row using the specification in list
emacspeak-dismal-row-summarizer-list"
  (interactive)
  (declare (special emacspeak-dismal-row-summarizer-list
                    emacspeak-dismal-value-personality
                    voice-lock-mode
                    dismal-current-row))
  (unless  (and  emacspeak-dismal-row-summarizer-list
                 (vectorp emacspeak-dismal-row-summarizer-list))
    (setq emacspeak-dismal-row-summarizer-list
          (read-minibuffer "Specify summarizer as a vector:
" "[")))
  (let ((voice-lock-mode t)
        (summary nil))
    (setq summary 
          (mapconcat
           (function
            (lambda (token)
              (let ((value nil))
                (cond
                 ((stringp token) token)
                 ((numberp token)
                  (setq value (emacspeak-dismal-cell-value
                               dismal-current-row token))
                  (put-text-property  0   (length value)
                                      'personality  emacspeak-dismal-value-personality 
                                      value )
                  value)
                 ((and (listp token)
                       (numberp (first token))
                       (numberp (second token )))
                  (setq value (emacspeak-dismal-cell-value
                               (first token)
                               (second token)))
                  (put-text-property 0   (length value )
                                     'personality emacspeak-dismal-value-personality 
                                     value)
                  value)
                 (t  (format "%s" token))))))
           emacspeak-dismal-row-summarizer-list 
           " "))
    (dtk-speak summary)))

(defun emacspeak-dismal-col-summarize  ()
  "Summarizes a col using the specification in list
emacspeak-dismal-col-summarizer-list"
  (interactive)
  (declare (special emacspeak-dismal-col-summarizer-list
                    emacspeak-dismal-value-personality voice-lock-mode
                    dismal-current-col))
  (unless  (and  emacspeak-dismal-col-summarizer-list
                 (vectorp emacspeak-dismal-col-summarizer-list))
    (setq emacspeak-dismal-col-summarizer-list
          (read-minibuffer "Specify summarizer as a vector:
" "[")))
  (let ((voice-lock-mode t)
        (summary nil))
    (setq summary 
          (mapconcat
           (function
            (lambda (token)
              (let ((value nil))
                (cond
                 ((stringp token) token)
                 ((numberp token)
                  (setq value (emacspeak-dismal-cell-value token
                                                           dismal-current-col))
                  (put-text-property 0 (length value)
                                     'personality
                                     emacspeak-dismal-value-personality value)
                  value)
                 ((and (listp token)
                       (numberp (first token))
                       (numberp (second token )))
                  (setq value (emacspeak-dismal-cell-value
                               (first token)
                               (second token)))
                  (put-text-property 0 (length value)
                                     'personality
                                     emacspeak-dismal-value-personality value)
                  value)
                 (t  (format "%s" token))))))
           emacspeak-dismal-col-summarizer-list 
           " "))
    (dtk-speak summary)))


(defun emacspeak-dismal-sheet-summarize  ()
  "Summarizes a sheet using the specification in list
emacspeak-dismal-sheet-summarizer-list"
  (interactive)
  (declare (special emacspeak-dismal-row-summarizer-list))
  (when emacspeak-dismal-sheet-summarizer-list
    (let ((emacspeak-speak-messages nil))
    (dis-recalculate-matrix))
    (message 
     (mapconcat
      (function
       (lambda (token)
         (cond
          ((stringp token) token)
          ((and (listp token)
                (numberp (first token))
                (numberp (second token )))
           (emacspeak-dismal-cell-value
            (first token)
            (second token)))
          (t  (format "%s" token)))))
      emacspeak-dismal-sheet-summarizer-list 
      " "))))

(defun emacspeak-dismal-set-row-summarizer-list ()
  "Specify or reset row summarizer list."
  (interactive)
  (declare (special emacspeak-dismal-col-summarizer-list))
  (setq emacspeak-dismal-row-summarizer-list
        (read-minibuffer
         "Specify summarizer as a list: "
         (format "%S"
                 (or emacspeak-dismal-row-summarizer-list  "[")))))

(defun emacspeak-dismal-set-col-summarizer-list ()
  "Specify or reset col summarizer list."
  (interactive)
  (declare (special emacspeak-dismal-col-summarizer-list))
  (setq emacspeak-dismal-col-summarizer-list
        (read-minibuffer
         "Specify summarizer as a vector: "
         (format "%S"
                 (or emacspeak-dismal-col-summarizer-list  "[")))))


(defun emacspeak-dismal-set-sheet-summarizer-list ()
  "Specify or reset sheet summarizer list."
  (interactive)
  (declare (special emacspeak-dismal-sheet-summarizer-list))
  (setq emacspeak-dismal-sheet-summarizer-list
        (read-minibuffer
         "Specify summarizer as a list: "
         (format "%S"
                 (or emacspeak-dismal-sheet-summarizer-list  "[")))))

;;}}}
;;{{{  key bindings

;;; record emacspeak stat that we want dismal to save
(defvar emacspeak-dismal-already-customized-dismal nil
  "Records if we have customized dismal.
Checked by emacspeak specific dis-mode-hooks entry.")

(add-hook 'dis-mode-hooks
          (function
           (lambda nil
             (declare (special dismal-saved-variables))
             (unless emacspeak-dismal-already-customized-dismal
               (setq emacspeak-dismal-already-customized-dismal t)
               (push 'emacspeak-dismal-sheet-summarizer-list
                   dismal-saved-variables)
             (push 'emacspeak-dismal-row-summarizer-list
                   dismal-saved-variables)
             (push 'emacspeak-dismal-col-summarizer-list
                   dismal-saved-variables)))))

(declaim (special dismal-map))
(eval-when (load)
  (emacspeak-keymap-remove-emacspeak-edit-commands dismal-map))
;;; this assumes emacspeak-prefix is C-e
;;; and function-key-prefix is M-[
(add-hook 'dis-mode-hooks
          (function
           (lambda nil
             (declare (special dismal-map emacspeak-prefix))
             (local-unset-key "\M-[")
             (local-unset-key emacspeak-prefix)
             (define-key dismal-map (concat emacspeak-prefix "e")
               'dis-last-column)
             (define-key dismal-map  "," 'emacspeak-dismal-display-cell-expression)
             (define-key dismal-map  "."
               'emacspeak-dismal-display-cell-value)
             (define-key dismal-map "R"
               'emacspeak-dismal-display-cell-with-row-header)
             (define-key dismal-map "S"
               'emacspeak-dismal-sheet-summarize)
             (define-key dismal-map "C"
               'emacspeak-dismal-display-cell-with-col-header)
             (define-key dismal-map "\M-m"
               'emacspeak-dismal-row-summarize)
             (define-key dismal-map '[up]
               'emacspeak-dismal-backward-row-and-summarize)
             (define-key dismal-map '[down]
               'emacspeak-dismal-forward-row-and-summarize)
             (define-key dismal-map '[left]
               'emacspeak-dismal-backward-col-and-summarize)
             (define-key dismal-map '[right]
               'emacspeak-dismal-forward-col-and-summarize)
             )))

;;}}}
;;{{{  Advice some commands. 

;;}}}
;;{{{ customize for use with html helper mode
(defadvice dis-html-dump-file (around fix pre act comp)
  "Sets html-helper-build-new-buffer to nil first so we dont
end up building a template page first."

  (let ((html-helper-build-new-buffer nil)) ad-do-it))

(defadvice dis-html-dump-range (around fix pre act comp)
  "Sets html-helper-build-new-buffer to nil first so we dont
end up building a template page first."

  (let ((html-helper-build-new-buffer nil)) ad-do-it))

;;}}}
(provide  'emacspeak-dismal)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

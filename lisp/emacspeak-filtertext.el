;;; emacspeak-filtertext.el --- Utilities to filter text  -*- lexical-binding: t; -*-
;; $Id$
;; $Author: tv.raman.tv $
;; Description:   Implement text filters 
;; Keywords: Emacspeak, Audio Desktop
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

;; Copyright (C) 1995 -- 2021, T. V. Raman<tv.raman.tv@gmail.com>
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

;;{{{ required modules

(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction:
;;; Commentary:
;; It is often useful  to view the results of filtering
;; large amounts of text.;;; Typically you do this with
;; various combinations of grep and friends.
;; When doing so it requires explicit effort to not destroy
;; the original text being filtered.
;; This module provides a textfilter utility that:
;; A) Copies over the selected text to a special filtertext
;; buffer
;; B) Implements a filtertext mode for that buffer that
;; allows easy application of filters
;; C: Provides commands for reverting to the original
;; unfiltered text
;; D: Provides commands for saving results from
;; intermediate filters.
;;; Code:
;;}}}
;;{{{  structures 

(cl-defstruct (emacspeak-filtertext
               (:constructor
                emacspeak-filtertext-constructor))
  text                                  ;original text
  filters                               ;chain of filters applied 
  )

;;}}}
;;{{{ filtertext  mode 

(defvar emacspeak-filtertext-info  nil
  "Holds filtertext info structure.")
(make-variable-buffer-local 'emacspeak-filtertext-info)

(define-derived-mode emacspeak-filtertext-mode text-mode 
  "FilterText mode"
  "Major mode for FilterText interaction. \n\n
\\{emacspeak-filtertext-mode-map}")
(cl-declare (special emacspeak-filtertext-mode-map))
(define-key emacspeak-filtertext-mode-map "=" 'keep-lines)
(define-key emacspeak-filtertext-mode-map "^" 'flush-lines)
(define-key emacspeak-filtertext-mode-map "r"
  'emacspeak-filtertext-revert)

;;}}}
;;{{{ Interactive commands 
;;;###autoload
(defun emacspeak-filtertext(start end)
  "Copy over text in region to special filtertext buffer to  filter text. "
  (interactive "r")
  (cl-declare (special emacspeak-filtertext-info
                       case-fold-search))
  (let ((this (buffer-substring-no-properties start end))
        (buffer (get-buffer-create
                 (format "filter-%s" (buffer-name)))))
    (save-current-buffer
      (set-buffer buffer)
      (setq case-fold-search t)
      (erase-buffer)
      (make-local-variable 'emacspeak-filtertext-info)
      (insert this)
      (emacspeak-filtertext-mode)
      (setq emacspeak-filtertext-info
            (emacspeak-filtertext-constructor :text this))
      (goto-char (point-min)))
    (switch-to-buffer buffer)
    (emacspeak-speak-mode-line)))


(defun emacspeak-filtertext-revert ()
  "Revert to original text."
  (interactive)
  (cl-declare (special emacspeak-filtertext-info))
  (unless (eq  major-mode 'emacspeak-filtertext-mode)
    (error "Not in filter text mode."))
  (when emacspeak-filtertext-info
    (erase-buffer)
    (insert (emacspeak-filtertext-text emacspeak-filtertext-info))
    (emacspeak-auditory-icon 'unmodified-object)
    (message "Reverted filtered text.")))

;;}}}
(provide 'emacspeak-filtertext)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}

;;; emacspeak-filtertext.el --- Utilities to filter text
;;; $Id$
;;; $Author$
;;; Description:   Implement text filters 
;;; Keywords: Emacspeak, Audio Desktop
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

;;; Copyright (C) 1995 -- 2004, T. V. Raman<raman@cs.cornell.edu>
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
;;}}}
;;{{{  Introduction:

;;; It is often useful  to view the results of filtering
;;; large amounts of text.;;; Typically you do this with
;;; various combinations of grep and friends.
;;; When doing so it requires explicit effort to not destroy
;;; the original text being filtered.
;;; This module provides a textfilter utility that:
;;; A) Copies over the selected text to a special filtertext
;;; buffer
;;; B) Implements a filtertext mode for that buffer that
;;; allows easy application of filters
;;; C: Provides commands for reverting to the original
;;; unfiltered text
;;; D: Provides commands for saving results from
;;; intermediate filters.

;;}}}
;;{{{  structures 

(defstruct (emacspeak-filtertext
            (:constructor
             emacspeak-filtertext-constructor))
  text					;original text
  filters				;chain of filters applied 
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
(declare (special emacspeak-filtertext-mode-map))
(define-key emacspeak-filtertext-mode-map "=" 'keep-lines)
(define-key emacspeak-filtertext-mode-map "^" 'flush-lines)
(define-key emacspeak-filtertext-mode-map "r"
  'emacspeak-filtertext-revert )

;;}}}
;;{{{ Interactive commands 
;;;###autoload
(defun emacspeak-filtertext(start end)
  "Copy over text in region to special filtertext buffer in
preparation for interactively filtering text. "
  (interactive "r")
  (declare (special emacspeak-filtertext-info
                    case-fold-search))
  (let ((this (buffer-substring-no-properties start end))
        (buffer (get-buffer-create
                 (format "filter-%s" (buffer-name)))))
    (save-excursion
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
  (declare (special emacspeak-filtertext-info))
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

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}

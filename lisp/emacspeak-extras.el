;;; emacspeak-extras.el --- Speech-enable EXTRAS  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable EXTRAS An Emacs Interface to extras
;;; Keywords: Emacspeak,  Audio Desktop extras
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, 2019, T. V. Raman
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
;;; MERCHANTABILITY or FITNEXTRAS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; Infrequently used wizards archived for posterity.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Keymaps <-> Org (text) Files :

;;; This makes it easy to consolidate personal bindings across machines.
;;; It also protects against custom losing settings due to Custom accidents.
;;;

(defun emacspeak-wizards-bindings-from-org (variable filename)
  "Load bindings from a specified file."
  (interactive "vVariable: \nfFilename: ")
  (let ((bindings nil))
    (with-temp-buffer
      "org-to-map"
      (insert-file-contents filename)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((fields
               (split-string
                (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))
                " " 'omit-nulls)))
          (push
           (list (cl-first fields) (intern (cl-second fields)))
           bindings))
        (forward-line 1)))
    (setq bindings (nreverse (copy-sequence bindings)))
    (set variable  bindings)
    (customize-save-variable variable bindings)))

(defun emacspeak-wizards-bindings-to-org (variable filename)
  "Persists mapping to org file."
  (interactive "vVariable: \nfFilename: ")
  (let ((buffer (find-file-noselect  filename)))
    (with-current-buffer
        buffer
      (goto-char (point-max))
      (cl-loop
       for binding  in (symbol-value variable) do
       (insert (format "%s %s\n" (cl-first binding) (cl-second binding))))
      (save-buffer buffer))
    (switch-to-buffer buffer)))

;;}}}

(provide 'emacspeak-extras)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

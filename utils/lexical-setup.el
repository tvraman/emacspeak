;;; lexical-setup.el --- Setup lexical-binding state:   -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Utility: Easily change lexical-binding setup across files.
;; Keywords: Emacspeak,  Audio Desktop lexical-binding
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;; A speech interface to Emacs |
;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;  $Revision: 4532 $ |
;; Location undetermined
;;

;;}}}
;;{{{  Copyright:
;;Copyright (C) 1995 -- 2007, 2011, T. V. Raman
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
;; MERCHANTABILITY or FITNSELF-DOCUMENT FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;; Easily change state of lexical-binding across all files.

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(require 'files-x)
;;}}}
;;{{{ Variables:

(defconst lexical-setup-files
  (directory-files-recursively
   (expand-file-name "../" (file-name-directory load-file-name))
  ".el$")
"List of files.")

(defun lexical-setup-add-to-files ()
  "Add lexical-binding line to all files in files."
  (declare (special lexical-setup-files))
  (cl-loop
   for f in   lexical-setup-files do
   (message "File: %s" f)
   (let ((buffer (find-file-noselect f)))
     (with-current-buffer buffer
       (modify-file-local-variable-prop-line 'lexical-binding t 'add-or-replace)
       (save-buffer buffer))
     (kill-buffer  buffer))))


(defun lexical-setup-delete-from-files ()
  "Add lexical-binding line to all files in files."
  (declare (special lexical-setup-files))
  (cl-loop
   for f in   lexical-setup-files do
   (let ((buffer (find-file-noselect f)))
     (with-current-buffer buffer
       (modify-file-local-variable-prop-line 'lexical-binding t 'delete)
       (save-buffer buffer))
     (kill-buffer  buffer))))

;;}}}

(provide 'lexical-setup)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; byte-compile-dynamic: t
;; end:

;;}}}

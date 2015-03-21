;;; self-document.el --- Documentation Generator For Emacspeak
;;; $Author: tv.raman.tv $
;;; Description:  Documentation Generator
;;; Keywords: Emacspeak,  Audio Desktop self-document
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
;;;Copyright (C) 1995 -- 2007, 2011, T. V. Raman
;;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
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
;;; MERCHANTABILITY or FITNSELF-DOCUMENT FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:

;;; Generate documentation for Emacspeak command and options.
;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'regexp-opt)

;;}}}
;;{{{ Load All Modules
;;; Setup load-path
(defvar self-document-lisp-directory 
  (expand-file-name "../lisp" (file-name-directory load-file-name))
  "Elisp directory")

(add-to-list
 'load-path
 self-document-lisp-directory)
(defvar self-document-files
  (directory-files  self-document-lisp-directory 'full ".elc$")
  "List of elisp modules  to document.")

(defvar self-document-command-map
  (make-hash-table :test #'equal)
  "Maps modules to commands they define.")

(defun self-document-load-modules ()
  "Load all modules"
  (declare (special self-document-files))
  (load-library "emacspeak-setup")
(load-library "emacspeak-loaddefs")
  (mapc #'load self-document-files))

(defconst self-document-patterns 
  (concat "^"
          (regexp-opt
   '("emacspeak" "cd-tool" "tts" 
   "voice-setup" "dtk" "amixer" )))
  "Patterns to match command names.")

(defconst self-document-advice-patterns 
  (concat "^"
          (regexp-opt '("ad-Advice" "ad-Orig" )))
  "Patterns to match advice generated functions.")

(defsubst self-document-command-p (f)
  "Predicate to check if  this command it to be documented."
  (declare (special self-document-patterns))
  (let ((fn (symbol-name f)))
    (when
        (and (fboundp f) (commandp f)
             (not (string-match self-document-advice-patterns fn))
             (string-match self-document-patterns fn))
      f)))

(defun self-document-map-command (f)
  "Map command to its defining module."
  (declare (special self-document-command-map))
  (when (self-document-command-p f)
    (let* ((file (locate-library (symbol-file f 'defun)))
           (entries (gethash file self-document-command-map)))
      (when (null file) (print f))
;(when (string-match "^emacspeak-bookshare" (symbol-name f)) (print f))
      (cond
       ((null entries)                  ; new
        (puthash file (list f) self-document-command-map))
       (t                               ;Add to entries
        (setq entries (append entries (list f)))
        (puthash  file entries self-document-command-map))))))

(defun self-document-build-command-map()
  "Build a map of module names to commands."
  (mapatoms #'self-document-map-command ))

;;; Simple test:
(defun self-document-load-test ()
  "Dump out command map in /tmp"
  (let ((output (find-file-noselect (make-temp-file "self-command-map"))))
    (self-document-build-command-map)
    (with-current-buffer output 
      (maphash 
       #'(lambda (f cmd-list)
           (insert (format "Module: %s Count: %d\n" f (length cmd-list))))
       self-document-command-map)
      (save-buffer))))

(self-document-load-test)

;;}}}
;;{{{ Document Commands In A Module

;;}}}
;;{{{ Iterate over all modules

                ;;}}}
(provide 'self-document)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

                ;;}}}

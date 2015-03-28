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

(require 'cl-lib)

(require 'regexp-opt)

;;}}}
;;{{{ Load All Modules

;;; Setup load-path
(defvar self-document-lisp-directory
  (expand-file-name "../lisp" (file-name-directory load-file-name))
  "Elisp directory")

(add-to-list 'load-path self-document-lisp-directory)
(add-to-list 'load-path
             (expand-file-name "../../site-lisp"
                               (file-name-directory load-file-name)))

(defvar self-document-files
  (directory-files  self-document-lisp-directory nil ".elc$")
  "List of elisp modules  to document.")

(defvar self-document-map
  (make-hash-table :test #'equal)
  "Maps modules to commands and options they define.")

(cl-defstruct self-document name commands options)

(defun self-document-load-modules ()
  "Load all modules"
  (declare (special self-document-files))
  (package-initialize)
  (load-library "emacspeak-load-path")
  (load-library "emacspeak-setup")
  (cl-loop
   for f in  self-document-files do
   (load-library f)))

(defconst self-document-patterns
  (concat "^"
          (regexp-opt
           '("emacspeak" "cd-tool" "tts"
             "outlout" "dectalk"
             "voice-setup" "dtk" "amixer" )))
  "Patterns to match command names.")
(defvar self-document-command-count 0
  "Global count of commands.")

(defsubst self-document-command-p (f)
  "Predicate to check if  this command it to be documented."
  (declare (special self-document-patterns))
  (let ((fn (symbol-name f)))
    (when (and (fboundp f) (commandp f)
               (string-match self-document-patterns fn))
      (cl-incf self-document-command-count)
      f)))

(defconst self-document-option-pattern
  (concat "^"
          (regexp-opt '("emacspeak" "cd-tool" "dtk" "voice"
                        "amixer" "outloud" "dectalk" "tts")))
  "Pattern that matches options we document.")

(defvar self-document-option-count 0
  "Global count of options.")

(defsubst self-document-option-p (o)
  "Predicate to test if we document this option."
  (declare (special self-document-option-pattern))
  (when (and ; (boundp o)
         (custom-variable-p o)
         (string-match self-document-option-pattern (symbol-name o)))
    4    (cl-incf self-document-option-count)
    o))

(defun self-document-map-command (f)
  "Map this command symbol."
  (declare (special self-document-map))
  (let ((file  (symbol-file f 'defun))
        (entry nil))
    (unless file (setq file "emacspeak"))
    (when file
      (setq file (file-name-sans-extension(file-name-nondirectory file )))
      (when (string-match "loaddefs" file) (setq file "emacspeak"))
      (setq entry  (gethash file self-document-map))
      (unless entry (message "%s: Entry not found for file %s" f file))
      (when entry (push f (self-document-commands  entry))))))

(defun self-document-map-option (f)
  "Map this option symbol."
  (declare (special self-document-map))
  (let ((file  (symbol-file f 'defvar))
        (entry nil))
    (unless file (setq file "emacspeak"))
    (when file
      (setq file (file-name-sans-extension(file-name-nondirectory file)))
      (setq entry  (gethash file self-document-map))
      (when entry (push f (self-document-options  entry))))))

(defun self-document-map-symbol (f)
  "Map command and options to its defining module."
  (declare (special self-document-map))
  (when (self-document-command-p f) (self-document-map-command f))
  (when (self-document-option-p f) (self-document-map-option f)))

(defun self-document-build-map()
  "Build a map of module names to commands."
;;; initialize table
  (cl-loop
   for f in self-document-files do
   (let ((module (file-name-sans-extension f)))
     (puthash module (make-self-document :name module) self-document-map)))
  (mapatoms #'self-document-map-symbol ))

;;; Simple test:
(defun self-document-load-test ()
  "Dump out command map in /tmp"
  (setq debug-on-error t)
  (let ((output (find-file-noselect (make-temp-file "self-command-map")))
        (c-count 0)
        (o-count 0))
    (self-document-load-modules)
    (self-document-build-map)
    (with-current-buffer output
      (insert
       (format "Global Counts: Commands: %d Options: %d\n"
               self-document-command-count self-document-option-count))
      (maphash
       #'(lambda (f self)
           (insert
            (format "\fModule: %s Commands: %d Options: %d\n"
                    f (length (self-document-commands self)) (length (self-document-options self))))
           (unless (zerop (length (self-document-commands self)))
             (insert
              (format "Commands: \n%s\n"
                      (mapconcat #'symbol-name (self-document-commands self) "\n"))))
           (unless (zerop (length (self-document-options self)))
             (insert
              (format "Options: \n%s\n"
                      (mapconcat #'symbol-name (self-document-options self) "\n"))))
           (cl-incf c-count (length (self-document-commands self)))
           (cl-incf o-count (length (self-document-options self))))
       self-document-map)
      (insert (format "Commands: %d Options: %d\n" c-count o-count))
      (save-buffer))))

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

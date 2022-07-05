;;; self-document.el --- Documentation Generator For Emacspeak  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Documentation Generator
;; Keywords: Emacspeak,  Audio Desktop self-document
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

;; Generate documentation for Emacspeak command and options.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(require 'advice)
(require 'lisp-mnt)
(require 'subr-x)
(require 'texnfo-upd)
(require 'regexp-opt)

;;}}}
;;{{{ Load All Modules

;; Setup load-path
(defconst self-document-lisp-directory
  (expand-file-name "../lisp" (file-name-directory load-file-name))
  "Elisp directory")

(add-to-list 'load-path self-document-lisp-directory)
(add-to-list
 'load-path
 (expand-file-name "../../site-lisp" (file-name-directory
                                      load-file-name)))
(load "emacspeak-preamble")
(load "plain-voices")
(load "voice-setup")
(load "emacspeak-loaddefs")
(defconst self-document-files
  (append
   (directory-files self-document-lisp-directory nil "\\.elc$")
   (list "emacspeak-maths.el" "emacspeak-muggles.el"
         "extra-muggles.el" "emacspeak-extras.el"))
  "List of elisp modules  to document.")
(defvar emacspeak-muggles-activate-p t)
(defvar self-document-fn-key
  "<XF86WakeUp>"
  "Notation for Laptop <fn> key.")


(defvar self-document-map
  (make-hash-table :test #'equal)
  "Maps modules to commands and options they define.")

(cl-defstruct self-document name commentary commands options)
(defvar emacspeak-play-emacspeak-startup-icon nil)

(defun self-document-load-modules ()
  "Load all modules"
  (cl-declare (special dtk-program self-document-files
                       emacspeak-auditory-icon-function))
  (let ((file-name-handler-alist nil)
        (load-source-file-function  nil)
        (dtk-program "log-null"))
    (package-initialize)              ; bootstrap emacs package system
;; Bootstrap Emacspeak
    (load-library "emacspeak-setup")
    (setq emacspeak-auditory-icon-function #'ignore)
;; Load all Emacspeak modules:
    (cl-loop
     for f in  self-document-files do
     (unless (string-match "emacspeak-setup" f) ; avoid loading setup twice :
                                        ;(condition-case nil
       (load-library f)
                                        ;(error (message  "check %s" f)))
       ))))

(defconst self-document-patterns
  (concat "^"
          (regexp-opt
           '("amixer" "cd-tool"
             "dectalk" "dtk" "espeak" "mac-"
             "emacspeak" "xbacklight" "light" "extra-muggles"
             "g-"    "gm-" "gmap"  "gweb"
             "ladspa" "soundscape" "outloud" "sox-"   "tts" "voice-")))
  "Patterns to match command names.")

(defvar self-document-command-count 0
  "Global count of commands.")

(defsubst self-document-command-p (f)
  "Predicate to check if  this command it to be documented."
  (cl-declare (special self-document-patterns))
  (when (and (fboundp f) (commandp f)
             (string-match self-document-patterns (symbol-name f)) ; candidate
             (if  (string-match  "/" (symbol-name f)) ; filter repeat muggles
                 (string-match "/body$" (symbol-name f))
               f))
    (cl-incf self-document-command-count)
    f))

(defvar self-document-option-count 0
  "Global count of options.")

(defsubst self-document-option-p (o)
  "Predicate to test if we document this option."
  (cl-declare (special self-document-patterns))
  (when (and
         (custom-variable-p o)
         (string-match self-document-patterns (symbol-name o)))
    (cl-incf self-document-option-count)
    o))

(defun self-document-map-command (f)
  "Add   this command symbol to our map."
  (cl-declare (special self-document-map))
  (let ((file  (symbol-file f 'defun))
        (entry nil))
    (unless file (setq file "emacspeak")) ; capture orphans if any 
    (when file
      (setq file (file-name-sans-extension(file-name-nondirectory file )))
      (when (string-match "loaddefs" file) (setq file "emacspeak"))
      (setq entry  (gethash file self-document-map))
      (unless entry (message "%s: Entry not found for file %s" f file))
      (when entry (push f (self-document-commands  entry))))))

(defun self-document-map-option (f)
  "Add this option symbol to our map."
  (cl-declare (special self-document-map))
  (let ((file  (symbol-file f 'defvar))
        (entry nil))
    (unless file (setq file "emacspeak")); capture orphans if any
    (when (string-match "loaddefs" file) (setq file "emacspeak"))
    (when file
      (setq file (file-name-sans-extension(file-name-nondirectory file)))
      (setq entry  (gethash file self-document-map))
      (when entry (push f (self-document-options  entry))))))

(defun self-document-map-symbol (f)
  "Map command and options to its defining module."
  (cl-declare (special self-document-map))
  (when (self-document-command-p f) (self-document-map-command f))
  (when (self-document-option-p f) (self-document-map-option f)))

(defun sd-cleanup-commentary (commentary )
  "Cleanup commentary."
  (with-temp-buffer
    (insert commentary)
    (goto-char (point-min))
    (flush-lines "{{{")
    (goto-char (point-min))
    (flush-lines "}}}")
    (goto-char (point-min))
    (delete-blank-lines)
    (goto-char (point-min))
    (while (re-search-forward "^;+ ?" nil t)
      (replace-match "" nil nil))
    (buffer-string)))

(defun sd-get-commentary (name)
  "Get commentary for named module"
  (let* ((lib (locate-library name))
         (lmc (lm-commentary
               (if (string-match ".el$" lib)
                   lib
                 (substring lib 0 -1)))))
    (if lmc
        (setq lmc (sd-cleanup-commentary lmc)))))

;; initialize table
(defun self-document-build-map()
  "Build a map of module names to commands and options."
  (let ((file-name-handler-alist nil))
  (cl-loop
   for f in self-document-files do
   (let ((module (file-name-sans-extension f)))
     (puthash module
              (make-self-document :name module
                                  :commentary (sd-get-commentary module))
              self-document-map)))
  (mapatoms #'self-document-map-symbol )))

;;}}}
;;{{{ Document Commands In A Module

(defun sd-texinfo-escape (string)
  "Escape texinfo special chars"
  (when string
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward "[{}@]" nil t)
        (replace-match "@\\&"))
      (buffer-string))))

(defun self-document-module-preamble (self)
  "Generate preamble for module documentation."
  (let ((name (self-document-name self))
        (file-name-handler-alist nil)
        (lmc (self-document-commentary self)))
    (insert (format "\n@node %s\n@section %s\n\n\n" name name))
    (insert (format "\n\n%s\n\n"
                    (or lmc
                        (format "### %s: No Commentary\n" name))))))

(defun self-document-option (o)
  "Document this option."
  (let ((doc (sd-texinfo-escape
              (documentation-property  o 'variable-documentation)))
        (value (symbol-value  o)))
    (insert (format "\n\n@defvar {User Option} %s\n" o))
    (insert (format "%s\n"
                    (or doc
                        (format "###%s: Not Documented\n" o))))
    (insert (format "\nDefault Value: @code{@verb{|%s|}}\n" value))
    (insert "\n@end defvar\n\n")))

(defun self-document-module-options (self)
  "Document options for this module."
  (let ((name (self-document-name self))
        (file-name-handler-alist nil)
        (options  nil))
    (insert (format "@subsection %s Options\n\n" name))
    (setq options
          (sort
           (self-document-options self)
           #'(lambda (a b) (string-lessp (symbol-name a) (symbol-name b)))))
    (mapc #'self-document-option options)))

(defun self-document-command (c)
  "Document this command."
  (let ((key (where-is-internal c))
        (keys nil))
    (insert
     (format "\n\n@subsubsection %s\n@deffn {Command} %s  %s\n"
             c c 
             (or (help-function-arglist c t) " ")))
    (when key
      (setq keys (mapcar #'sd-texinfo-escape (mapcar #'key-description key )))
      (insert "@table @kbd\n")
      (cl-loop for k in keys do 
               (insert (format "@item %s\n" k))
               (insert (format "@kindex %s\n" k)))
      (insert "@end table\n\n"))
    (insert (format "@findex %s\n\n" c))
    (insert
     (if
         (documentation c)
         (format "@format\n%s\n@end format"
                 (sd-texinfo-escape (documentation c)))
       (format "### %s: Not Documented\n" c)))
    (insert "\n@end deffn\n\n")))

(defun self-document-module-commands (self)
  "Document commands for this module."
  (let ((name (self-document-name self))
        (file-name-handler-alist nil)
        (commands  nil))
    (insert (format "@subsection %s Commands\n\n" (capitalize name)))
    (setq commands
          (sort
           (self-document-commands self)
           #'(lambda (a b) (string-lessp (symbol-name a) (symbol-name b)))))
    (mapc #'self-document-command commands)))

(defun self-document-module (self)
  "Generate documentation for commands and options in a module."
  (let ((file-name-handler-alist nil))
;; Only generate in non-degenerate case
  (when (or (self-document-commentary self)
            (not  (zerop (length (self-document-commands self))))
            (not  (zerop (length (self-document-options self)))))
    (self-document-module-preamble self)
    (when (self-document-commands self) (self-document-module-commands self))
    (when (self-document-options self)(self-document-module-options self)))))

;;}}}
;;{{{ Iterate over all modules

(declare-function emacspeak-url-template-generate-texinfo-documentation (buffer))
(defun self-document-fix-quotes ()
  "Fix UTF8 curved quotes since makeinfo doesn't handle them well."
  (goto-char (point-min))
  (while
      (search-forward (format "%c" 8216) (point-max) 'no-error)
    (replace-match "``"))
  (goto-char (point-min))
  (while
      (search-forward (format "%c" 8217) (point-max) 'no-error)
    (replace-match "''")))


(defun self-document-fix-fn-key ()
  "Change <XF86WakeUp> to <fn>."
  (goto-char (point-min))
  (while
      (search-forward self-document-fn-key (point-max) 'no-error)
    (replace-match "<fn>")))
  
(defun self-document-update-menu-entries ()
  "Locates master menu, and updates description for each node."
  (message "Adding descriptions to master menu entries.")
  (save-excursion
    (goto-char  (point-min))
    (goto-char (re-search-forward "^@menu"))
    (forward-line 1)
    (while (not (looking-at "^@end menu"))
      (goto-char (line-beginning-position))
      (forward-char 2)
      (when-let* ((module (sexp-at-point))
                 (summary (lm-summary (locate-library (format "%s.el" module)))))
        (goto-char (line-end-position))
        (insert (format "%s." summary)))
      (forward-line 1))))
(defun self-document-all-modules()
  "Generate documentation for all modules."
  (cl-declare (special self-document-map))
  (let ((file-name-handler-alist nil)
        (output (find-file-noselect "docs.texi"))
        (keys nil))
    (self-document-load-modules)
    (self-document-build-map)
    (setq keys
          (sort
           (cl-loop for k being  the hash-keys of self-document-map collect k)
           #'string-lessp))
    (with-current-buffer output
      (erase-buffer)
      (texinfo-mode)
      (insert
       (format
        "@node Emacspeak Commands And Options \n
@chapter Emacspeak Commands And Options \n\n
@include intro-docs.texi\n\n
This chapter documents a total of %d commands and %d options.\n\n"
        self-document-command-count self-document-option-count ))
      (cl-loop
       for k in keys do
       (self-document-module (gethash k self-document-map)))
      (emacspeak-url-template-generate-texinfo-documentation (current-buffer))
      (texinfo-all-menus-update)
      (self-document-update-menu-entries)
      (flush-lines "^Commentary: *$" (point-min) (point-max))
      (self-document-fix-fn-key)
      (shell-command-on-region          ; squeeze blanks
       (point-min) (point-max)
       "cat -s" (current-buffer) 'replace)
      (save-buffer))))

;;}}}
;;{{{ Document all keybindings:

(defun sd-sort-keymap (key-entries)
  "Safely sort and return keymap entries."
  (let ((temp (copy-sequence key-entries)))
  (cl-sort
   temp
   #'(lambda (a b)
       (when (and (characterp (car a)) (characterp (car b)))
       (string-lessp
        (key-description (format "%c" (car a)))
        (key-description (format "%c" (car b)))))))))

(defvar self-document-keymap-list
  '(
    emacspeak-keymap emacspeak-dtk-submap
    emacspeak-hyper-keymap emacspeak-super-keymap emacspeak-alt-keymap
    emacspeak-personal-keymap emacspeak-personal-ctlx-keymap
    )
"List of keymaps that we document.")

(defun self-document-keymap (keymap)
  "Output Texinfo documentation for bindings in keymap."
  (cl-assert  (keymapp keymap) t "Not a valid keymap: %s")
  (let ((entries (sd-sort-keymap (cdr (copy-keymap keymap )))))
    (insert "@table @kbd\n")
    (cl-loop for binding in
          entries
          when (and (characterp (car binding))
                    (not (keymapp  (cdr binding))))
          do 
          (insert
           (format "@item %s\n %s\n\n"
                   (sd-texinfo-escape (key-description (format "%c" (car binding))))
                   (cdr binding))))
    (insert "@end table\n")))


(defun self-document-all-keymaps()
  "Generate documentation for all Emacspeak keymaps."
  (cl-declare (special self-document-keymap-list))
  (let ((output (find-file-noselect "keys.texi"))
        (title nil))
    (with-current-buffer output
      (erase-buffer)
      (texinfo-mode)
      (cl-loop
       for keymap in self-document-keymap-list do
       (setq title (format "Emacspeak Keybindings from %s" (symbol-name keymap)))
       (insert (format "\n@node %s\n @section %s\n\n" title title))
       (self-document-keymap (symbol-value keymap)))
      (shell-command-on-region          ; squeeze blanks
       (point-min) (point-max)
       "cat -s" (current-buffer) 'replace)
      (save-buffer))))

;;}}}
;;{{{ Tests:

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
            (format
             "\fModule: %s Commands: %d Options: %d\n"
             f
             (length (self-document-commands self))
             (length (self-document-options self))))
           (unless (zerop (length (self-document-commands self)))
             (insert
              (format "Commands: \n%s\n"
                      (mapconcat #'symbol-name (self-document-commands self)
                                 "\n"))))
           (unless (zerop (length (self-document-options self)))
             (insert
              (format "Options: \n%s\n"
                      (mapconcat #'symbol-name (self-document-options self)
                                 "\n"))))
           (cl-incf c-count (length (self-document-commands self)))
           (cl-incf o-count (length (self-document-options self))))
       self-document-map)
      (insert (format "Commands: %d Options: %d\n" c-count o-count))
      (save-buffer))))

(defun self-document-module-test ()
  "Test documentation generator."
  (cl-declare (special self-document-map))
  (setq debug-on-error t)
  (let ((output (find-file-noselect (make-temp-file "doc" nil ".texi"))))
    (self-document-load-modules)
    (self-document-build-map)
    (with-current-buffer output
      (insert
       (format
        "@node Emacspeak Commands And Options \n
@chapter Emacspeak Commands And Options \n\n
This chapter documents a total of %d commands and %d options.\n\n"
        self-document-command-count self-document-option-count ))
      (cl-loop
       for v being the hash-values of self-document-map  do
       (self-document-module v))
      (save-buffer))))

;;}}}
(provide 'self-document)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}

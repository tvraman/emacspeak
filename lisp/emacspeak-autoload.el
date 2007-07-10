;;; emacspeak-autoload.el --- Emacspeak Autoload Generator
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  autoload Wizard for the emacspeak desktop
;;; Keywords: Emacspeak,  Audio Desktop autoload
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
;;;Copyright (C) 1995 -- 2007, T. V. Raman
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; generate autoloads for emacspeak

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'autoload)
;;}}}
;;{{{ Variables

(declaim (special emacspeak-lisp-directory))
(defvar emacspeak-auto-autoloads-file
  (expand-file-name "emacspeak-loaddefs.el" emacspeak-lisp-directory)
  "File that holds automatically generated autoloads for
Emacspeak.")

(defvar emacspeak-auto-custom-file
  (expand-file-name "cus-load.el" emacspeak-lisp-directory)
  "File that holds automatically generated custom dependencies for
Emacspeak.")

;;}}}
;;{{{ generate autoloadms

(defvar emacspeak-update-autoloads-from-directories
  (cond
   ((fboundp 'update-autoloads-from-directories)
    'update-autoloads-from-directories)
   ((fboundp  'update-directory-autoloads)
    'update-directory-autoloads))
  "Function used to extract autoloads.")

(defun emacspeak-auto-generate-autoloads ()
  "Generate emacspeak autoloads."
  (declare (special  emacspeak-auto-autoloads-file
                     emacspeak-update-autoloads-from-directories emacspeak-lisp-directory))
  (let ((dtk-quiet t)
        (generated-autoload-file emacspeak-auto-autoloads-file))
    (funcall emacspeak-update-autoloads-from-directories
             emacspeak-lisp-directory)
    (funcall emacspeak-update-autoloads-from-directories
             (expand-file-name "g-client" emacspeak-lisp-directory))
    ))

;;}}}
;;{{{ custom dependencies:

(defun emacspeak-auto-custom-make-dependencies ()
  "Generate emacspeak custom deps."
  (declare (special  emacspeak-auto-custom-file))
  (let ((dtk-quiet t)
        (generated-custom-dependencies-file emacspeak-auto-custom-file))
    (custom-make-dependencies)))
                                                

;;}}}
(provide 'emacspeak-autoload)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}

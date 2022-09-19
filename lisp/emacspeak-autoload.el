;;; emacspeak-autoload.el --- Autoload Generator  -*- lexical-binding: t; -*-
;;
;; $Author: tv.raman.tv $
;; Description:  autoload Wizard for the emacspeak desktop
;; Keywords: Emacspeak,  Audio Desktop autoload
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; 
;;  $Revision: 4532 $ |
;; Location undetermined
;; 

;;}}}
;;{{{ Copyright:
;; Copyright (C) 1995 -- 2022, T. V. Raman Copyright
;;(c) 1994, 1995 by Digital Equipment Corporation.  All Rights
;;Reserved.  This file is not part of GNU Emacs, but the same
;;permissions apply.  GNU Emacs is free software; you can redistribute
;;it and/or modify it under the terms of the GNU General Public
;;License as published by the Free Software Foundation; either version
;;2, or (at your option) any later version.  GNU Emacs is distributed
;;in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;;without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;;A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;details.  You should have received a copy of the GNU General Public
;;License along with GNU Emacs; see the file COPYING.  If not, write
;;to the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;;Boston,MA 02110-1301, USA.

;;}}}

;;; Commentary:
;; generate autoloads for emacspeak
;;; Code:

(require 'cl-lib)
(defvar emacspeak-auto-autoloads-file
  (expand-file-name "emacspeak-loaddefs.el"
                    (file-name-directory load-file-name))
  "File that holds automatically generated autoloads for Emacspeak.")

(defun emacspeak-auto-generate-autoloads ()
  "Generate emacspeak autoloads."
  (cond
   ((locate-library "loaddefs-gen")     ; emacs 29
    (loaddefs-generate emacspeak-lisp-directory "emacspeak-loaddefs.el"))
   (t (require 'autoload)
      (let ((dtk-quiet t)
            (generated-autoload-file emacspeak-auto-autoloads-file))
        (update-directory-autoloads emacspeak-lisp-directory)))))

(provide 'emacspeak-autoload)
;;{{{ end of file
;; local variables:
;; folded-file: t
;; end:

;;}}}

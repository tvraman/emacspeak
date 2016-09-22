;;; webean.el --- run net.emacspeak.web.Client in beanshell  -*- lexical-binding: t; -*-
;;; $Id: webean.el 6333 2009-10-19 15:21:20Z tv.raman.tv $
;;; $Author: tv.raman.tv $ 
;;; Description: Launch net.emacspeak.web.Client from inside BeanShell
;;; Keywords: headless Web, BeanShell, Java
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; webhead| T. V. Raman |raman@cs.cornell.edu 
;;; Emacs on  on  a headless Web
;;; $Date: 2007-09-01 15:30:13 -0700 (Sat, 01 Sep 2007) $ |
;;;  $Revision: 4532 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (c) 2009, T. V. Raman
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

;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))

;;}}}
;;{{{  Introduction

;;; FRun net.emacspeak.web.Client in a BeanShell to help easy
;;interactive testing.

;;}}}
;;{{{ locations and pathnames 
(defvar webhead-directory
  (expand-file-name "../" (file-name-directory load-file-name))
  "Directory where webhead is installed.")
(defsubst webhead-classpath ()
  "Builds webhead classpath."
  (format "%s:%s:%s"
  (mapconcat
   #'identity
   (directory-files
              (expand-file-name "java/lib" webhead-directory)
              'full
              ".jar")
":")
  "/usr/share/java/bsh.jar"
  (expand-file-name "java/build/webhead.jar"
                    webhead-directory)))

(defun webean ()
  "Launch a Bean Shell with web client loaded."
  (interactive)
  (let ((wb (make-comint "WeBean"
                "java" nil
                "-cp" (webhead-classpath) "bsh.Interpreter")))
      (process-send-string
       (get-buffer-process wb)
       (format "source(\"%s\");"
               (expand-file-name "java/bsh/commands/wep.bsh"
                                 webhead-directory)))
  (switch-to-buffer wb)))
  


  

;;}}}
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

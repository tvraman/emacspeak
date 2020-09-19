;;; emacspeak-load-path.el -- Setup Emacs load-path for compiling Emacspeak  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Sets up load-path and env for emacspeak compilation and installation
;;; Keywords: Emacspeak, Speech extension for Emacs
;;{{{  LCD Archive entry:
;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2018, T. V. Raman
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
(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(setq byte-compile-warnings t)
(setq ad-redefinition-action 'accept)

(defvar emacspeak-directory
  (expand-file-name "../" (file-name-directory load-file-name))
  "Directory where emacspeak is installed. ")

(defvar emacspeak-lisp-directory
  (expand-file-name "lisp/" emacspeak-directory)
  "Directory containing lisp files for  Emacspeak.")

(unless (member emacspeak-lisp-directory load-path)
  (setq load-path
        (cons emacspeak-lisp-directory load-path)))

(defvar emacspeak-resource-directory (expand-file-name "~/.emacspeak")
  "Directory where Emacspeak resource files such as pronunciation dictionaries are stored. ")

;;{{{ Interactive Check Implementation:

;;; Notes:
;;; This implementation below appears to work for  emacspeak.
;;; See http://tvraman.github.io/emacspeak/blog/ems-interactive-p.html
;;; ems-interactive-p is reserved for use within Emacspeak advice.

(defvar ems-called-interactively-p nil
  "Flag recording interactive calls to functions adviced by Emacspeak.")

(defun ems-record-interactive-p (f)
  "Predicate to test if we need to record interactive calls of
this function. Memoizes result for future use by placing a
property 'emacspeak on the function."
  (cond
   ((not (symbolp f)) nil)
   ((get f 'emacspeak) t) ; already memoized 
   ((ad-find-some-advice f 'any  "emacspeak");emacspeak advice present
    (put f 'emacspeak t)); memoize and return t
   (t nil)))

(defadvice funcall-interactively (around emacspeak  pre act comp)
  "Set emacspeak  interactive flag if there is an advice."
  (let ((ems-called-interactively-p ems-called-interactively-p)) ; save state 
    (when (ems-record-interactive-p (ad-get-arg 0))
      (setq ems-called-interactively-p (ad-get-arg 0)))
    ad-do-it))

(defadvice call-interactively (around emacspeak  pre act comp)
  "Set emacspeak  interactive flag if there is an advice."
  (let ((ems-called-interactively-p ems-called-interactively-p))
    (when (ems-record-interactive-p (ad-get-arg 0))
      (setq ems-called-interactively-p (ad-get-arg 0)))
    ad-do-it))

(defsubst ems-interactive-p ()
  "Check our interactive flag.
Return T if set and we are called from the advice for the current
interactive command. Turn off the flag once used."
  (when ems-called-interactively-p      ; interactive call
    (let ((caller (cl-second (backtrace-frame 1))) ; containing function name
          (caller-advice ; advice wrapper of containing function
           (ad-get-advice-info-field ems-called-interactively-p  'advicefunname))
          (result nil))
                                        ; T if called from our advice
      (setq result (eq caller caller-advice))
      (when result
        (setq ems-called-interactively-p nil) ; turn off now that we used  it
        result))))

(defsubst ems-debug-interactive-p ()
  "Check our interactive flag.
Return T if set and we are called from the advice for the current
interactive command. Turn off the flag once used."
  (message "Debug: %s" ems-called-interactively-p)
  (when ems-called-interactively-p      ; interactive call
    (let ((caller (cl-second (backtrace-frame 1)))
          (caller-advice (ad-get-advice-info-field ems-called-interactively-p  'advicefunname))
          (result nil))
      (setq result (or (eq caller caller-advice) ; called from our advice
                       (eq ems-called-interactively-p caller) ; call-interactively call
                       ))
      (message "this: %s caller: %s caller-advice %s
  ems-called-interactively-p %s"
               this-command caller caller-advice ems-called-interactively-p)
      (when result
        (setq ems-called-interactively-p nil) ; turn off now that we used  it
        result))))

;;}}}
;;{{{defsubst: ems--fastload:

;;; Internal function  used to efficiently load files.

(defsubst ems--fastload (file)
  "Load file efficiently."
  (let ((file-name-handler-alist nil)
         (load-source-file-function nil)
         (inhibit-message t))
    (load file)))


;;}}}
(provide 'emacspeak-load-path)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

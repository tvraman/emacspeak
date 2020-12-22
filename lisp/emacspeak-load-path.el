;;; emacspeak-load-path.el -- Setup Emacs load-path for compiling Emacspeak  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Sets up load-path and env for emacspeak compilation and installation
;;; Keywords: Emacspeak, Speech extension for Emacs
;;{{{  LCD Archive entry:
;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
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
(require 'advice)
(setq ad-redefinition-action 'accept)

(cl-pushnew (file-name-directory load-file-name) load-path :test #'string-equal)
;;{{{acss-structure:
(cl-defstruct  acss
  family
  gain left-volume right-volume
  average-pitch pitch-range stress richness
  punctuations)

;;}}}
;;{{{ Interactive Check Implementation:

;;; Notes:
;;; This implementation below appears to work for  emacspeak.
;;; See http://tvraman.github.io/emacspeak/blog/ems-interactive-p.html
;;; ems-interactive-p is reserved for use within Emacspeak advice.

(defvar ems-called-interactively-p nil
  "Record interactive calls to adviced functions.")

(defun ems-record-interactive-p (f)
  "Predicate to test if we  record interactive calls.
 Memoizes result  via property 'emacspeak."
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
;;;###autoload
(defsubst ems-interactive-p ()
  "Check  interactive flag.
Return T if set and we are called from the advice for the current
 command. Turn off the flag once used."
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

;;}}}
;;{{{defsubst: ems--fastload:

;;; Internal function  used to efficiently load files.

(defsubst ems--fastload (file)
  "Load file efficiently."
  (let ((file-name-handler-alist nil)
         (load-source-file-function nil))
    (load file)))

;;}}}
(provide 'emacspeak-load-path)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

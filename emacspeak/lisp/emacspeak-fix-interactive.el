;;; emacspeak-fix-interactive.el --- Tools to make  Emacs' builtin prompts   speak
;;; $Id$
;;; $Author$ 
;;; Description: Fixes functions that use interactive to prompt for args.
;;; Approach suggested by hans@cs.buffalo.edu
;;; Keywords: Emacspeak, Advice, Automatic advice, Interactive
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision$ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2000, T. V. Raman 
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

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'advice)
(require 'dtk-speak)
;;{{{  Introduction:

;;; Emacs commands that use the 'interactive spec
;;; to read interactive arguments are a problem for Emacspeak.
;;;  This is because the prompting for the arguments is done from C
;;; See (callint.c) in the Emacs sources.
;;; Advicing the various input functions,
;;; e.g. read-file-name therefore will not help.
;;; This module defines a function that solves this problem.
;;; emacspeak-fix-commands-that-use-interactive needs to be called
;;; To speech enable such functions. 

;;}}}
;;{{{  functions that are  fixed. 

(defvar emacspeak-interactive-functions-that-are-fixed 
  nil
  "Functions which have been  adviced automatically to make their
interactive prompts speak. ")
(defvar emacspeak-fix-interactive-dont-fix-regexp 
  (concat 
   "^ad-Orig\\|^mouse\\|^scroll-bar"
   "\\|^face\\|^frame\\|^font"
   "\\|^color\\|^timer")
  "Regular expression matching function names whose interactive spec should not be fixed.")

(defsubst emacspeak-should-i-fix-interactive-p (sym)
  "Predicate to test if this function should be fixed. "
  (declare (special emacspeak-xemacs-p
                    emacspeak-interactive-functions-that-are-fixed 
                    emacspeak-fix-interactive-dont-fix-regexp))
  (unless emacspeak-xemacs-p
      ; we dont need to fix interactive commands for xemacs.
    (save-match-data 
      (let ((interactive-string nil )
            (dont-fix-regexp emacspeak-fix-interactive-dont-fix-regexp))
        (and (fboundp  sym)
             (commandp sym)
             (not
              (memq  sym emacspeak-interactive-functions-that-are-fixed))
             (not (string-match dont-fix-regexp (symbol-name sym )))
             (stringp
              (setq interactive-string
                    (second (ad-interactive-form (symbol-function sym ))))))))))

(defun emacspeak-split-interactive-string-on-newline(string)
  "Helper function used to split the interactive string. "
  (let ((start 0) (end nil) (split nil))
    (save-match-data 
      (while
          (setq end 
                (string-match  "\n" string start  ))
        (setq split
              (cons (substring  string  start end ) split ))
        (setq start (+ 1 end ))))
    (when (< start (length string ))
      (setq split
            (cons (substring string start ) split )))
    (setq split (nreverse split )))) 

(defun emacspeak-fix-commands-that-use-interactive ()
  "Returns a list of symbols whose function definition
uses interactive to prompt for args
after fixing them. "
  (declare (special emacspeak-interactive-functions-that-are-fixed ))
  (mapatoms
   (function
    (lambda (sym)
      (when (and
             (emacspeak-should-i-fix-interactive-p sym)
             (emacspeak-fix-interactive sym))
        (push sym emacspeak-interactive-functions-that-are-fixed ))))))
  

;;{{{  Understanding aid 
(defun emacspeak-show-auto-advice-for-interactive (sym)
  "Show the auto advice that will be genrated 
by emacspeak to make 
function definition of sym to make its interactive form speak its prompts. "
  (let ((interactive-list
         (split-string
          (second (ad-interactive-form (symbol-function sym )))
          "\n")))
                                        ; advice if necessary
    (when
        (some
         (function
          (lambda (prompt)
            (not
             (or
              (save-match-data 
                (string-match  "^\\*?[pPr]" prompt ))
              (string= "*" prompt )))))
         interactive-list )
      (progn
        (`
         (defadvice (, sym) (before  emacspeak-auto activate  )
           "Automatically defined advice to speak interactive prompts. "
           (interactive
            (nconc  
             (,@
              (mapcar
               (function 
                (lambda (prompt)
                  (` (let
                         ((dtk-stop-immediately nil)
                          (emacspeak-last-command-needs-minibuffer-spoken t)
                          (emacspeak-speak-messages nil))
                       (tts-with-punctuations "all"
                                              (dtk-speak
                                               (,
                                                (format " %s "
                                                        (or
                                                         (if (= ?* (aref  prompt 0))
                                                             (substring prompt 2 )
                                                           (substring prompt 1 ))
                                                         "")))))
                       (call-interactively
                        '(lambda (&rest args)
                           (interactive (, prompt))
                           args) nil)))))
               interactive-list))))))))))


;;}}}

(defun emacspeak-fix-interactive (sym)
  "Fix the function definition of sym to make its interactive form speak its prompts. "
  (let ((interactive-list
         (split-string
          (second (ad-interactive-form (symbol-function sym )))
          "\n")))
                                        ; advice if necessary
    (when
        (some
         (function
          (lambda (prompt)
            (not
             (or
              (save-match-data 
                (string-match  "^\\*?[pPr]" prompt ))
              (string= "*" prompt )))))
         interactive-list )
      (eval
       (`
        (defadvice (, sym) (before  emacspeak-auto activate  )
          "Automatically defined advice to speak interactive prompts. "
          (interactive
           (nconc  
            (,@
             (mapcar
              (function 
               (lambda (prompt)
                 (` (let
                        ((dtk-stop-immediately nil)
                         (emacspeak-last-command-needs-minibuffer-spoken t)
                         (emacspeak-speak-messages nil))
                      (tts-with-punctuations "all"
                                             (dtk-speak
                                              (,
                                               (format " %s "
                                                       (or
                                                        (if (= ?* (aref  prompt 0))
                                                            (substring prompt 2 )
                                                          (substring prompt 1 ))
                                                        "")))))
                      (call-interactively
                       '(lambda (&rest args)
                          (interactive (, prompt))
                          args) nil)))))
              interactive-list))))))))))

;;; inline function for use from other modules:
(defvar emacspeak-last-command-needs-minibuffer-spoken nil 
  "Used to signal to minibuffer that the contents need to be
spoken.")

(defsubst  emacspeak-fix-interactive-command-if-necessary (command)
(declare (special emacspeak-interactive-functions-that-are-fixed))
(and (emacspeak-should-i-fix-interactive-p command)
(emacspeak-fix-interactive command)
(push command  emacspeak-interactive-functions-that-are-fixed )))

;;}}}
(provide 'emacspeak-fix-interactive)

;;{{{  end of file
;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

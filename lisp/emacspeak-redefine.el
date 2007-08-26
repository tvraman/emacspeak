;;; emacspeak-redefine.el --- Redefines some key Emacs builtins to speak
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak's redefinition of some key functions.
;;; Emacspeak does most of its work by advising other functions to speak.
;;; This module contains those functions that have to be explicitly redefined.
;;; Keywords: Emacspeak, Redefine, Spoken Output
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date$ |
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
;;{{{  Introduction:

;;; This module redefines a few vital functions,
;;; since advising them won't help.
;;; Convention used:
;;; To redefine function fn:
;;; The original function will be renamed to Orig-fn.
;;; A new function called emacspeak-fn will be defined.
;;;  Finally, we will fset fn to emacspeak-fn
;;; In the case of backward-char, forward-char, and self-insert-command
;;; mere redefinition of the function will not do:
;;; We will need to bind the new functions explicitly to the keys.

;;}}}
;;{{{ requires
(require 'emacspeak-preamble)

;;}}}
;;{{{  How to redefine and restore a function:

(defun emacspeak-redefine (function-name )
  "Redefines function-name to its emacspeak version. "
  (let ((save-name (intern (format "Orig-%s" function-name )))
        (new-name (intern (format "emacspeak-%s" function-name ))))
    (fset   save-name (symbol-function  function-name ))
    (fset function-name new-name )))

(defun emacspeak-undo-redefinition (function-name)
  "Undo the effect of having called emacs-redefine on function-name. "
  (let ((restore-name (intern (format "Orig-%s" function-name ))))
    (fset function-name (symbol-function restore-name ))))

;;}}}
;;{{{  The new functions:

(defun emacspeak-self-insert-command (&optional arg)
  "Insert a character.
Speaks the character if emacspeak-character-echo is true.
See  command emacspeak-toggle-word-echo bound to
\\[emacspeak-toggle-word-echo].
Toggle variable dtk-stop-immediately-while-typing if you want to have
speech flush as you type."
  (interactive "p")
  (declare (special last-command-event dtk-stop-immediately-while-typing
                    buffer-undo-list  buffer-read-only
                    emacspeak-character-echo emacspeak-word-echo))
  (or arg (setq arg 1))
  (when buffer-read-only
    (signal 'buffer-read-only (list (current-buffer))))
  (and (listp buffer-undo-list)
       (null (car buffer-undo-list))
       (pop buffer-undo-list ))
  (self-insert-command  arg )
  (when (interactive-p)
    (cond
     ((and emacspeak-word-echo
           (= (char-syntax last-command-event )32 ))
      (save-excursion
        (condition-case nil
            (forward-word -1)
          (error nil))
        (emacspeak-speak-word)))
     (emacspeak-character-echo
      (when dtk-stop-immediately-while-typing (dtk-stop))
      (emacspeak-speak-this-char last-command-event ))))
  (and auto-fill-function
       (= (char-syntax  last-command-event) 32)
       (>= (current-column) fill-column)
       (funcall auto-fill-function)))

(defun emacspeak-forward-char (&optional arg)
  "Forward-char redefined to speak char moved to. "
  (interactive "p")
  (declare (special dtk-stop-immediately))
  (or arg (setq arg 1))
  (cond
   ((<= (+ arg (point)) (point-max))
    (forward-char arg)
    (when (interactive-p)
      (and dtk-stop-immediately (dtk-stop))
      (emacspeak-speak-char t  )))
   (t(ding)
     (message "End of buffer"))))

(defun emacspeak-backward-char (&optional arg)
  "Backward-char redefined to speak char moved to. "
  (interactive "p")
  (or arg (setq arg 1))
  (declare (special dtk-stop-immediately))
  (cond
   ((>= (- (point) arg) (point-min))
    (backward-char arg)
    (when (interactive-p)
      (and dtk-stop-immediately (dtk-stop))
      (emacspeak-speak-char t )))
   (t (ding)
      (message "Beginning of buffer"))))

;;}}}
;;{{{  Rebinding functions to keys:

(defun emacspeak-rebind(old-fn new-fn &optional keymap)
  "Rebinds new-fn to all those keys that normally invoke old-fn"
  (let ((keys (where-is-internal old-fn keymap)))
    (mapcar
     (if keymap
         #'(lambda (key)
             (define-key keymap  key new-fn ))
       #'(lambda (key)
           (global-set-key key new-fn )))
     keys )))

(defvar emacspeak-functions-that-bypass-function-cell
  (list 'backward-char 'forward-char 'self-insert-command )
  "These commands are activated directly through C,
rather than through their function cell.
They have to be redefined and rebound to make them talk. " )

(mapcar
 #'(lambda (f)
     (emacspeak-rebind f
                       (intern (format "emacspeak-%s" f ))))
 emacspeak-functions-that-bypass-function-cell )

;;}}}
(provide 'emacspeak-redefine)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

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
;;;  $Revision$ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2002, T. V. Raman 
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
(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-speak)
(require 'emacspeak-sounds)

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
;;{{{  How to redefine and restore a function: 

(defun emacspeak-redefine (function-name )
  "Redefines function-name to its emacspeak version. "
  (let
      ((save-name (intern (format "Orig-%s" function-name )))
       (new-name (intern (format "emacspeak-%s" function-name ))))
    (fset   save-name (symbol-function  function-name ))
    (fset function-name new-name ))
  )

(defun emacspeak-undo-redefinition (function-name)
  "Undo the effect of having called emacs-redefine on function-name. "
  (let
      ((restore-name (intern (format "Orig-%s" function-name ))))
    (fset function-name (symbol-function restore-name )))
  )

;;}}}
;;{{{  The new functions: 

(defun emacspeak-self-insert-command (arg)
  "Insert a character.
Speaks the character if emacspeak-character-echo is true.
See  command emacspeak-toggle-word-echo bound to
\\[emacspeak-toggle-word-echo].
Toggle variable dtk-stop-immediately-while-typing if you want to have
speech flush as you type."
  (interactive "p")
  (declare (special last-input-char
                    dtk-stop-immediately-while-typing dtk-program 
                    buffer-undo-list  buffer-read-only
                    emacspeak-character-echo
                    emacspeak-word-echo))
  (when buffer-read-only
    (signal 'buffer-read-only
            (list (current-buffer))))
  (unless (car buffer-undo-list)
    (pop buffer-undo-list ))
  (self-insert-command  arg )
  (cond
   ((and emacspeak-word-echo
         (interactive-p)
	 (= last-input-char 32 ))
    (save-excursion
      (condition-case nil
          (forward-word -1)
        (error nil))
      (emacspeak-speak-word)))
   ((and emacspeak-character-echo
         (interactive-p ))
    (when dtk-stop-immediately-while-typing (dtk-stop))
    (emacspeak-speak-this-char last-input-char )))
  (and
   (= (char-syntax  last-input-char) 32)
   (>= (current-column) fill-column)
   auto-fill-function
   (funcall auto-fill-function)))

(defun emacspeak-forward-char (arg)
  "Forward-char redefined to speak char moved to. "
  (interactive "p")
  (declare (special dtk-stop-immediately))
  (cond
   ((<= (+ arg (point)) (point-max))
    (forward-char arg)
    (when (interactive-p)
      (and dtk-stop-immediately (dtk-stop ))
      (emacspeak-speak-char t )))
   (t(ding)
     (message "End of buffer"))))

(defun emacspeak-backward-char (arg)
  "Backward-char redefined to speak char moved to. "
  (interactive "p")
  (declare (special dtk-stop-immediately))
  (cond
   ((>= (- (point) arg) (point-min))
    (backward-char arg)
    (when (interactive-p)
      (and dtk-stop-immediately (dtk-stop ))
      (emacspeak-speak-char t )))
   (t (ding)
      (message "Beginning of buffer"))))



;;{{{  kill buffer for emacs 21


;;;There is a bug in emacs 21
;;; that causes the normal emacspeak advice  to fire too late
;;; Interestingly enough  if you just bind the key again 
;;; the emacspeak advice works, which indicates that perhaps 
;;; the bug stems from how it gets bound in commands.c
;;; also, note that emacspeak-fix-interactive gets fooled by emacs 21
;;; into auto-advising kill-buffer

(defun emacspeak-kill-buffer (buffer)
  "Speech-enabled version of kill-buffer for Emacs 21."
  (interactive
   (list 
    (read-buffer "Kill buffer: "
                 (current-buffer))))
  (kill-buffer buffer)
  (emacspeak-auditory-icon 'close-object)
  (emacspeak-speak-mode-line))


;;}}}

;;}}}
;;{{{  Rebinding functions to keys:

(defun emacspeak-rebind(old-fn new-fn)
  "Rebinds new-fn to all those keys that normally invoke old-fn"
  (let
      ((keys (where-is-internal old-fn)))
    (mapcar 
     (function
      (lambda (key)
        (global-set-key key new-fn )))
     keys ))
  )

(defvar emacspeak-functions-that-bypass-function-cell 
  (list 'backward-char 'forward-char 'self-insert-command )
  "These commands are activated directly through C,
rather than through their function cell.
They have to be redefined and rebound to make them talk. " )
;;; for emacs 21 add kill-buffer to the above list:
(declaim (special emacs-version))

(when (string-match "^21" emacs-version)
  (push 'kill-buffer 
emacspeak-functions-that-bypass-function-cell))
(mapcar 
 (function
  (lambda (f)
    (emacspeak-rebind f
                      (intern (format "emacspeak-%s" f )))))
 emacspeak-functions-that-bypass-function-cell )

;;}}}
;;{{{  fix ding 

(when (subrp (symbol-function 'ding))
  (fset 'orig-ding (symbol-function 'ding))
  (defun ding ( &optional arg)
    "Beep, or flash the screen.
Also, unless an argument is given,
terminate any keyboard macro currently executing.
Additionally, emacspeak sets this up to play an auditory icon. "
    (emacspeak-auditory-icon 'warn-user)
    (orig-ding arg)))

;;}}}
(provide 'emacspeak-redefine)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

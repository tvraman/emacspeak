;;; emacspeak-redefine.el --- Redefines some key Emacs builtins to speak  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak's redefinition of some key functions.
;;; Emacspeak does most of its work by advising other functions to speak.
;;; This module contains those functions that have to be explicitly redefined.
;;; Keywords: Emacspeak, Redefine, Spoken Output
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2008-06-21 19:19:09 -0700 (Sat, 21 Jun 2008) $ |
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;{{{  Introduction:
;;; Commentary:
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
;;; Code:
;;}}}
;;{{{ requires
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{  How to redefine and restore a function:

(defun emacspeak-redefine (function-name)
  "Redefines function-name to its emacspeak version. "
  (let ((save-name (intern (format "Orig-%s" function-name)))
        (new-name (intern (format "emacspeak-%s" function-name))))
    (fset   save-name (symbol-function  function-name))
    (fset function-name new-name)))

(defun emacspeak-undo-redefinition (function-name)
  "Undo the effect of having called emacs-redefine on function-name. "
  (let ((restore-name (intern (format "Orig-%s" function-name))))
    (fset function-name (symbol-function restore-name))))

;;}}}
;;{{{  The new functions:
;;;###autoload
(defun emacspeak-self-insert-command (&optional arg)
  "Insert a character.
Speaks the character if emacspeak-character-echo is true.
See  command emacspeak-toggle-word-echo bound to
\\[emacspeak-toggle-word-echo].
Speech flushes as you type."
  (interactive "p")
  (cl-declare (special last-command-event buffer-undo-list  buffer-read-only
                       emacspeak-character-echo emacspeak-word-echo))
  (or arg (setq arg 1))
  (when buffer-read-only
    (signal 'buffer-read-only (list (current-buffer))))
  (and (listp buffer-undo-list)
       (null (car buffer-undo-list))
       (pop buffer-undo-list))
  (self-insert-command  arg)
  (when (called-interactively-p 'interactive)
    (let ((display (get-char-property (1- (point)) 'display)))
      (dtk-stop)
      (cond
       ((stringp display) (dtk-say display))
       ((and emacspeak-word-echo
             (= (char-syntax last-command-event)32))
        (save-excursion
          (condition-case nil
              (forward-word -1)
            (error nil))
          (emacspeak-speak-word)))
       (emacspeak-character-echo
        (emacspeak-speak-this-char (preceding-char))))))
  (and auto-fill-function
       (= (char-syntax  last-command-event) 32)
       (>= (current-column) fill-column)
       (funcall auto-fill-function)))

(defun emacspeak-post-self-insert-hook ()
  "Speaks the character if emacspeak-character-echo is true.
See  command emacspeak-toggle-word-echo bound to
\\[emacspeak-toggle-word-echo].
Speech flushes as you type."
  (cl-declare (special last-command-event 
                       emacspeak-character-echo emacspeak-word-echo))
  (when buffer-read-only (dtk-speak "Buffer is read-only. "))
  (when
      (and (eq (preceding-char) last-command-event) ; Sanity check.
           (not executing-kbd-macro)
           (not noninteractive))
    (let ((display (get-char-property (1- (point)) 'display)))
      (dtk-stop)
      (cond
       ((stringp display) (dtk-say display))
       ((and emacspeak-word-echo
             (= (char-syntax last-command-event)32))
        (save-excursion
          (condition-case nil
              (forward-word -1)
            (error nil))
          (emacspeak-speak-word)))
       (emacspeak-character-echo
        (emacspeak-speak-this-char (preceding-char)))))))
(when (<= 24 emacs-major-version)  
  (add-hook 'post-self-insert-hook
            'emacspeak-post-self-insert-hook)
  (unless (boundp 'command-error-function)
    (defadvice self-insert-command (before emacspeak pre act comp)
      "Provide feedback for read-only context."
      (when (and (ems-interactive-p)
                 (or buffer-read-only
                     (get-text-property (point)  'read-only)))
        (dtk-speak "Text is read-only")))))
;;;###autoload
(defun emacspeak-forward-char (&optional arg)
  "Forward-char redefined to speak char moved to. "
  (interactive "p")
  (cl-declare (special dtk-stop-immediately))
  (or arg (setq arg 1))
  (cond
   ((<= (+ arg (point)) (point-max))
    (forward-char arg)
    (when (called-interactively-p 'interactive)
      (and dtk-stop-immediately (dtk-stop))
      (emacspeak-speak-char t)))
   (t(ding)
     (message "End of buffer"))))

(defun emacspeak-backward-char (&optional arg)
  "Backward-char redefined to speak char moved to. "
  (interactive "p")
  (or arg (setq arg 1))
  (cl-declare (special dtk-stop-immediately))
  (cond
   ((>= (- (point) arg) (point-min))
    (backward-char arg)
    (when (called-interactively-p 'interactive)
      (and dtk-stop-immediately (dtk-stop))
      (emacspeak-speak-char t)))
   (t (ding)
      (message "Beginning of buffer"))))

;;}}}
;;{{{  Rebinding functions to keys:

(defun emacspeak-rebind(old-fn new-fn &optional keymap)
  "Rebinds new-fn to all those keys that normally invoke old-fn"
  (let ((keys (where-is-internal old-fn  keymap)))
    (mapcar
     (if keymap
         #'(lambda (key)
             (define-key keymap  key new-fn))
       #'(lambda (key)
           (global-set-key key new-fn)))
     keys)))
;;; self-insert-command is removed since we can use
;;; post-self-insert-hook

(defvar emacspeak-functions-that-bypass-function-cell nil
  "These commands are activated directly through C,
rather than through their function cell.
They have to be redefined and rebound to make them talk in versions older than Emacs 24. ")

(unless (<=   24 emacs-major-version)
  (push 'backward-char emacspeak-functions-that-bypass-function-cell)
  (push 'forward-char emacspeak-functions-that-bypass-function-cell)
  (push 'self-insert-command emacspeak-functions-that-bypass-function-cell))

(mapcar
 #'(lambda (f)
     (emacspeak-rebind f
                       (intern (format "emacspeak-%s" f))))
 emacspeak-functions-that-bypass-function-cell)

;;}}}
(provide 'emacspeak-redefine)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

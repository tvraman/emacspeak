;;; emacspeak-todo-mode.el --- speech-enable todo-mode
;;; $Id$
;;; $Author$
;;; Description: todo-mode  for maintaining todo lists 
;;; Keywords: Emacspeak, todo-mode 
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

;;; Copyright (c) 1995 -- 2002, T. V. Raman
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

(eval-when-compile (require 'cl)
                   (require 'backquote))
(declaim  (optimize  (safety 0) (speed 3)))
(eval-when (compile)
  (require 'emacspeak-fix-interactive))
(require 'emacspeak-sounds)
(require 'emacspeak-speak)
(require 'advice)

;;}}}
;;{{{ Introduction:

;;; todo-mode (part of Emacs 21) provides todo-lists that can be
;;; integrated with the Emacs calendar.
;;; This module speech-enables tood-mode

;;}}}
;;{{{  Advice interactive commands:

(defvar emacspeak-todo-mode-navigation-commands
  '(todo-forward-item
    todo-backward-item
    todo-forward-category
    todo-backward-category
    todo-jump-to-category
    )
  "Todo mode navigation commands to speech enable.")

(loop for f in emacspeak-todo-mode-navigation-commands
      do
      (eval
       (`
        (defadvice (, f) (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (interactive-p)
            (emacspeak-auditory-icon 'select-object)
            (emacspeak-speak-line))))))

(defadvice todo-save (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'save-object)))

(defadvice todo-quit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

;;}}}
(provide 'emacspeak-todo-mode)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

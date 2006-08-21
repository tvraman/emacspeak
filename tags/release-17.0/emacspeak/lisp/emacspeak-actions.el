;;; emacspeak-actions.el --- Emacspeak actions -- callbacks that can be associated with portions of a buffer
;;; $Id$
;;; $Author$
;;; Define emacspeak actions for various modes
;;; Keywords:emacspeak, audio interface to emacs actions
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
;;; Copyright (c) 1995 by T. V. Raman
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


;;; Commentary:
;;{{{  Introduction:

;;; Define mode-specific  actions.
;;; Actions are defined by adding them to hook
;;; emacspeak-<mode-name>-actions-hook

;;}}}
;;{{{  required modules 

;;; Code:
(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-speak)
(require 'emacspeak-sounds)

;;}}}
;;{{{  Define actions for emacs lisp mode

(defun emacspeak-activate-match-blinker ()
  "Setup action on right parens.
The defined   emacspeak action   causes
emacspeak to show the matching paren when the cursor moves across a right paren."
  (save-excursion
    (goto-char (point-min))
    (ems-modify-buffer-safely
     (while (search-forward ")" nil t )
       (put-text-property  (point) (1+ (point))
			   'emacspeak-action
			   'emacspeak-blink-matching-open )))))
(add-hook 'emacspeak-emacs-lisp-mode-actions-hook
          'emacspeak-activate-match-blinker )
;;}}}
;;{{{  Define actions for c and c++ modes

(defun emacspeak-c-speak-semantics-when-on-closing-brace ()
  "Setup action on right braces.
The defined  action    causes
emacspeak to speak the semantics of the line
 when the cursor moves across a right brace."
  (save-excursion
    (goto-char (point-min))
    (ems-modify-buffer-safely
     (while (search-forward "}" nil t )
       (put-text-property  (point) (1+ (point))
                           'emacspeak-action
                           'emacspeak-c-speak-semantics )))))
(add-hook 'emacspeak-c-mode-actions-hook
	  'emacspeak-c-speak-semantics-when-on-closing-brace)

;;}}}
(provide  'emacspeak-actions)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

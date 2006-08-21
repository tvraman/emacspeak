;;; emacspeak-ispell.el --- Speech enable Ispell -- Emacs' interactive spell checker
;;; $Id$
;;; $Author$ 
;;; Description:  Emacspeak extension to speech enable flyspell
;;; Keywords: Emacspeak, Ispell, Spoken Output, fly spell checking
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
(require 'custom)
(require 'advice)
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
;;{{{  Introduction:

;;; Commentary:

;;; This module speech enables flyspell.




;;}}}
;;{{{  define personalities

(defgroup emacspeak-flyspell nil
  "Emacspeak support for on the fly spell checking."
  :group 'emacspeak
  :group 'flyspell
  :prefix "emacspeak-flyspell-")

(defcustom emacspeak-flyspell-highlight-personality 'harry
  "Voice used to highlight spelling errors. "
  :type 'symbol
:group 'emacspeak-flyspell)

;;}}}
;;{{{ advice
(declaim (special flyspell-delayed-commands))
(push 'emacspeak-self-insert-command flyspell-delayed-commands)
(defadvice flyspell-auto-correct-word (around emacspeak pre act comp)
  "Speak the correction we inserted"
  (cond
   ((interactive-p)
    ad-do-it
    (dtk-speak (car  (flyspell-get-word nil)))
    (emacspeak-auditory-icon 'select-object))
   (t ad-do-it))
  ad-return-value)

(defadvice flyspell-unhighlight-at (before debug pre act comp)

  (let ((overlay-list (overlays-at pos))
(o nil))
    (while overlay-list 
(setq o (car overlay-list))
          (when (flyspell-overlay-p o)
          (put-text-property (overlay-start o)
                             (overlay-end o)
                             'personality  nil))
(setq overlay-list (cdr overlay-list)))))
;;}}}
;;{{{  Highlighting the error 

(defun emacspeak-flyspell-highlight-incorrect-word (beg end)
  "Put property personality with value
`emacspeak-flyspell-highlight-personality' from beg to end"
  (declare (special emacspeak-flyspell-highlight-personality))
  (ems-modify-buffer-safely
   (put-text-property beg end 'personality
                      emacspeak-flyspell-highlight-personality))
  (emacspeak-speak-region beg end))

(add-hook 'flyspell-incorrect-hook 'emacspeak-flyspell-highlight-incorrect-word)

;;}}}
(provide 'emacspeak-flyspell)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

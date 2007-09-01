;;; emacspeak-find-func.el --- Speech enable emacs' code finder
;;; $Id$
;;; $Author$ 
;;; Description:  Emacspeak extension to speech enable find-func
;;; Keywords: Emacspeak, find-func
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

;;; Commentary:

;;; This module speech enables find-func

;;}}}
;;{{{ requires
(require 'emacspeak-preamble)

;;}}}
;;{{{ advice
(defvar emacspeak-find-func-commands
  '(find-function
    find-function-at-point
    find-variable
    find-variable-at-point
    find-function-on-key)
  "Commands to speech enable")

(loop for f in emacspeak-find-func-commands
      do
      (eval
       `(defadvice ,f  (after emacspeak pre act comp)
          "Speak current line"
          (when (interactive-p)
            (emacspeak-auditory-icon 'open-object)
            (emacspeak-dtk-sync)
            (dtk-speak
             (format "Found %s" (ad-get-arg 0)))))))

;;}}}

(provide 'emacspeak-find-func)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

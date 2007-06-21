;;; emacspeak-moz.el.el --- Talk to Firefox via MozRepl
;;; $Id: moz.el 4532 2007-05-04 01:13:44Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Control Firefox from Emacs
;;; Keywords: Emacspeak,  Audio Desktop Firefox
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
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

;;{{{  introduction

;;; Commentary:


;;; MozRepl provides a read-eval-print loop into Firefox
;;; This module provides convenient functions for driving MozRepl
;;; See http://repo.hyperstruct.net/mozlab

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'browse-url)

;;}}}
;;{{{ Customizations

(defgroup emacspeak-moz nil
  "Control Firefox from Emacs."
  :group 'emacspeak)

(define-prefix-command 'emacspeak-moz-prefix-command 'emacspeak-moz-keymap )
(global-set-key "\C-x@hf" 'emacspeak-moz-prefix-command)
(loop for k in
      '(
        ("e" emacspeak-moz-eval-expression-and-go)
        ("i" inferior-moz-switch-to-mozilla)
        ("f" browse-url-firefox))
      do
      (emacspeak-keymap-update  emacspeak-moz-keymap k))



;;}}}
;;{{{ Interactive commands:

(defun emacspeak-moz-eval-expression-and-go (exp)
  "Send expression to Moz and switch to it."
  (interactive "sJSEval: ")
  (comint-send-string (inferior-moz-process) exp)
  (switch-to-buffer (process-buffer (inferior-moz-process)))
  (emacspeak-auditory-icon 'select-object)
  (emacspeak-speak-line))

;;}}}
;;{{{ Advice interactive commands:

(defadvice inferior-moz-switch-to-mozilla (after emacspeak pre
                                                 act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

;;}}}
(provide 'emacspeak-moz)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

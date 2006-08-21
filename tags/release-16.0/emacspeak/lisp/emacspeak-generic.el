;;; emacspeak-generic.el --- Speech enable  generic modes
;;; $Id$
;;; $Author$
;;; Description:   extension to speech enable generic 
;;; Keywords: Emacspeak, Audio Desktop
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

;;; Copyright (C) 1995 -- 2002, T. V. Raman<raman@cs.cornell.edu>
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

;;{{{ required modules

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; This module speech-enables generic.el so that modes
;;; defined using  define-generic-mode get voice locking
;;; support. Examples include apache-generic-mode and
;;; friends defined in generic-x.el

;;; Code:

;;}}}
;;{{{ voice locking 

(defvar emacspeak-generic-voice-lock-expressions nil
  "Records voice lock expressions for a specific generic
mode. ")

(make-variable-buffer-local 'emacspeak-generic-voice-lock-expressions)

(defun emacspeak-generic-voice-lock-setup (keywords expressions)
  "Set up voice-lock functionality for generic mode."
  (declare (special voice-lock-defaults))
  (let ((generic-voice-lock-expressions))
    ;; Keywords
    (when  keywords
      (setq generic-voice-lock-expressions
            (list
             (let ((regexp (regexp-opt keywords)))
               (list (concat "\\<\\(" regexp "\\)\\>")
                     1
                     'voice-lock-keyword-personality)))))
    ;; Other voice-lock expressions
    (when expressions
         (setq generic-voice-lock-expressions
               (append expressions
                generic-voice-lock-expressions)))
(setq emacspeak-generic-voice-lock-expressions  generic-voice-lock-expressions)
    (when (or expressions keywords)
         (make-local-variable 'voice-lock-defaults)
	 (setq voice-lock-defaults
               '(emacspeak-generic-voice-lock-expressions nil)))))

(defadvice generic-mode-set-font-lock (after emacspeak pre act com)
  (emacspeak-generic-voice-lock-setup (ad-get-arg 0)
                                      (ad-get-arg 1)))
  

;;}}}
;;{{{  generic setup 

(defadvice generic-mode-with-type (after emacspeak pre act
                                         comp)
  "Setup emacspeak extensions. "
  
  (emacspeak-setup-programming-mode)
  (voice-lock-mode 1))

;;}}}
(provide 'emacspeak-generic)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

;;; emacspeak-jde.el --- Speech enable JDE -- An integrated Java Development Environment
;;; $Id$
;;; $Author$ 
;;; Description: Auditory interface to JDE
;;; Keywords: Emacspeak, Speak, Spoken Output, Java
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

;;; Copyright (c) 1997 by T. V. Raman  
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

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-speak)
(require 'emacspeak-keymap)
(require 'emacspeak-sounds)
(require 'voice-lock)

(eval-when (compile)
  (require 'emacspeak-fix-interactive))

;;}}}
;;{{{  Introduction

;;;Speech enable Java IDE.
;;; The Java IDE --JDE-- can be found at 
;;;http://sunsite.auc.dk/jde/

;;}}}
;;{{{ Advice interactive commands:

(defadvice jde-compile (after emacspeak pre act comp)
"Provide auditory feedback"
(when (interactive-p)
(emacspeak-auditory-icon 'select-object)
(dtk-speak "Compiling current java project")))

(defadvice jde-run (after emacspeak pre act comp)
"Provide auditory feedback"
(when (interactive-p)
(emacspeak-auditory-icon 'select-object)
(emacspeak-speak-mode-line)))

(defadvice jde-db (after emacspeak pre act comp)
"Provide auditory feedback"
(when (interactive-p)
(emacspeak-auditory-icon 'select-object)
(emacspeak-speak-mode-line)))

;;}}}
;;{{{  fix interactive prompts

(loop for command in 
      (list
       'jde-db-set-app-args
       'jde-db-set-args
       'jde-db-set-debugger
       'jde-db-set-source-paths
       'jde-gen-class-buffer
       'jde-gen-console-buffer
       'jde-menu
       'jde-run-applet
       'jde-run-set-app
       'jde-run-set-app-args
       'jde-run-set-applet-doc
       'jde-run-set-applet-viewer
       'jde-run-set-args
       'jde-run-set-vm
       'jde-run-set-vm-w
       'jde-set-classpath
       'jde-set-compile-options
       'jde-set-compiler
       'jde-set-global-classpath)
do
(emacspeak-fix-interactive-command-if-necessary command))

;;}}}
;;{{{ voice lock 

(declaim (special voice-lock-defaults-alist))
(if (not (assq 'jde-mode voice-lock-defaults-alist))
      (setq voice-lock-defaults-alist
	    (cons
	     (cons 'jde-mode

		   ;; jde-mode-defaults
		   '((java-voice-lock-keywords java-voice-lock-keywords-1
		      java-voice-lock-keywords-2 java-voice-lock-keywords-3)
		     nil nil ((?_ . "w") (?$ . "w")) nil
		     (voice-lock-mark-block-function . mark-defun)))

	     voice-lock-defaults-alist)))

;;}}}
(provide 'emacspeak-jde )
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

;;; emacspeak-eclim.el --- Speech-enable ECLIM: emacs/eclipse integration
;;; $Id: emacspeak-eclim.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable ECLIM An Emacs Interface to eclim: eclipse/emacs integration    
;;; Keywords: Emacspeak,  Audio Desktop, eclim: Emacs/Eclipse
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
;;;Copyright (C) 1995 -- 2007, 2011, T. V. Raman
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
;;; MERCHANTABILITY or FITNECLIM FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; ECLIM ==  Eclipse/Vim integration.
;;; http://www.eclim.org turns Eclipse into a headless server that can be called from other programs.
;;; Package Emacs-Eclim connects Emacs to Eclim.
;;; Package emacspeak-eclim speech-enables emacs-eclim.
;;;

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ eclim-ant.el

(defadvice eclim-ant-clear-cache (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)))
(defadvice eclim-ant-validate (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)))

(defadvice eclim-ant-run (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)))

;;}}}
;;{{{ eclim-completion.el

;;}}}
;;{{{ eclimd.el

;;}}}
;;{{{ eclim.el

;;}}}
;;{{{ eclim-java.el

;;}}}
;;{{{ eclim-maven.el

;;}}}
;;{{{ eclim-problems.el

;;}}}
;;{{{  eclim-project.el

;;}}}
(provide 'emacspeak-eclim)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

;;; emacspeak-company.el --- Speech-enable COMPANY-mode
;;; $Id: emacspeak-company.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable COMPANY An Emacs Interface to company
;;; Keywords: Emacspeak,  Audio Desktop company
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
;;; MERCHANTABILITY or FITNCOMPANY FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; COMPANY -mode: Complete Anything Support for emacs.
;;; This module provides an Emacspeak Company Front-end,
;;; And advices the needed interactive commands in Company.

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(eval-when-compile (require 'company "company" 'no-error))
;;}}}
;;{{{ Customizations:

;;}}}
;;{{{ Helpers:
(defsubst ems-company-current ()
  "Helper: Return current selection in company."
  (declare (special  company-selection company-candidates))
  (nth company-selection company-candidates))

(defun emacspeak-company-speak-this ()
  "Formatting rule for speaking company selection."
  (let ((metadata (funcall 'company-fetch-metadata)))
    (when metadata (ems-voiceify-string metadata 'voice-annotate))
    (dtk-speak
     (concat (ems-company-current) " " metadata))))    

;;}}}
;;{{{ Emacspeak Front-End For Company:
(defun emacspeak-company-frontend (command)
  "Emacspeak front-end for Company."
  (case command
    (pre-command (emacspeak-company-speak-this))
    (post-command (emacspeak-company-speak-this))
    (hide nil)))

;;}}}
;;{{{ Advice Interactive Commands:

(defadvice company-complete-selection (before emacspeak pre act comp)
  "Speak the selection."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (dtk-speak (ems-company-current))))
(defadvice company-show-doc-buffer (before emacspeak pre act comp)
  "Provide spoken feedback."
  (when (ems-interactive-p)
    (let* ((selected (nth company-selection company-candidates))
           (doc-buffer (or (company-call-backend 'doc-buffer selected)
                           (error "No documentation available"))))
      (with-current-buffer doc-buffer (dtk-speak (buffer-string)))
      (emacspeak-auditory-icon 'help))))

;;}}}
;;{{{ Company Setup For Emacspeak:
(defun emacspeak-company-setup ()
  "Set front-end to our sole front-end action."
  (declare (special company-frontends))
  (add-hook 'company-frontends 'emacspeak-company-frontend 'at-end)
  (add-hook
   'company-completion-started-hook
   #'(lambda (&rest ignore) (emacspeak-auditory-icon 'help)))
  (add-hook
   'company-completion-finished-hook
   #'(lambda (&rest ignore) (emacspeak-auditory-icon 'close-object))))

;;}}}
(emacspeak-company-setup)
(provide 'emacspeak-company)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

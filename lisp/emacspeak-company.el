;;; emacspeak-company.el --- Speech-enable COMPANY -*- lexical-binding: t; -*-
;; $Id: emacspeak-company.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;; $Author: tv.raman.tv $
;; Description:  Speech-enable COMPANY An Emacs Interface to company
;; Keywords: Emacspeak,  Audio Desktop company
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; 
;;  $Revision: 4532 $ |
;; Location undetermined
;; 

;;}}}
;;{{{  Copyright:
;; Copyright (C) 1995 -- 2022, T. V. Raman
;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
;; All Rights Reserved.
;; 
;; This file is not part of GNU Emacs, but the same permissions apply.
;; 
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNCOMPANY FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;; COMPANY -mode: Complete Anything Support for emacs.
;; 
;; This module provides an Emacspeak Company Front-end, And advises
;; the needed interactive commands in Company. It adds an
;; emacspeak-specific front-end @code{emacspeak-company-frontend} to
;; the value of company-frontends. Note that @var{company-frontends}
;; is a user-customizable option and ends up getting saved by emacs
;; along with other custom settings. Function
;; @code{emacspeak-company-frontend} handles providing spoken
;; feedback, and leaves it to other frontends on
;; @var{company-frontends}   to generate their own feedback.
;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(declare-function company-fetch-metadata "company" nil)

;;}}}
;;{{{ map faces:
(voice-setup-add-map
 '(
   (company-echo voice-bolden)
   (company-echo-common voice-bolden-medium)
   (company-preview voice-lighten)
   (company-preview-common voice-lighten-medium)
   (company-preview-search voice-brighten)
   (company-template-field voice-smoothen)))

;;}}}
;;{{{ Helpers:
(defun ems-company-current ()
  "Helper: Return current selection in company."
  (cl-declare (special  company-selection company-candidates))
  (nth company-selection company-candidates))

(defun emacspeak-company-speak-this ()
  "Formatting rule for speaking company selection."
  (ems-with-messages-silenced
   (let ((metadata (funcall 'company-fetch-metadata)))
     (when metadata
       (propertize metadata 'personality 'voice-annotate))
     (dtk-speak-and-echo
      (concat (ems-company-current) " " metadata)))))

;;}}}
;;{{{ Emacspeak Front-End For Company:

(defun emacspeak-company-frontend (command)
  "Emacspeak front-end for Company."
  (cl-case command
    (pre-command nil)
    (post-command (emacspeak-auditory-icon 'help)
                  (emacspeak-company-speak-this))
    (hide nil)))

;;}}}
;;{{{ Advice Interactive Commands:

(defadvice company-complete-selection (before emacspeak pre act comp)
  "Speak the selection."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (dtk-speak (ems-company-current))))
(defadvice company-complete-number (after emacspeak pre act com)
  "Speak what we completed."
  (when (ems-interactive-p)
    (emacspeak-speak-line)))

(defadvice company-show-doc-buffer (before emacspeak pre act comp)
  "Speak."
  (let* ((selected (nth company-selection company-candidates))
         (doc-buffer (or (company-call-backend 'doc-buffer selected)
                         (error "No documentation available"))))
                                        ;(emacspeak-auditory-icon 'help)
    (with-current-buffer doc-buffer (dtk-speak (buffer-string)))))

;;}}}
;;{{{ Company Setup For Emacspeak:

(defun emacspeak-company-setup ()
  "Set front-end to our  front-end action."
  (cl-declare (special company-frontends))
  (when (boundp 'company-frontends)
    (cl-pushnew 'emacspeak-company-frontend company-frontends))
  (add-hook
   'company-completion-started-hook
   #'(lambda (&rest _ignore) (emacspeak-play-auditory-icon 'open-object)))
  (add-hook
   'company-completion-finished-hook
   #'(lambda (&rest _ignore) (emacspeak-play-auditory-icon 'close-object))))

;;}}}
(eval-after-load "company" #'emacspeak-company-setup)
(provide 'emacspeak-company)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}

;;; emacspeak-epa.el --- Speech-enable EasyPG Assistant  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable EPA An Emacs Interface to epa
;;; Keywords: Emacspeak,  Audio Desktop epa
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
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
;;; MERCHANTABILITY or FITNEPA FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; EPA == EasyPG Assistant
;;; Integrate GPG functionality into Emacs.
;;; Speech-enable all interactive commands.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map Faces:
(voice-setup-add-map
 '(
   (epa-validity-high voice-animate)
   (epa-validity-medium voice-smoothen)
   (epa-validity-low voice-smoothen-extra)
   (epa-validity-disabled voice-monotone-extra)
   (epa-string voice-lighten)
   (epa-mark voice-bolden)
   (epa-field-name voice-smoothen)
   (epa-field-body voice-animate)))

;;}}}
;;{{{ Advice Interactive Commands:

(cl-loop
 for f in
 '(
   epa-mail-verify epa-mail-import-keys
   epa-file-select-keys epa-insert-keys
   epa-verify-region epa-verify-file epa-verify-cleartext-in-region
   epa-sign-region epa-sign-file epa-mail-sign
   epa-mail-encrypt epa-mail-decrypt
   epa-import-keys-region epa-import-keys
   epa-import-armor-in-region epa-export-keys
   epa-decrypt-region epa-decrypt-file epa-decrypt-armor-in-region
   epa-encrypt-file epa-encrypt-region
   epa-dired-do-verify epa-dired-do-sign
   epa-dired-do-encrypt epa-dired-do-decrypt
   )
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "speak. "
     (ems-with-messages-silenced
         ad-do-it
         (when (ems-interactive-p)
           (emacspeak-auditory-icon 'task-done))))))

(add-hook
 'epa-key-list-mode-hook
 #'(lambda nil
     (when (sit-for 0.3)
       (emacspeak-auditory-icon 'open-object)
       (emacspeak-speak-line))))

(defadvice epa-delete-keys (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)))

(defadvice epa-exit-buffer (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(cl-loop
 for f in
 '(
   epa-mail-mode epa-global-mail-mode
   epa-file-disable epa-file-enable)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak. "
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'button)))))

(cl-loop
 for f in
 '(epa-list-keys epa-list-secret-keys)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak. "
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)
       (emacspeak-speak-mode-line)))))

(defadvice epa-mark-key(after emacspeak pre act comp)
  "Produce auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'mark-object)))

(defadvice epa-unmark-key(after emacspeak pre act comp)
  "Produce auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'unmark-object)))

;;}}}

(provide 'emacspeak-epa)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

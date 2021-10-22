;;; emacspeak-rpm-spec.el --- Speech enable rpm spec editor  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description: Controlling mplayer from emacs 
;;; Keywords: Emacspeak, rpm-spec streaming media 
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com 
;;; A speech interface to Emacs |
;;; $Date: 2007-09-01 15:30:13 -0700 (Sat, 01 Sep 2007) $ |
;;;  $Revision: 4555 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (c) 1995 -- 2021, T. V. Raman
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Required modules

(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
;;}}}
;;{{{ Introduction:
;;; Commentary:
;;; speech-enable rpm-spec-mode --part of Emacs 21 on RH 7.3
;;; Code:
;;}}}
;;{{{ Advice insertion commands:

(defvar emacspeak-rpm-spec-insertion-commands
  '(rpm-insert-file 
    rpm-insert-config 
    rpm-insert-doc 
    rpm-insert-ghost 
    rpm-insert-dir 
    rpm-insert-docdir 
    rpm-insert 
    rpm-insert-n
    rpm-insert-tag
    rpm-insert-packager)
  "List of rpm-spec insertion commands to speech-enable.")

(cl-loop for f in emacspeak-rpm-spec-insertion-commands
         do
         (eval
          `(defadvice ,f (after emacspeak pre act comp)
             "Speak."
             (when (ems-interactive-p)
               (let ((entry  (format "%s"
                                     (quote ,f))))
                 (setq entry
                       (car (last (split-string entry "-"))))
                 (message
                  (format "Inserted %s entry" entry)))))))

;;}}}
;;{{{ Advice navigation 
(defvar emacspeak-rpm-spec-navigation-commands
  '(rpm-backward-section rpm-beginning-of-section 
                         rpm-forward-section 
                         rpm-end-of-section 
                         rpm-goto-section)
  "Navigation commands in rpm-spec to speech-enable.")
(cl-loop for f in emacspeak-rpm-spec-navigation-commands
         do
         (eval
          `(defadvice ,f (after emacspeak pre act comp)
             "speak."
             (when (ems-interactive-p)
               (emacspeak-auditory-icon 'large-movement)
               (emacspeak-speak-line)))))

;;}}}
;;{{{ Advice build commands 

(defvar emacspeak-rpm-spec-build-commands
  '(rpm-build-bp 
    rpm-build-bl 
    rpm-build-bc 
    rpm-build-bi 
    rpm-build-bb 
    rpm-build-bs 
    rpm-build-ba)
  "Build commands from rpm-spec that are speech-enabled.")

(cl-loop for  f in emacspeak-rpm-spec-build-commands
         do
         (eval
          `(defadvice ,f (after emacspeak pre act comp)
             "Speak."
             (when (ems-interactive-p)
               (let ((target  (format "%s"
                                      (quote ,f))))
                 (setq target
                       (car (last (split-string target "-"))))
                 (emacspeak-auditory-icon 'task-done)
                 (message
                  (format "Launched build %s " target)))))))

;;}}}
;;{{{ advice toggles 
(defvar emacspeak-rpm-spec-toggle-commands
  '(rpm-toggle-short-circuit 
    rpm-toggle-rmsource 
    rpm-toggle-clean 
    rpm-toggle-test 
    rpm-toggle-sign-gpg 
    rpm-toggle-add-attr)
  "Toggle commands from rpm-spec that are speech-enabled.")

(cl-loop for f in emacspeak-rpm-spec-toggle-commands
         do
         (eval
          `(defadvice ,f (after emacspeak pre act comp)
             "Speak."
             (when (ems-interactive-p)
               (let ((toggle  (format "%s" (quote ,f)))
                     (switch nil))
                 (setq switch
                       (intern
                        (replace-regexp-in-string "toggle"
                                                  "spec"
                                                  toggle)))
                 (emacspeak-auditory-icon (if (eval switch) 'on 'off)))))))

;;}}}
;;{{{ voice locking 

(voice-setup-add-map
 '(
   (rpm-spec-macro-face voice-bolden)
   (rpm-spec-tag-face voice-smoothen)
   (rpm-spec-package-face voice-animate)
   (rpm-spec-dir-face voice-lighten)
   (rpm-spec-doc-face voice-smoothen-extra)
   (rpm-spec-ghost-face voice-smoothen-medium)
   ))
;;}}}
(provide 'emacspeak-rpm-spec)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}

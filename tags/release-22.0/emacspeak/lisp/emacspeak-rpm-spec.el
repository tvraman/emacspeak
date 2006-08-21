;;; emacspeak-rpm-spec.el --- Speech enable rpm spec editor
;;; $Id$
;;; $Author$
;;; Description: Controlling mplayer from emacs 
;;; Keywords: Emacspeak, rpm-spec streaming media 
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

;;; Copyright (c) 1995 -- 2004, T. V. Raman
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

(require 'emacspeak-preamble)
;;}}}
;;{{{ Introduction:

;;; speech-enable rpm-spec-mode --part of Emacs 21 on RH 7.3

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

(loop for f in emacspeak-rpm-spec-insertion-commands
      do
      (eval
       (`
        (defadvice (, f) (after emacspeak pre act comp)
          "Provide spoken feedback."
          (when (interactive-p)
            (let ((entry  (format "%s"
                                  (quote (, f)))))
              (setq entry
                    (car (last
			  (split-string entry "-"))))
              (message
               (format "Inserted %s entry" entry))))))))

;;}}}
;;{{{ Advice navigation 
(defvar emacspeak-rpm-spec-navigation-commands
  '(rpm-backward-section rpm-beginning-of-section 
			 rpm-forward-section 
			 rpm-end-of-section 
			 rpm-goto-section )
  "Navigation commands in rpm-spec to speech-enable.")
(loop for f in emacspeak-rpm-spec-navigation-commands
      do
      (eval
       (`
        (defadvice (, f) (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (interactive-p)
            (emacspeak-auditory-icon 'large-movement)
            (emacspeak-speak-line))))))

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

(loop for  f in emacspeak-rpm-spec-build-commands
      do
      (eval
       (`
        (defadvice (, f) (after emacspeak pre act comp)
          "Provide spoken feedback."
          (when (interactive-p)
            (let ((target  (format "%s"
				   (quote (, f)))))
              (setq target
                    (car (last (split-string target "-"))))
              (emacspeak-auditory-icon 'task-done)
              (message
               (format "Launched build %s " target))))))))

;;}}}
;;{{{ advice toggles 
(defvar emacspeak-rpm-spec-toggle-commands
  '(rpm-toggle-short-circuit 
    rpm-toggle-rmsource 
    rpm-toggle-clean 
    rpm-toggle-test 
    rpm-toggle-sign-gpg 
    rpm-toggle-add-attr )
  "Toggle commands from rpm-spec that are speech-enabled.")

(loop for f in emacspeak-rpm-spec-toggle-commands
      do
      (eval
       (`
        (defadvice (, f) (after emacspeak pre act comp)
          "Provide spoken feedback."
          (when (interactive-p)
            (let ((toggle  (format "%s" (quote (, f))))
                  (switch nil))
              (setq switch
                    (intern
                     (replace-regexp-in-string "toggle"
                                               "spec"
                                               toggle)))
              (emacspeak-auditory-icon
               (if (eval switch) 'on 'off))))))))

;;}}}
;;{{{ voice locking 

(def-voice-font rpm-spec-macro-personality  voice-bolden
  'rpm-spec-macro-face
  ".Personality for macros"
  :group 'emacspeak-rpm)

(def-voice-font rpm-spec-tag-personality voice-smoothen
  'rpm-spec-tag-face
  ".Personality for tags"
  :group 'emacspeak-rpm)

(def-voice-font rpm-spec-package-personality voice-animate
  'rpm-spec-package-face
  ".Personality for package tag"
  :group 'emacspeak-rpm)

(def-voice-font rpm-spec-dir-personality voice-lighten
  'rpm-spec-dir-face
  ".Personality for directory entries"
  :group 'emacspeak-rpm)

(def-voice-font rpm-spec-doc-personality voice-smoothen-extra
  'rpm-spec-doc-face
  ".Personality for documentation entries"
  :group 'emacspeak-rpm)

(def-voice-font rpm-spec-ghost-personality voice-smoothen-medium
  'rpm-spec-ghost-face
  ".Personality for %ghost files"
  :group 'emacspeak-rpm)

;;}}}
(provide 'emacspeak-rpm-spec)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

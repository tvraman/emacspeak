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

;;; Copyright (c) 1995 -- 2002, T. V. Raman
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

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(eval-when (compile)
  (require 'emacspeak-fix-interactive))
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
(require 'voice-lock)

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

(defvar rpm-spec-macro-personality 'harry
  "*Personality for macros")
(defvar rpm-spec-tag-personality 'paul-smooth
  "*Personality for tags")
(defvar rpm-spec-package-personality 'paul-animated
  "*Personality for package tag")
(defvar rpm-spec-dir-personality 'betty
  "*Personality for directory entries")
(defvar rpm-spec-doc-personality 'paul-monotone
  "*Personality for documentation entries")
(defvar rpm-spec-ghost-personality 'annotation-voice
  "*Personality for %ghost files")

(defvar rpm-spec-voice-lock-keywords
  '(
    ("%[a-zA-Z0-9_]+" 0 rpm-spec-macro-personality)
    ("^\\([a-zA-Z0-9]+\\)\\(\([a-zA-Z0-9,]+\)\\):"
     (1 rpm-spec-tag-personality)
     (2 rpm-spec-ghost-personality))
    ("^\\([a-zA-Z0-9]+\\):" 1 rpm-spec-tag-personality)
    ("%\\(define\\|files\\|package\\|description\\)[ \t]+\\([^ \t\n-]+\\)"
     (2 rpm-spec-package-personality))
    ("%configure " 0 rpm-spec-macro-personality)
    ("%dir[ \t]+\\([^ \t\n]+\\)[ \t]*" 1 rpm-spec-dir-personality)
    ("%doc\\(\\|dir\\)[ \t]+\\(.*\\)\n" 2 rpm-spec-doc-personality)
    ("%\\(ghost\\|config\\)[ \t]+\\(.*\\)\n" 2 rpm-spec-ghost-personality)
    ("^%.+-[a-zA-Z][ \t]+\\([a-zA-Z0-9\.-]+\\)" 1 rpm-spec-doc-personality)
    ("^\\(.+\\)(\\([a-zA-Z]\\{2,2\\}\\)):" 
     (1 rpm-spec-tag-personality)
     (2 rpm-spec-doc-personality))
    ("^\\*\\(.*[0-9] \\)\\(.*\\)\\(<.*>\\)\\(.*\\)\n"
     (1 rpm-spec-dir-personality)
     (2 rpm-spec-package-personality)
     (3 rpm-spec-tag-personality)
     (4 voice-lock-warning-personality))
    ("%{[^{}]*}" 0 rpm-spec-macro-personality)
    )
  "Additional expressions to highlight in RPM Spec mode.")

(voice-lock-set-major-mode-keywords 'rpm-spec-mode
                                    'rpm-spec-voice-lock-keywords)

;;}}}
(provide 'emacspeak-rpm-spec)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

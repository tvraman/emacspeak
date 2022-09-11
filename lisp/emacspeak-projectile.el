;;; emacspeak-projectile.el --- Speech-enable PROJECTILE  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable PROJECTILE An Emacs Project Manager
;; Keywords: Emacspeak,  Audio Desktop projectile
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
;; Copyright (C) 1995 -- 2007, 2011, T. V. Raman
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
;; MERCHANTABILITY or FITNPROJECTILE FOR A PARTICULAR PURPOSE.  See the
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

;; PROJECTILE ==  @samp{M-x package-install projectile}.
;; Project management in Emacs.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Emacspeak Helpers:

(defun emacspeak-projectile-file-action ()
  "speak for file open actions."
  (emacspeak-auditory-icon 'open-object)
  (emacspeak-speak-mode-line))

;;}}}
;;{{{ Speech-enable Interactive Commands:
(defadvice projectile-vc (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(cl-loop
 for f in
 '(projectile-ag
   projectile-cleanup-known-projects
   projectile-clear-known-projects
   projectile-compile-project
   projectile-regenerate-tags
   projectile-run-async-shell-command-in-root
   projectile-run-command-in-root
   projectile-run-project
   projectile-run-shell-command-in-root
   projectile-test-project
   projectile-ibuffer
   )
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'task-done)
       (emacspeak-speak-line)))))
(add-hook 'projectile-find-file-hook 'emacspeak-projectile-file-action)
(defadvice projectile-edit-dir-locals(after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(cl-loop
 for f in
 '(projectile-run-shell projectile-run-eshell projectile-run-term)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-speak-mode-line)
       (emacspeak-auditory-icon 'open-object)))))

;;}}}
(provide 'emacspeak-projectile)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}

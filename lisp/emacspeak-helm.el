;;; emacspeak-helm.el --- Speech-enable HELM  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable HELM An Emacs Interface to helm
;; Keywords: Emacspeak,  Audio Desktop helm
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
;; MERCHANTABILITY or FITNHELM FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;; HELM == Smart narrowing/selection in emacs This module
;; speech-enables Helm interaction.  See tvr/helm-prepare.el in the
;; GitHub repository for my helm setup.  that file provides convenient
;; emacspeak-centric keybindings for Helm interaction.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(require 'emacspeak-google)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Setup Helm Hooks:

(defadvice helm-mode (after emacspeak pre act comp)
  "Cue state of helm mode."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon (if helm-mode  'on 'off))
    (message "Turned %s helm-mode"
             (if helm-mode "on" "off"))))

(declare-function emacspeak-minibuffer-setup-hook "emacspeak-advice" nil)

(defun emacspeak-helm-before-initialize-hook ()
  "Remove emacspeak minibuffer setup hook."
  (emacspeak-auditory-icon 'complete)
  (remove-hook 'minibuffer-setup-hook #'emacspeak-minibuffer-setup-hook))

                                        ;(add-hook
                                        ;'helm-minibuffer-set-up-hook
(defun emacspeak-helm-cleanup-hook ()
  "Restore Emacspeak's minibuffer setup hook."
  (add-hook 'minibuffer-setup-hook #'emacspeak-minibuffer-setup-hook))

(defun emacspeak-helm-cue-update ()
  " Cue update."
  (let ((inhibit-read-only t)
        (line (buffer-substring (line-beginning-position) (line-end-position)))
        (count-msg nil))
    (setq count-msg
          (concat
           (propertize
            (format "%d of %d"
                    (- (line-number-at-pos) 2)
                    (- (count-lines(point-min) (point-max))2))
            'personality voice-bolden)))
    (when (and line count-msg)
      (dtk-speak (concat line count-msg)))))

(add-hook 'helm-move-selection-after-hook #'emacspeak-helm-cue-update 'at-end)
(add-hook 'helm-after-action-hook #'emacspeak-speak-mode-line 'at-end)

;;}}}
;;{{{ Advice helm-google-suggest to filter results:

(declare-function eww-display-dom-by-id-list  "emacspeak-eww.el" (id-list))

(defadvice helm-google-suggest (before emacspeak pre act comp)
  "setup emacspeak post-processing-hook"
  (add-hook
   'emacspeak-eww-post-process-hook
   #'(lambda nil
       (let  ((emacspeak-google-toolbelt (emacspeak-google-toolbelt)))
         (eww-display-dom-by-id-list '("center_col" "rhs"))))))

;;}}}
;;{{{ Advice helm-recenter-top-bottom-other-window:

(defadvice helm-recenter-top-bottom-other-window (after emacspeak pre act comp)
  "Speak current selection."
  (when (ems-interactive-p)
    (with-current-buffer (helm-buffer-get)
      (emacspeak-auditory-icon 'scroll)
      (emacspeak-speak-line))))

;;}}}
;;{{{ Advice helm-yank-selection

(defadvice helm-yank-selection (after emacspeak pre act comp)
  "Speak minibuffer after yanking."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-line)))

;;}}}

;;{{{ Support helm-help
(add-hook
 'helm-help-mode-before-hook
 #'(lambda()
     "Turn off speaking read-key prompts"
     (setq emacspeak-speak-messages nil)
     (emacspeak-auditory-icon 'open-object)))

(add-hook
 'helm-help-mode-after-hook
 #'(lambda()
     "restore speaking messages."
     (setq emacspeak-speak-messages t)
     (emacspeak-auditory-icon 'close-object)))

;;}}}
(provide 'emacspeak-helm)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}

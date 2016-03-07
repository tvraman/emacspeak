;;; emacspeak-helm.el --- Speech-enable HELM
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable HELM An Emacs Interface to helm
;;; Keywords: Emacspeak,  Audio Desktop helm
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
;;; MERCHANTABILITY or FITNHELM FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; HELM ==  Smart narrowing/selection in emacs

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Setup Helm Hooks:

(defadvice helm-mode (after emacspeak pre act comp)
  "Emacspeak setup."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon (if helm-mode  'on 'off))
    (message "Turned %s helm-mode"
           (if helm-mode "on" "off"))))


(defun emacspeak-helm-before-initialize-hook ()
  "Remove emacspeak minibuffer setup hook."
  (emacspeak-auditory-icon 'open-object)
  (remove-hook 'minibuffer-setup-hook #'emacspeak-minibuffer-setup-hook))

(add-hook 'helm-before-initialize-hook #'emacspeak-helm-before-initialize-hook)

(defun emacspeak-helm-cleanup-hook ()
  "Restore Emacspeak's minibuffer setup hook."
  (add-hook 'minibuffer-setup-hook #'emacspeak-minibuffer-setup-hook))

(add-hook 'helm-cleanup-hook #'emacspeak-helm-cleanup-hook)

(defun emacspeak-helm-cue-update ()
  " Cue update."
  (let ((inhibit-read-only t)
        (line (buffer-substring (line-beginning-position) (line-end-position)))
        (count-msg nil)
        (count (-  (count-lines (point-min) (point-max)) 2)))
    (when (and (not   (zerop count))
               (sit-for 0.2 t))
      (setq count-msg
            (concat
             (propertize
              (format "%d of %d"
                      (- (line-number-at-pos) 2)
                      (- (count-lines(point-min) (point-max))2))
              'personality voice-bolden)))
      (emacspeak-auditory-icon 'progress)
      (condition-case nil
          (dtk-speak (concat line count-msg))
        (error nil)))))

(add-hook 'helm-move-selection-after-hook #'emacspeak-helm-cue-update 'at-end)
(add-hook 'helm-after-action-hook #'emacspeak-speak-mode-line 'at-end)

;;}}}
;;{{{ Helm Setup:

;;}}}
;;{{{ Advice helm-google-suggest to filter results:

(defadvice helm-google-suggest (before emacspea pre act comp)
  "setup emacspeak post-processing-hook"
  (add-hook
   'emacspeak-web-post-process-hook
   #'(lambda nil
       (setq emacspeak-google-toolbelt (emacspeak-google-toolbelt))
       (eww-display-dom-by-id-list '("center_col" "rhs")))))

;;}}}
;;{{{ Advice helm-recenter-top-bottom-other-window:

(defadvice helm-recenter-top-bottom-other-window (after emacspeak pre act comp)
  "Speak current selection."
  (with-current-buffer (helm-buffer-get)
    (emacspeak-auditory-icon 'progress)
    (emacspeak-speak-line)))

;;}}}
(provide 'emacspeak-helm)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

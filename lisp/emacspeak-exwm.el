;;; emacspeak-exwm.el --- Speech-enable EXWM  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable EXWM An Emacs Interface to exwm
;;; Keywords: Emacspeak,  Audio Desktop exwm
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, 2019, T. V. Raman
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
;;; MERCHANTABILITY or FITNEXWM FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; EXWM ==  Emacs X Window Manager
;;; This module speech-enables and integrates EXWM on the Emacspeak
;;; Audio Desktop
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'exwm "exwm" 'no-error)

;;}}}
;;{{{Advise internal helpers:

(defadvice exwm-workspace--prompt-for-workspace (before emacspeak pre act comp)
  "speak prompt."
  (dtk-speak (ad-get-arg 0)))

;;}}}
;;{{{ Interactive Commands:

'(
exwm-workspace--handle-focus-in
exwm-workspace--handle-focus-out
exwm-workspace--prompt-add
exwm-workspace--prompt-delete
exwm-workspace--switch-map-nth-prefix
exwm-workspace--switch-map-select-nth
exwm-workspace-add
exwm-workspace-attach-minibuffer
exwm-workspace-delete
exwm-workspace-detach-minibuffer
exwm-workspace-move
exwm-workspace-move-window
exwm-workspace-swap
exwm-workspace-switch
exwm-workspace-switch-create
exwm-workspace-switch-to-buffer
exwm-workspace-toggle-minibuffer
)

;;}}}
;;{{{  Advice Interactive Commands

(defadvice exwm-floating-hide (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (dtk-speak "Hid floating window")))


(defadvice exwm-floating-toggle-floating (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (dtk-speak
     (format "Turned %s floating"
             (if exwm--floating-frame "on" "off")))
    (emacspeak-auditory-icon
      (if  exwm--floating-frame 'on 'off))))

(defadvice exwm-input-grab-keyboard (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (dtk-speak "line mode")
    (emacspeak-auditory-icon 'off)))


(defadvice exwm-input-release-keyboard (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (dtk-speak "Char mode")
    (emacspeak-auditory-icon 'oon)))


(defadvice exwm-input-toggle-keyboard (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (cl-case exwm--input-mode
        (line-mode
         (dtk-speak "Line mode")
         (emacspeak-auditory-icon 'off))
        (char-mode
         (dtk-speak "Char mode")
         (emacspeak-auditory-icon 'on)))))


(defadvice exwm-layout-show-mode-line (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (dtk-speak "Showing mode line")
    (emacspeak-auditory-icon 'open-object)))

(defadvice exwm-layout-set-fullscreen (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (dtk-speak "Full screen")
  (emacspeak-auditory-icon 'window-resize)))


(defadvice exwm-layout-hide-mode-line (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (dtk-speak "hid mode line")
    (emacspeak-auditory-icon 'close-object)))

(defadvice exwm-layout-toggle-fullscreen (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (dtk-speak
     (format
      "Turned %s full screen"
      (if (exwm-layout--fullscreen-p) "on" "off")))
    (emacspeak-auditory-icon (if (exwm-layout--fullscreen-p) 'on  'off))))

(defadvice exwm-layout-toggle-mode-line (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (dtk-speak
     (format "Turned %s mode line"
             (if mode-line-format 'on 'off)))
    (emacspeak-auditory-icon (if mode-line-format 'on 'off))))
(defadvice exwm-workspace-switch (after emacspeak pre act comp)
  "speak frame title."
  (when (ems-interactive-p)
    (emacspeak-speak-frame-title)))


;;}}}
;;{{{Additional Interactive Commands:
;; I bind this to s-/ via custom:

(defun emacspeak-exwm-workspace-cycle ()
  "Cycle to next workspace, with wrap-around"
  (interactive)
  (cl-declare (special exwm-workspace-number ))
  (let ((index (exwm-workspace--position exwm-workspace--current)))
    (exwm-workspace-switch (% (1+ index) exwm-workspace-number))
    (emacspeak-speak-frame-title)))

;;}}}
;;{{{Orca Toggle:

;;; Easily start/stop orca for use with Chrome etc.

(defvar emacspeak-exwm-orca-handle nil
  "Orca process handle")

(defun emacspeak-exwm-orca-toggle ()
  "Toggle state of orca."
  (interactive)
  (cl-declare (special emacspeak-exwm-orca-handle))
  (cond
    (emacspeak-exwm-orca-handle (delete-process emacspeak-exwm-orca-handle)
                  (setq emacspeak-exwm-orca-handle  nil))
    (t (setq emacspeak-exwm-orca-handle (start-process "Orca"nil "orca")))))

(global-set-key (kbd "s-o") 'emacspeak-exwm-orca-toggle)


;;}}}

(provide 'emacspeak-exwm)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

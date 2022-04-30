;;; emacspeak-flyspell.el --- Speech enable flyspell -*- lexical-binding: t; -*-
;;
;; $Author: tv.raman.tv $
;; Description:  Emacspeak extension to speech enable flyspell
;; Keywords: Emacspeak, Ispell, Spoken Output, fly spell checking
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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Introduction:

;;; Commentary:

;; This module speech enables flyspell.
;; it loads flyspell-correct if available,
;; And when loading flyspell-correct sets up that module
;; to use  one of   three supported correction styles:
;; @itemize @bullet
;; @item ido: IDO-like completion with C-s and C-r moving through choices.
;; @item popup:Use  up and down arrows to move through  corrections.
;; @item helm: A helm interface for picking amongst  corrections.
;; @end itemize
;; See documentation for package flyspell-correct for additional
;; details.
;; 
;; Use Customization emacspeak-flyspell-correct to pick
;; between ido, popup and helm.

;;; Code:

;;}}}
;;{{{ Requires

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ define personalities

(defgroup emacspeak-flyspell nil
  "Emacspeak support for on the
fly spell checking."
  :group 'emacspeak
  :group 'flyspell
  :prefix "emacspeak-flyspell-")

(voice-setup-add-map
 '((flyspell-incorrect voice-bolden)
   (flyspell-duplicate voice-monotone-extra)))

;;}}}
;;{{{ advice

(cl-declaim (special flyspell-delayed-commands))
(when (fboundp 'emacspeak-self-insert-command)
  (push 'emacspeak-self-insert-command flyspell-delayed-commands))

(defadvice flyspell-auto-correct-word (around emacspeak pre act comp)
  "Speak the correction we inserted."
  (cond
   ((ems-interactive-p)
    (ems-with-messages-silenced
     ad-do-it
     (dtk-speak (car (flyspell-get-word nil)))
     (when (sit-for 1)
       (dtk-notify-speak (cl-second flyspell-auto-correct-ring)))
     (when (sit-for 1) (emacspeak-speak-message-again))
     (emacspeak-auditory-icon 'select-object))) (t ad-do-it)) ad-return-value)

(defadvice flyspell-unhighlight-at (before emacspeak pre act comp)
  "handle highlight/unhighlight."
  (let ((overlay-list (overlays-at (ad-get-arg 0)))
        (o nil))
    (while overlay-list
      (setq o (car overlay-list))
      (when (flyspell-overlay-p o)
        (put-text-property (overlay-start o) (overlay-end o) 'personality nil))
      (setq overlay-list (cdr overlay-list)))))

(add-hook
 'flyspell-incorrect-hook
 #'(lambda (_s _e p)
     (unless (eq p 'doublon) (emacspeak-auditory-icon 'help))
     nil))

;;}}}
;;{{{ use flyspell-correct if available:
(defcustom emacspeak-flyspell-correct
  (cond
   ((locate-library "flyspell-correct-ido") 'flyspell-correct-ido)
   ((locate-library "flyspell-correct-popup") 'flyspell-correct-popup)
   ((locate-library "flyspell-correct-helm") 'flyspell-correct-helm)
   (t nil))
  "Correction style to use with flyspell."
  :type 'symbol)

;; flyspell-correct is available on melpa:
(cl-declaim (special flyspell-mode-map))
(when
    (and (bound-and-true-p flyspell-mode-map)
         (locate-library "flyspell-correct"))
  (define-key flyspell-mode-map (ems-kbd "C-x .") 'flyspell-correct-at-point)
  (define-key flyspell-mode-map (ems-kbd "C-'") 'flyspell-correct-previous)
  (define-key flyspell-mode-map (ems-kbd "C-;") 'flyspell-correct-wrapper)
  (require emacspeak-flyspell-correct))

(cl-loop
 for f in
 '(flyspell-correct-next flyspell-correct-previous flyspell-correct-at-point)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak word."
     (when (ems-interactive-p)
       (dtk-speak (car (flyspell-get-word nil)))))))

;;}}}
(provide 'emacspeak-flyspell)
;;{{{ emacs local variables

;; local variables:
;; folded-file: t
;; end:

;;}}}

;;; emacspeak-flyspell.el ends here

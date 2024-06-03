;;; emacspeak-eat.el --- Speech-enable EAT  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Keywords: Emacspeak,  Audio Desktop eat
;;;   LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;;  $Revision: 4532 $ |
;;; Location https://github.com/tvraman/emacspeak
;;;

;;;   Copyright:

;; Copyright (C) 1995 -- 2024, T. V. Raman
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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Commentary:
;; EAT ==  Emacs Terminal Emulator

;;; Code:

;;   Required modules:

(eval-when-compile  (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(eval-when-compile (require 'eat "eat" 'no-error))
(declare-function eat-term-display-cursor "eat" (terminal))

;;;  Map Faces:

(voice-setup-add-map
 '(
   (eat-shell-prompt-annotation-failure voice-lighten)
   (eat-shell-prompt-annotation-running voice-monotone)
   (eat-shell-prompt-annotation-success voice-animate)
   (eat-term-bold voice-bolden)
   (eat-term-italic voice-smoothen)))
;;; Eat Setup:

(defun emacspeak-eat-mode-setup ()
  "Placed on eat-mode-hook to do Emacspeak setup."
  (cl-declare (special eat-semi-char-mode-map eat-mode-map
                       eat-line-mode-map  eat-char-mode-map))
  (define-key eat-semi-char-mode-map emacspeak-prefix 'emacspeak-keymap)
  (cl-loop
   for map in
   '(eat-line-mode-map eat-mode-map eat-char-mode-map)
   do
   (when (keymapp map) (define-key map emacspeak-prefix  'emacspeak-keymap))))

(add-hook 'eat-mode-hook 'emacspeak-eat-mode-setup)

;;;  Interactive Commands:

'(

  eat-input-char
  eat-kill-process
  eat-line-delchar-or-eof
  eat-line-find-input
  eat-line-history-isearch-backward
  eat-line-history-isearch-backward-regexp
  eat-line-load-input-history-from-file
  eat-line-next-input
  eat-line-next-matching-input
  eat-line-next-matching-input-from-input
  eat-line-previous-input
  eat-line-previous-matching-input
  eat-line-previous-matching-input-from-input
  eat-line-restore-input
  eat-line-send-input
  eat-line-send-interrupt
  eat-mouse-yank-primary
  eat-mouse-yank-secondary
  eat-narrow-to-shell-prompt
  eat-next-shell-prompt
  eat-other-window
  eat-previous-shell-prompt
  eat-project
  eat-project-other-window
  eat-quoted-input
  eat-reload
  eat-reset
  eat-self-input
  eat-send-password
  eat-trace-replay
  eat-trace-replay-next-frame
  eat-xterm-paste
  )

(cl-loop
 for f in 
 '(eat-yank eat-yank-from-kill-ring)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Icon."
     ad-do-it
     (when (ems-interactive-p) (emacspeak-icon 'yank-object))
     ad-return-value)))

(defadvice eat-reload (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-icon 'task-done)
    (dtk-speak "Reloaded Eat")))

(defadvice eat-reset (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-icon 'task-done)
    (dtk-speak "Reset Eat")))

(cl-loop
 for f in
 '(
   eat-blink-mode eat-char-mode eat-emacs-mode
   eat-eshell-char-mode eat-eshell-emacs-mode eat-eshell-mode
   eat-eshell-semi-char-mode eat-eshell-visual-command-mode
   eat-line-mode eat-mode eat-semi-char-mode
   eat-trace-mode eat-trace-replay-mode)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-icon 'button)
       (message "%s " ,(symbol-name f))))))

(defadvice eat (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-icon 'open-object)
    (emacspeak-speak-mode-line)))

;;; Speech-Enable Terminal Emulation:

(defun emacspeak-eat-update-hook ()
  "Eat update"
  (cl-declaim (special eat-terminal))
  (let* ((emacspeak-show-point  t)
         (cursor (eat-term-display-cursor eat-terminal))
         (char (and cursor (char-before cursor))))
    (cond
     ((= char 32) (emacspeak-speak-line))
     (t (emacspeak-speak-this-char char)))))

(add-hook 'eat-update-hook #'emacspeak-eat-update-hook)
(provide 'emacspeak-eat)
;;;  end of file

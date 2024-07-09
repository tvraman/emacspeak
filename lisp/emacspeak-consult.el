;;; emacspeak-consult.el --- Speech-enable CONSULT  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Keywords: Emacspeak,  Audio Desktop consult
;;; LCD Archive Entry:
;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;; A speech interface to Emacs |
;; Location https://github.com/tvraman/emacspeak
;;;   Copyright:

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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; CONSULT ==  A modern completing-read 
;; @section Consult
;; @itemize
;; @item Setup @code{consult-after-jump-hook} to speak  where we
;; land.
;; @item Advice needed interactive commands.
;; @item Map faces.
;; @item Setup: Put consult commands on @code{C-/}
;; @end itemize
;;; Code:

;;   Required modules

(eval-when-compile  (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(eval-when-compile  (require 'consult "consult" 'no-error))
;;;  Map Faces:



(voice-setup-add-map 
'(
(consult-async-failed voice-lighten)
(consult-async-finished voice-monotone)
(consult-async-running voice-animated)
(consult-async-split voice-brighten)
(consult-bookmark voice-bolden)
(consult-buffer voice-bolden)
(consult-file voice-bolden)
(consult-grep-context voice-animate)
(consult-help voice-lighten)
(consult-highlight-mark voice-animate)
(consult-highlight-match voice-brighten)
(consult-key voice-monotone)
(consult-line-number voice-smoothen)
(consult-line-number-prefix voice-lighten)
(consult-line-number-wrapped voice-lighten)
(consult-preview-insertion voice-bolden)
(consult-preview-line voice-animate)
(consult-preview-match voice-bolden)))


;;;  Interactive Commands:

'(
consult-complex-command
consult-flymake
consult-focus-lines
consult-global-mark
consult-goto-line
consult-history
consult-isearch-backward
consult-isearch-forward
consult-isearch-history
consult-keep-lines
consult-kmacro
consult-line
consult-line-multi
consult-mark
consult-minor-mode-menu
consult-mode-command
consult-narrow
consult-narrow-help
consult-preview-at-point
consult-preview-at-point-mode
consult-project-buffer
consult-recent-file
consult-register
consult-register-load
consult-register-store
consult-theme
consult-yank-from-kill-ring
consult-yank-pop
consult-yank-replace
)



(add-hook 'consult-after-jump-hook #'emacspeak-speak-line)

(cl-loop
 for f in 
 '(consult-bookmark
consult-compile-error)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-icon 'select-object)
       (emacspeak-speak-line)))))



(cl-loop
 for f in 
 '(
   consult-buffer consult-buffer-other-frame
   consult-buffer-other-tab consult-buffer-other-window
   consult-find consult-fd)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-icon 'open-object)
       (emacspeak-speak-mode-line)))))

;;; Set it up:
(defvar  emacspeak-consult-keymap nil "Emacspeak consult keymap")

(define-prefix-command 'emacspeak-consult-keymap)

(global-set-key (kbd "C-/") 'emacspeak-consult-keymap)

(cl-loop
 for b in
 '(
   ("#" consult-register-load)
   ("'" consult-register-store)
   ("4b" consult-buffer-other-window)
   ("5b" consult-buffer-other-frame)
   (":" consult-complex-command)
   ("B" consult-bookmark)
   ("G" consult-goto-line)
   ("K" consult-keep-lines)
   ("L" consult-line-multi)
   ("M" consult-mark)
   ("M-e" consult-isearch-history)
   ("M-x" consult-mode-command)
   ("M-y" consult-yank-pop)
   ("b" consult-buffer)
   ("c" consult-locate)
   ("d" consult-find)
   ("e" consult-compile-error)
   ("g" consult-grep)
   ("f" consult-fd)
   ("h" consult-org-heading)
   ("i" consult-info)
   ("j" consult-imenu)
   ("k" consult-global-mark)
   ("l" consult-line)
   ("m" consult-man)
   ("o" consult-outline)
   ("p" consult-project-buffer)
   ("r" consult-ripgrep)
   ("u" consult-focus-lines)
   )
 do
 (define-key  emacspeak-consult-keymap (kbd (car b))  (cadr b)))


(provide 'emacspeak-consult)
;;;  end of file

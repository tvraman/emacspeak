;;; emacspeak-evil.el --- Speech-enable EVIL  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable EVIL An Emacs Interface to evil
;;; Keywords: Emacspeak,  Audio Desktop evil
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
;;; MERCHANTABILITY or FITNEVIL FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; EVIL ==  VIM In Emacs
;;; This is work-in-progress and is not complete.
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map Faces:

(voice-setup-add-map
 '(
   (evil-ex-commands voice-bolden)
   (evil-ex-info voice-monotone)
   (evil-ex-lazy-highlight voice-animate)
   (evil-ex-search voice-bolden-and-animate)
   (evil-ex-substitute-matches voice-lighten)
   (evil-ex-substitute-replacement voice-smoothen)))

;;}}}
;;{{{ Interactive Commands:

;;}}}
;;{{{ Structured  Motion:

(cl-loop
 for f in
 '(
   evil-beginning-of-line evil-end-of-line
   evil-next-line evil-previous-line
   evil-ret evil-window-top)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'select-object)
       (emacspeak-speak-line)))))

(cl-loop
 for f in
 '(
   evil-goto-mark evil-goto-mark-line
   evil-goto-definition evil-goto-first-line evil-goto-line
   evil-forward-section-begin evil-forward-section-end
   evil-backward-paragraph evil-forward-paragraph
   evil-backward-section-begin evil-backward-section-end
   evil-previous-open-paren evil-previous-match evil-next-match
   evil-next-line-first-non-blank evil-next-line-1-first-non-blank
   evil-next-close-paren evil-last-non-blank
   evil-jump-backward evil-jump-forward evil-jump-to-tag
   evil-forward-sentence-begin evil-first-non-blank
   evil-backward-sentence-begin)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (let ((emacspeak-show-point t))
         (emacspeak-auditory-icon 'large-movement)
         (emacspeak-speak-line))))))

;;}}}
;;{{{ Word Motion

(cl-loop
 for f in
 '(
   evil-backward-WORD-begin evil-backward-WORD-end
   evil-forward-WORD-begin evil-forward-WORD-end
   evil-backward-word-begin evil-backward-word-end
   evil-forward-word-begin evil-forward-word-end)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-speak-word)))))

;;}}}
;;{{{ Char Motion :

(defadvice evil-backward-char (before emacspeak pre act comp)
  "Speak char."
  (when (ems-interactive-p)
    (emacspeak-speak-this-char (char-after (1- (point))))))

(defadvice evil-forward-char (before emacspeak pre act comp)
  "Speak char."
  (when (ems-interactive-p)
    (emacspeak-speak-this-char (char-after (1+ (point))))))

;;}}}
;;{{{ Deletion:

(defadvice evil-delete-char (before emacspeak pre act comp)
  "Speak char we are deleting."
  (when (ems-interactive-p)
    (emacspeak-speak-char t)
    (dtk-tone-deletion)))

(defadvice evil-delete-backward-char (before emacspeak pre act comp)
  "Speak char we are deleting."
  (when (ems-interactive-p)
    (emacspeak-speak-this-char (preceding-char))
    (dtk-tone-deletion)))

(defadvice evil-delete-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (dtk-speak "Deleted to end of line.")
    (emacspeak-auditory-icon 'delete-object)))

(defadvice evil-delete (before emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-region (ad-get-arg 0) (ad-get-arg 1))))

;;}}}
;;{{{ Searching:
(cl-loop
 for f in
 '(evil-search-next evil-search-previous)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak line with point highlighted."
     (when (ems-interactive-p)
       (let ((emacspeak-show-point t))
         (emacspeak-speak-line)
         (emacspeak-auditory-icon 'search-hit))))))

;;}}}
;;{{{ Completion:

(cl-loop
 for f in
 '(evil-complete-next evil-complete-previous)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Speak what was completed."
     (cond
      ((ems-interactive-p)
       (let ((orig (save-excursion (skip-syntax-backward "^ >") (point))))
         (ems-with-messages-silenced
          ad-do-it
          (emacspeak-auditory-icon 'complete)
          (if (< orig (point))
              (dtk-speak (buffer-substring orig (point)))
            (dtk-speak (word-at-point))))))
      (t ad-do-it))
     ad-return-value)))

(cl-loop
 for f in
 '(evil-complete-next-line evil-complete-previous-line)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak completed line."
     (when (ems-interactive-p)
       (let ((emacspeak-show-point t))
         (emacspeak-auditory-icon 'complete)
         (emacspeak-speak-line))))))

;;}}}
;;{{{ Marks:
(defadvice evil-set-marker (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (let ((emacspeak-show-point t))
      (dtk-notify-speak (format "Marker %c" (ad-get-arg 0)))
      (emacspeak-speak-line))))

;;}}}
;;{{{ Update keymaps:

(defun emacspeak-evil-fix-emacspeak-prefix (keymap)
  "Move original evil command on C-e to C-e e."
  (cl-declare (special emacspeak-prefix))
  (when (keymapp keymap)
    (let ((orig (lookup-key keymap emacspeak-prefix)))
      (when orig
        (define-key keymap emacspeak-prefix  'emacspeak-prefix-command)
        (define-key keymap (concat emacspeak-prefix "e") orig)
        (define-key keymap (concat emacspeak-prefix emacspeak-prefix) orig)))))

(cl-declaim (special
             evil-normal-state-map evil-insert-state-map
             evil-visual-state-map evil-replace-state-map
             evil-operator-state-map evil-motion-state-map))

(eval-after-load
    "evil-maps"
  `(progn
     (mapc
      #'emacspeak-evil-fix-emacspeak-prefix
      (list
       evil-normal-state-map evil-insert-state-map
       evil-visual-state-map evil-replace-state-map
       evil-operator-state-map evil-motion-state-map))
     (emacspeak-keymap-recover-eol)))

(eval-after-load
    "evil-maps"
  `(progn
     (mapc
      #'emacspeak-evil-fix-emacspeak-prefix
      (list
       evil-normal-state-map evil-insert-state-map
       evil-visual-state-map evil-replace-state-map
       evil-operator-state-map evil-motion-state-map))
     (emacspeak-keymap-recover-eol)))

;;}}}
;;{{{ State Hooks:

(defun  emacspeak-evil-state-change-hook  ()
  "State change feedback."
  (cl-declare (special evil-previous-state evil-next-state))
  (when (and evil-previous-state evil-next-state
             (not (eq evil-previous-state evil-next-state)))
    (emacspeak-auditory-icon 'select-object)
    (dtk-notify-speak
     (format "Changing state from %s to %s"
             evil-previous-state evil-next-state))))

(cl-loop
 for hook in
 '(
   evil-normal-state-exit-hook evil-insert-state-exit-hook
   evil-visual-state-exit-hook evil-replace-state-exit-hook
   evil-operator-state-exit-hook evil-motion-state-exit-hook)
 do
 (add-hook hook #'emacspeak-evil-state-change-hook))
(defadvice evil-exit-emacs-state (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-notify-speak "Leaving Emacs state.")))

;;}}}
;;{{{ Additional Commands:

(declare-function evil-mode "evil-core" (&optional flag))

;;;###autoload
(defun emacspeak-evil-toggle-evil ()
  "Interactively toggle evil-mode."
  (interactive)
  (cl-declare (special evil-mode))
  (cl-assert (locate-library "evil") nil "I see no evil!")
  (require 'evil)
  (evil-mode (if evil-mode -1 1))
  (emacspeak-auditory-icon (if evil-mode 'on 'off))
  (message "Turned %s evil-mode"
           (if evil-mode "on" "off")))

;;}}}
(provide 'emacspeak-evil)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

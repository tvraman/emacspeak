;;; emacspeak-outline.el --- Speech enable Outline --   Browsing  Structured Documents  -*- lexical-binding: t; -*-
;;
;; $Author: tv.raman.tv $
;; DescriptionEmacspeak extensions for outline-mode
;; Keywords:emacspeak, audio interface to emacs Outlines
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |raman@crl.dec.com
;; A speech interface to Emacs |
;; $date: $ |
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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;}}}

;;{{{  Introduction:

;;; Commentary:

;; Provide additional advice to outline-mode

;;; Code:

;;}}}
;;{{{ requires

(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'outline)

;;}}}
;;{{{  Navigating through an outline:

(defadvice outline-next-heading (after emacspeak pre act comp)
  "Speak the line."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'section)
    (emacspeak-speak-line)))

(defadvice outline-back-to-heading (after emacspeak pre act comp)
  "Speak the line."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'section)
    (emacspeak-speak-line)))

(defadvice outline-next-visible-heading (after emacspeak pre act comp)
  "Speak the line."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'section)
    (and (looking-at "^$")
         (skip-syntax-backward " "))
    (emacspeak-speak-line)))

(defadvice outline-previous-visible-heading (after emacspeak pre act comp)
  "Speak the line."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'section)
    (emacspeak-speak-line)))

(defadvice outline-up-heading (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice outline-forward-same-level (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'section)
    (emacspeak-speak-line)))

(defadvice outline-backward-same-level (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'section)
    (emacspeak-speak-line)))

;;}}}
;;{{{outline-flag-region:
;; Handle outline hide/show directly here --- rather than relying on
;;overlay advice alone.

(defadvice outline-flag-region (around emacspeak pre act comp)
  "Reflect hide/show via property invisible as wel"
  (defvar ems--voiceify-overlays)
  (let  ((ems--voiceify-overlays  nil)
         (beg (ad-get-arg 0))
         (end (ad-get-arg 1))
         (inhibit-read-only t))
    ad-do-it
    (when (zerop beg) (setq beg (point-min)))
    (with-silent-modifications
      (put-text-property
       beg end 'invisible
       (if (ad-get-arg 2) 'outline nil)))))

;;{{{  Hiding and showing subtrees

(defadvice outline-hide-entry (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Hid the body directly following this heading")))

(defadvice outline-show-entry (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Exposed body directly following current heading")))

(defadvice outline-hide-body (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Hid all of the buffer except for header lines")))

(defadvice outline-show-all (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Exposed all text in the buffer")))

(defadvice outline-hide-subtree (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Hid everything at deeper levels from current heading")))

(defadvice outline-hide-leaves (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Hid all of the body at deeper levels")))

(defadvice outline-show-subtree  (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Exposed everything after current heading at deeper levels")))

(defadvice outline-hide-sublevels (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Hid everything except the top  %s levels"
             (ad-get-arg 0))))

(defadvice outline-hide-other (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Hid everything except current body and parent headings")))

(defadvice outline-show-branches (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Exposed all subheadings while leaving their bodies hidden")))

(defadvice outline-show-children (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Exposed subheadings below current level")))

;;}}}
;;{{{  Interactive speaking of sections

(defvar emacspeak-outline-dont-query-before-speaking t
  "Option to control prompts when speaking  outline sections.")

(defun emacspeak-outline-speak-heading (what direction)
  "Function used by all interactive section speaking
commands. "
  (cl-declare (special emacspeak-outline-query-before-speaking))
  (let ((start nil)
        (end nil))
    (funcall what  direction)
    (setq start (point))
    (save-excursion
      (condition-case nil
          (progn
            (forward-line 1)
            (funcall what 1)
            (setq end (point)))
        (error (setq end (point-max)))))
    (when (or  emacspeak-outline-dont-query-before-speaking
               (y-or-n-p
                (format  "Speak %s lines from section %s"
                         (count-lines start end) (ems--this-line))))
      (emacspeak-speak-region start end))))

(defun emacspeak-outline-speak-next-heading ()
  "Analogous to outline-next-visible-heading,
except that the outline section is  spoken"
  (interactive)
  (emacspeak-auditory-icon 'section)
  (emacspeak-outline-speak-heading 'outline-next-visible-heading 1))

(defun emacspeak-outline-speak-previous-heading ()
  "Analogous to outline-previous-visible-heading,
except that the outline section is  spoken"
  (interactive)
  (emacspeak-auditory-icon 'section)
  (emacspeak-outline-speak-heading 'outline-next-visible-heading -1))

(defun emacspeak-outline-speak-forward-heading ()
  "Analogous to outline-forward-same-level,
except that the outline section is  spoken"
  (interactive)
  (emacspeak-auditory-icon 'section)
  (emacspeak-outline-speak-heading 'outline-forward-same-level 1))

(defun emacspeak-outline-speak-backward-heading ()
  "Analogous to outline-backward-same-level
except that the outline section is  spoken"
  (interactive)
  (emacspeak-auditory-icon 'section)
  (forward-line -1)
  (emacspeak-outline-speak-heading 'outline-forward-same-level -1))

(defun emacspeak-outline-speak-this-heading ()
  "Speak current outline section starting from point"
  (interactive)
  (let ((start (point))
        (end nil))
    (save-excursion
      (condition-case nil
          (progn
            (outline-next-visible-heading 1)
            (setq end (point)))
        (error (setq end (point-max)))))
    (and
     (or emacspeak-outline-dont-query-before-speaking
         (y-or-n-p
          (format "Speak %s lines from section %s"
                  (count-lines start end) (ems--this-line))))
     (emacspeak-speak-region start end))))

;;{{{ bind these in outline mode

(defun emacspeak-outline-setup-keys ()
  "Bind keys in outline minor mode map"
  (cl-declare (special outline-mode-prefix-map
                       outline-navigation-repeat-map))
  (cl-loop
 for map in
 (list outline-mode-prefix-map outline-navigation-repeat-map)
 do
  (define-key map "p" 'emacspeak-outline-speak-previous-heading)
  (define-key map "n" 'emacspeak-outline-speak-next-heading)
  (define-key map "b" 'emacspeak-outline-speak-backward-heading)
  (define-key map "f" 'emacspeak-outline-speak-forward-heading)
  (define-key map " " 'emacspeak-outline-speak-this-heading))
  
  (mapc
   #'(lambda (cmd)
       (put cmd 'repeat-map 'outline-navigation-repeat-map))
   '(emacspeak-outline-speak-next-heading
     emacspeak-outline-speak-backward-heading
     emacspeak-outline-speak-forward-heading
     emacspeak-outline-speak-this-heading)))

(add-hook 'outline-mode-hook 'emacspeak-outline-setup-keys)
(add-hook 'outline-minor-mode-hook 'emacspeak-outline-setup-keys)

;;}}}

;;}}}

;;{{{ Personalities (
(voice-setup-add-map
 '(
   (outline-1 voice-bolden-extra)
   (outline-2 voice-bolden-medium)
   (outline-3 voice-bolden)
   (outline-4 voice-lighten)
   (outline-5 voice-lighten-medium)
   (outline-6 voice-lighten-extra)
   ))

;;}}}
;;{{{ silence errors to help org-mode:

(defadvice outline-up-heading (around emacspeak pre act comp)
  "Silence error messages."
  (ems-with-errors-silenced
   ad-do-it
   ad-return-value))

;;}}}

;;}}}
;;{{{ foldout specific advice

(with-eval-after-load "foldout"
  (defadvice foldout-zoom-subtree (after emacspeak pre act comp)
    "speak about the child we zoomed into"
    (when (ems-interactive-p)
      (emacspeak-auditory-icon 'open-object)
      (message
       "Zoomed into outline %s containing %s lines"
       (ems--this-line) (count-lines (point-min) (point-max)))))

  (defadvice foldout-exit-fold (after emacspeak pre act comp)
    "speak when exiting a fold"
    (when (ems-interactive-p)
      (emacspeak-auditory-icon 'close-object)
      (emacspeak-speak-line))))

;;}}}
(provide  'emacspeak-outline)
;;{{{  emacs local variables

;; local variables:
;; folded-file: t
;; end:

;;}}}

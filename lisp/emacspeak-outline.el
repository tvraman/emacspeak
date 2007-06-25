;;; emacspeak-outline.el --- Speech enable Outline --   Browsing  Structured Documents
;;; $Id$
;;; $Author: tv.raman.tv $
;;; DescriptionEmacspeak extensions for outline-mode
;;; Keywords:emacspeak, audio interface to emacs Outlines
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@crl.dec.com
;;; A speech interface to Emacs |
;;; $date: $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, T. V. Raman
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}

;;{{{  Introduction:

;;; Commentary:

;;; Provide additional advice to outline-mode

;;; Code:

;;}}}
;;{{{ requires
(require 'emacspeak-preamble)
(require 'outline)

;;}}}
;;{{{  Navigating through an outline:

(defadvice outline-next-heading (after emacspeak pre act comp)
  "Speak the line."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice outline-back-to-heading (after emacspeak pre act comp)
  "Speak the line."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice outline-next-visible-heading (after emacspeak pre act comp)
  "Speak the line."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (and (looking-at "^$")
         (skip-syntax-backward " "))
    (emacspeak-speak-line )))

(defadvice outline-previous-visible-heading (after emacspeak pre act comp)
  "Speak the line."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice outline-up-heading (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice outline-forward-same-level (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice outline-backward-same-level (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

;;}}}
;;{{{  Hiding and showing subtrees

(defadvice hide-entry (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Hid the body directly following this heading")))

(defadvice show-entry (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Exposed body directly following current heading")))

(defadvice hide-body (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Hid all of the buffer except for header lines")))

(defadvice show-all (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Exposed all text in the buffer")))

(defadvice hide-subtree (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Hid everything at deeper levels from current heading")))

(defadvice hide-leaves (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Hid all of the body at deeper levels")))

(defadvice show-subtree  (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Exposed everything after current heading at deeper levels")))

(defadvice hide-sublevels (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Hid everything except the top  %s levels"
             (ad-get-arg 0))))

(defadvice hide-other (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Hid everything except current body and parent headings")))

(defadvice show-branches (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Exposed all subheadings while leaving their bodies hidden")))

(defadvice show-children (after emacspeak pre act comp)
  "Produce an auditory icon"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Exposed subheadings below current level")))

;;}}}
;;{{{  Interactive speaking of sections

(defcustom emacspeak-outline-dont-query-before-speaking t
  "*Option to control prompts when speaking  outline
sections."
  :group 'emacspeak-outline
  :type 'boolean)

(defun emacspeak-outline-speak-heading (what direction)
  "Function used by all interactive section speaking
commands. "
  (declare (special emacspeak-outline-query-before-speaking))
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
                         (count-lines start end )
                         (thing-at-point 'line))))
      (emacspeak-speak-region start end ))))

(defun emacspeak-outline-speak-next-heading ()
  "Analogous to outline-next-visible-heading,
except that the outline section is optionally spoken"
  (interactive)
  (emacspeak-outline-speak-heading 'outline-next-visible-heading 1))

(defun emacspeak-outline-speak-previous-heading ()
  "Analogous to outline-previous-visible-heading,
except that the outline section is optionally spoken"
  (interactive)
  (emacspeak-outline-speak-heading 'outline-next-visible-heading -1))

(defun emacspeak-outline-speak-forward-heading ()
  "Analogous to outline-forward-same-level,
except that the outline section is optionally spoken"
  (interactive)
  (emacspeak-outline-speak-heading 'outline-forward-same-level 1))

(defun emacspeak-outline-speak-backward-heading ()
  "Analogous to outline-backward-same-level
except that the outline section is optionally spoken"
  (interactive)
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
                  (count-lines start end)
                  (thing-at-point 'line))))
     (emacspeak-speak-region start end ))))

;;{{{ bind these in outline mode

(defun emacspeak-outline-setup-keys ()
  "Bind keys in outline minor mode map"
  (declare (special outline-mode-prefix-map))
  (define-key outline-mode-prefix-map "p"
    'emacspeak-outline-speak-previous-heading)
  (define-key outline-mode-prefix-map "n"
    'emacspeak-outline-speak-next-heading)
  (define-key outline-mode-prefix-map "b"
    'emacspeak-outline-speak-backward-heading)
  (define-key outline-mode-prefix-map "f"
    'emacspeak-outline-speak-forward-heading)
  (define-key outline-mode-prefix-map " " 'emacspeak-outline-speak-this-heading))

(add-hook 'outline-mode-hook 'emacspeak-outline-setup-keys)
(add-hook 'outline-minor-mode-hook 'emacspeak-outline-setup-keys)

;;}}}

;;}}}
;;{{{ foldout specific advice

(and (locate-library "foldout")
     (require 'foldout))
(defadvice foldout-zoom-subtree (after emacspeak pre act comp)
  "Provide auditory feedback about the child we zoomed into"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Zoomed into outline %s containing %s lines"
             (thing-at-point 'line)
             (count-lines (point-min) (point-max)))))

(defadvice foldout-exit-fold (after emacspeak pre act comp)
  "Provide auditory feedback when exiting a fold"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))

;;}}}
;;{{{ Personalities (
(voice-setup-add-map
 '(
   (outline-1 voice-bolden-extra)
   (outline-2 voice-bolden-medium)
   (outline-3 voice-bolden)
   (outline-4 voice-lighten)
   (outline-5 voice-lighten-medium)
   (outline-6 voice-lighten)
   ))

;;}}}
(provide  'emacspeak-outline)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

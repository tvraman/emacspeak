;;; emacspeak-folding.el --- Speech enable Folding Mode -- enables structured editing  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; DescriptionEmacspeak extensions for folding-mode
;;; Keywords:emacspeak, audio interface to emacs Folding editor
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2018, T. V. Raman
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
;;; Folding mode turns emacs into a folding editor.
;;; Folding mode is what I use:
;;; emacs 19 comes with similar packages, e.g. allout.el
;;; This module defines some advice forms that make folding mode a pleasure to use.
;;; Think of a fold as a container.
;;;
;;; Code:

;;}}}
;;{{{ requires
(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Advice

(cl-loop
 for f in
 '(folding-backward-char folding-forward-char)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak char."
     (when (ems-interactive-p)
       (emacspeak-speak-char t)))))

(defadvice folding-goto-line (after emacspeak pre act)
  "Speak the line. "
  (when (ems-interactive-p)
    (emacspeak-speak-line)))

(defadvice folding-mode (after emacspeak pre act)
  "Provide spoken feedback"
  (when (ems-interactive-p)
    (message "turned %s folding mode"
             (if folding-mode " on " " off"))))

(cl-loop
 for f in
 '(
   folding-context-next-action folding-toggle-show-hide folding-pick-move
   folding-toggle-enter-exit folding-region-open-close
   )do 
 (eval
  `(defadvice ,f (after emacspeak pre act)
     "Produce an auditory icon and then speak the line. "
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'button)
       (emacspeak-speak-line)))))

(cl-loop
 for f in
 '(
   folding-hide-current-subtree folding-hide-current-entry
   folding-shift-out folding-whole-buffer)
 do
 (eval
  `(defadvice  ,f (after emacspeak pre act)
     "Produce an auditory icon.
Then speak the folded line."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon'close-object)
       (emacspeak-speak-line)))))

(cl-loop
 for f in
 '(
   folding-show-all folding-show-current-entry folding-show-current-subtree
   folding-shift-in folding-open-buffer)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act)
     "Produce an auditory icon.
Then speak the  line."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon'open-object)
       (emacspeak-speak-line)))))

(defadvice folding-fold-region (after emacspeak pre act)
  "Produce an auditory icon. "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Specify a meaningful name for the new fold ")))

(cl-loop
 for f in 
 '(folding-previous-visible-heading folding-next-visible-heading)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))

;;}}}
;;{{{ Fix keymap:
(add-hook
 'folding-load-hook
 #'(lambda ()
 
     (cl-declare (special folding-mode-map))
     (when (boundp 'folding-mode-map)
       (define-key folding-mode-map (kbd "C-e") 'emacspeak-prefix-command))))

;;}}}
(provide  'emacspeak-folding)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

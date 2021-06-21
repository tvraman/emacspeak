;;; emacspeak-vdiff.el --- Speech-enable VDIFF  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable VDIFF An Emacs Interface to vdiff
;;; Keywords: Emacspeak,  Audio Desktop vdiff
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
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
;;; MERCHANTABILITY or FITNVDIFF FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; VDIFF ==  vimdiff
;;; Installable from melpa, vdiff enables synchronized movement
;;; through diff buffers without resorting to an extra control-panel
;;; as is the case with ediff.
;;;  In addition to speech-enabling interactive commands and setting
;;;  up face->voice mappings, this module provides commands that speak
;;;  the current hunk. These are bound in @code{vdiff-mode-prefix-map}.
;;; @itemize  @bullet
;;; @item  @code{emacspeak-vdiff-speak-this-hunk} bound to @kbd{SPC}.
;;; @item @code{emacspeak-vdiff-speak-other-hunk} bound to @kbd{C-SPC}.
;;; @item @code{emacspeak-vdiff-speak-other-line} bound to @kbd{l}.
;;;@end itemize
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'vdiff "vdiff" 'no-error)
;;}}}
;;{{{ Map Faces:

(voice-setup-add-map
 '(
   (vdiff-addition-face voice-brighten)
   (vdiff-change-face voice-overlay-1)
   (vdiff-closed-fold-face voice-smoothen)
   (vdiff-open-fold-face voice-lighten)
   (vdiff-refine-added voice-overlay-0)
   (vdiff-refine-changed voice-lighten)
   (vdiff-subtraction-face voice-smoothen)
   (vdiff-subtraction-fringe-face voice-smoothen-extra)
   (vdiff-target-face voice-monotone-extra)))

;;}}}
;;{{{ Emacspeak VDiff Commands:

(defun emacspeak-vdiff-get-overlay-at-point ()
  "Return vdiff overlay  at point."
  (let ((ovr (vdiff--overlay-at-pos)))
    (and (overlayp ovr)
         (overlay-get ovr 'vdiff-type)
         (not (eq (overlay-get ovr 'vdiff-type) 'fold))
         ovr)))

(defun  emacspeak-vdiff-speak-this-hunk ()
  "Speak VDiff hunk under point."
  (interactive)
  (let ((o(emacspeak-vdiff-get-overlay-at-point)))
    (when o (dtk-speak (buffer-substring (overlay-start o) (overlay-end o))))))

(defun emacspeak-vdiff-speak-other-hunk ()
  "Speak corresponding hunk from other buffer."
  (interactive)
  (save-window-excursion
    (save-excursion
      (vdiff-switch-buffer (line-number-at-pos))
      (emacspeak-vdiff-speak-this-hunk))))

(defun emacspeak-vdiff-speak-other-line ()
  "Speak corresponding line from other buffer."
  (interactive)
  (save-window-excursion
    (save-excursion
      (vdiff-switch-buffer (line-number-at-pos))
      (emacspeak-speak-line))))

;;}}}
;;{{{ Interactive Commands:

(cl-loop
 for f in
 '(
   vdiff-receive-changes vdiff-receive-changes-and-step
   vdiff-send-changes vdiff-send-changes-and-step)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'task-done)
       (emacspeak-vdiff-speak-this-hunk)))))

(defadvice vdiff-switch-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defadvice vdiff-refine-all-hunks (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)))
(cl-loop
 for f in
 '(vdiff-buffers vdiff-buffers3 vdiff-magit-compare
                 vdiff-current-file vdiff-files vdiff-files3)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'task-done)
       (emacspeak-speak-mode-line)))))

;;}}}
;;{{{ open/close Folds:
(cl-loop
 for f in
 '(vdiff-open-all-folds vdiff-open-fold)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)
       (emacspeak-speak-line)))))

(cl-loop
 for f in
 '(vdiff-close-all-folds vdiff-close-fold vdiff-close-other-folds)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'close-object)
       (emacspeak-speak-line)))))

;;}}}
;;{{{ Navigation:

;; (defadvice vdiff--scroll-function (around emacspeak pre act comp)
;;   "Silence messages."
;;   (ems-with-messages-silenced ad-do-it))

(cl-loop
 for f in
 '(vdiff-next-fold vdiff-next-hunk vdiff-previous-fold vdiff-previous-hunk)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-vdiff-speak-this-hunk)
       (emacspeak-auditory-icon 'large-movement)))))

;;}}}
;;{{{ Setup:

(eval-after-load
    "vdiff"
  `(progn
     (cl-declare (special vdiff-mode-prefix-map vdiff-mode-map))
     (define-key vdiff-mode-prefix-map "h" 'vdiff-hydra/body)
     (define-key vdiff-mode-map (ems-kbd "C-c") vdiff-mode-prefix-map)
     (define-key vdiff-mode-prefix-map   " " 'emacspeak-vdiff-speak-this-hunk)
     (define-key vdiff-mode-prefix-map   (ems-kbd "C-SPC") 'emacspeak-vdiff-speak-other-hunk)
     (define-key vdiff-mode-prefix-map   (ems-kbd "l") 'emacspeak-vdiff-speak-other-line)))

;;}}}
(provide 'emacspeak-vdiff)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

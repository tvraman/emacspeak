;;; emacspeak-ivy.el --- Speech-enable IVY  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable IVY An Emacs Interface to ivy
;;; Keywords: Emacspeak,  Audio Desktop ivy
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
;;; MERCHANTABILITY or FITNIVY FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; IVY ==  One More Smart Completion Technique 
;;; Speech-enable ivy-style completion.
;;; This is still experimental and preliminary.
;;;
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
   (ivy-action voice-animate)
   (ivy-confirm-face voice-bolden)
   (ivy-current-match voice-lighten)
   (ivy-cursor voice-smoothen)
   (ivy-match-required-face voice-bolden-extra)
   (ivy-minibuffer-match-face-1 voice-monotone-extra)
   (ivy-minibuffer-match-face-2 voice-monotone-medium)
   (ivy-minibuffer-match-face-3 voice-monotone-medium)
   (ivy-minibuffer-match-face-4 voice-monotone-extra)
   (ivy-modified-buffer voice-bolden-and-animate)
   (ivy-remote voice-lighten)
   (ivy-subdir voice-smoothen)
   (ivy-virtual voice-animate)))

;;}}}
;;{{{ Interactive Commands:

(cl-loop
 for f  in
 '(ivy-switch-buffer-other-window ivy-switch-buffer)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (with-current-buffer (window-buffer (selected-window))
         (emacspeak-speak-mode-line))))))


(cl-loop
 for f in 
 '(ivy-done ivy-alt-done ivy-immediate-done)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'close-object)))))

(defun emacspeak-ivy-speak-selection ()
  "Speak current ivy selection."
  (cl-declare (special ivy--length ivy--old-cands ivy--index ivy-text))
  (dtk-speak
   (format
    "%d: %s"
    ivy--length
    (elt ivy--old-cands ivy--index))))

(cl-loop
 for f in
 '(
   ivy-beginning-of-buffer  ivy-end-of-buffer
   ivy-next-line ivy-previous-line)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak selection."
     (when (ems-interactive-p)
       (emacspeak-ivy-speak-selection)
       (emacspeak-auditory-icon 'select-object)))))

(defadvice ivy--exhibit (after emacspeak pre act comp)
  "Speak updated Ivy list."
  (emacspeak-ivy-speak-selection)
  (sit-for 5)
  (emacspeak-speak-rest-of-buffer))

(defadvice ivy-read (before emacspeak pre act comp)
  "Speak prompt"
  (emacspeak-auditory-icon 'open-object)
  (dtk-speak (ad-get-arg 0)))

;;}}}
(provide 'emacspeak-ivy)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

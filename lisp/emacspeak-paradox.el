;;; emacspeak-paradox.el --- Speech-enable PARADOX  -*- lexical-binding: t; -*-
;;; $Id: emacspeak-paradox.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable PARADOX An Emacs Interface to paradox
;;; Keywords: Emacspeak,  Audio Desktop paradox
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
;;; MERCHANTABILITY or FITNPARADOX FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; PARADOX == paradox.el Improved package management interface
;;; Manage Emacs packages.
;;; This module speech-enables paradox.el with a few convenience commands.

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'paradox "paradox" 'no-error)
(require 'calendar)

;;}}}
;;{{{ Map Faces:

(voice-setup-add-map
 '(
   (paradox-name-face voice-bolden)
   (paradox-download-face voice-smoothen)
   (paradox-description-face voice-lighten)
   (paradox-description-face-multiline voice-monotone-extra)
   (paradox-comment-face voice-monotone)
   (paradox-star-face voice-animate)
   (paradox-starred-face voice-bolden-and-animate)
   (paradox-archive-face voice-smoothen)
   (paradox-commit-tag-face voice-brighten)
   (paradox-highlight-face voice-animate)
   (paradox-homepage-button-face voice-bolden-medium)))

;;}}}
;;{{{ Additional Commands

(defun emacspeak-paradox-summarize-line ()
  "Succinct Summary."
  (interactive)
  (let* ((entry   (get-text-property (point) 'tabulated-list-entry))
         (name (aref entry 0))
         (desc (aref entry 5))
         (stars (aref entry  4))
         (state (aref entry 2)))
    (unless (zerop (length stars))
      (setq stars (concat "stars: " stars)))
    (cond
     ((string= state "installed") (emacspeak-auditory-icon 'select-object))
     ((string= state "built-in") (emacspeak-auditory-icon 'mark-object))
     ((string= state "dependency") (emacspeak-auditory-icon 'close-object))
     ((string= state "obsolete") (emacspeak-auditory-icon 'deselect-object))
     ((string= state "incompat") (emacspeak-auditory-icon 'alert-user))
     (t (emacspeak-auditory-icon 'item)))
    (dtk-speak-and-echo  (concat name ": "desc stars))))

(defun emacspeak-paradox-mode-hook ()
  "Emacspeak setup hook for paradox-mode."
  (cl-declare (special paradox-menu-mode-map))
  (define-key paradox-menu-mode-map (ems-kbd "<left>") 'emacspeak-speak-previous-field)
  (define-key paradox-menu-mode-map (ems-kbd "<right>") 'emacspeak-speak-next-field)
  (define-key paradox-menu-mode-map " " 'emacspeak-paradox-summarize-line)
  (emacspeak-pronounce-add-buffer-local-dictionary-entry
   emacspeak-pronounce-date-yyyymmdd-pattern
   (cons 're-search-forward 'emacspeak-pronounce-yyyymmdd-date))
  (emacspeak-auditory-icon 'open-object)
  (emacspeak-speak-mode-line))

(add-hook 'paradox-menu-mode-hook 'emacspeak-paradox-mode-hook)

;;}}}
;;{{{ Managing Packages:

(defadvice paradox-menu-execute(around emacspeak pre act comp)
  "Silence messages while installing packages. "
  (ems-with-messages-silenced ad-do-it)
  (emacspeak-speak-message-again))

(cl-loop
 for f in
 '(paradox-next-entry paradox-previous-entry)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-paradox-summarize-line)))))

;;}}}
;;{{{ Advice:

(defadvice paradox-quit-and-close (after emacspeak pre act comp)
  "provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))


(cl-loop
 for f in
 '(
   paradox-sort-by-package paradox-sort-by-status
   paradox-sort-by-version paradox-sort-by-★) do
 (eval
  `(defadvice ,f  (after emacspeak pre act comp)
     "Speak after done."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'task-done)))))

;;}}}
(provide 'emacspeak-paradox)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

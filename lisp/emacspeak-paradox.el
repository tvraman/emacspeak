;;; emacspeak-paradox.el --- Speech-enable PARADOX  -*- lexical-binding: t; -*-
;; $Id: emacspeak-paradox.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;; $Author: tv.raman.tv $
;; Description:  Speech-enable PARADOX An Emacs Interface to paradox
;; Keywords: Emacspeak,  Audio Desktop paradox
;;;   LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; 
;;  $Revision: 4532 $ |
;; Location https://github.com/tvraman/emacspeak
;; 

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
;; MERCHANTABILITY or FITNPARADOX FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Commentary:
;; PARADOX == paradox.el Improved package management interface
;; Manage Emacs packages.
;; This module speech-enables paradox.el with a few convenience commands.

;;   Required modules

(eval-when-compile (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'paradox "paradox" 'no-error)
(require 'calendar)

;;;  Map Faces:

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

;;;  Additional Commands

(defun emacspeak-paradox-summarize-line ()
  "Succinct Summary."
  (interactive)
  (let* ((entry   (tabulated-list-get-entry))
         (name (aref entry 0))
         (desc (aref entry 5))
         (state (aref entry 2)))
    (cond
     ((string= state "installed") (emacspeak-icon 'mark-object))
     ((string= state "built-in") (emacspeak-icon 'select-object))
     ((string= state "dependency") (emacspeak-icon 'close-object))
     ((string= state "obsolete") (emacspeak-icon 'deselect-object))
     ((string= state "incompat") (emacspeak-icon
                                  'alert-user))
     (t (emacspeak-icon 'doc)))
    (dtk-speak
     (concat
      (propertize name 'personality voice-animate) "  "desc))))

(defun emacspeak-paradox-mode-hook ()
  "Emacspeak setup hook for paradox-mode."
  (cl-declare (special paradox-menu-mode-map))
  (define-key paradox-menu-mode-map " " 'emacspeak-paradox-summarize-line)
  (emacspeak-pronounce-add-buffer-local-dictionary-entry
   emacspeak-pronounce-date-yyyymmdd-pattern
   (cons 're-search-forward 'emacspeak-pronounce-yyyymmdd-date))
  (emacspeak-icon 'open-object)
  (emacspeak-speak-mode-line))

(add-hook 'paradox-menu-mode-hook 'emacspeak-paradox-mode-hook)

;;;  Managing Packages:

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

;;;  Advice:

(defadvice paradox-quit-and-close (after emacspeak pre act comp)
  "provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-icon 'close-object)
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
       (emacspeak-icon 'task-done)))))

;;;  Commit Navigation:
(cl-loop
 for f in 
 '(paradox-next-commit paradox-previous-commit)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-icon 'select-object)
       (emacspeak-tabulated-list-speak-cell)))))

(defadvice paradox-menu-view-commit-list (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice fparadox-commit-list-visit-commit (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-icon 'open-object)
    (emacspeak-speak-line)))

(provide 'emacspeak-paradox)
;;;  end of file


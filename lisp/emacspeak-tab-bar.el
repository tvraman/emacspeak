;;; emacspeak-tab-bar.el --- Speech-enable tab-bar  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable tab-bar An Emacs Interface to tab-bar
;;; Keywords: Emacspeak,  Audio Desktop tab-bar
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
;;; MERCHANTABILITY or FITNtab-bar FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:

;;; tab-bar == tabs for window configuration.
;;; Speech-enable tab-bar interaction.  If you have
;;; @var{browse-url-new-window-flag} set to T to have EWW open Web
;;; pages in a new buffer, then set
;;; @var{eww-browse-url-new-window-is-tab} to nil to avoid leaking
;;; tabs.

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
  (tab-bar voice-bolden)
  (tab-bar-tab voice-animate)
  (tab-bar-tab-inactive voice-smoothen)
  (tab-line voice-lighten)))

;;}}}
;;{{{Helpers:



(defsubst emacspeak-tab-bar-speak-tab-name ()
  "Speak name of current tab."
  (emacspeak-auditory-icon 'tick-tick)
  (dtk-notify-speak
   (format "%s"
             (alist-get 'name (alist-get 'current-tab (tab-bar-tabs))))))

;;}}}
;;{{{ Interactive Commands:

(defadvice tab-bar-switch-to-tab (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-tab-bar-speak-tab-name)))

(cl-loop
 for f in 
 '(
   tab-next tab-previous tab-select
   tab-bar-select-tab tab-bar-select-tab-by-name
   tab-bar-switch-to-next-tab tab-bar-switch-to-prev-tab
   tab-bar-switch-to-recent-tab)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'select-object)
       (emacspeak-tab-bar-speak-tab-name)))))


(cl-loop
 for f in 
 '(
   tab-bar-close-other-tabs tab-bar-close-tab
   tab-close tab-close-other)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'close-object)
       (emacspeak-tab-bar-speak-tab-name)))))

(cl-loop
 for f in 
 '(tab-new tab-bar-new-tab)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)
       (emacspeak-tab-bar-speak-tab-name)))))

(defadvice tab-bar-close-tab-by-name (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (dtk-speak (message "Closed tab %s" (ad-get-arg  0)))
    (emacspeak-auditory-icon 'close-object)))

;;}}}
;;{{{tab-list commands:

(cl-loop
 for f in 
 '(tab-list tab-bar-list)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)))))

(defadvice tab-bar-list-execute (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(cl-loop
 for f in 
 '(tab-bar-list-prev-line tab-bar-list-next-line)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))

(defadvice tab-bar-list-unmark (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'unmark-object)
    (emacspeak-speak-line)))


(cl-loop
 for f in 
 '(tab-bar-list-delete  tab-bar-list-delete-backwards)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'delete-object)
       (emacspeak-speak-line)))))

(defadvice tab-bar-list-select (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

;;}}}
(provide 'emacspeak-tab-bar)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

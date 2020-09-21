;;; emacspeak-elscreen.el --- Speech-enable ELSCREEN  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable ELSCREEN An Emacs Interface to elscreen
;;; Keywords: Emacspeak,  Audio Desktop elscreen
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
;;; MERCHANTABILITY or FITNELSCREEN FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; ELSCREEN ==  Emacs Window Session Manager
;;; Speech-enable interactive commands.
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map faces:

(voice-setup-add-map
 '
 ((elscreen-tab-current-screen-face voice-bolden)
  (elscreen-tab-other-screen-face voice-smoothen)))

;;}}}
;;{{{ Advice interactive commands:
(cl-loop
 for f in
 '(
   elscreen-jump-0 elscreen-jump-1 elscreen-jump-2 elscreen-jump-3
   elscreen-jump-4 elscreen-jump-5 elscreen-jump-6 elscreen-jump-7
   elscreen-jump-8 elscreen-jump-9
   elscreen-toggle elscreen-swap elscreen-select-and-goto
   elscreen-previous elscreen-next elscreen-jump
   elscreen-goto elscreen-find-file-read-only elscreen-find-file
   elscreen-find-and-goto-by-buffer elscreen-execute-extended-command elscreen-dired
   elscreen-clone elscreen-create)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'window-resize)
       (dtk-notify-using-voice
        voice-smoothen
        (or
         (elscreen-get-screen-nickname  (elscreen-get-current-screen))
         (buffer-name)))
       (emacspeak-speak-mode-line)))))

(cl-loop
 for f in
 '(elscreen-kill elscreen-kill-others elscreen-kill-screen-and-buffers)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'close-object)
       (emacspeak-speak-mode-line))))
 )

;;}}}
;;{{{ Override:  Display screen list

(defadvice elscreen-display-screen-name-list (around emacspeak pre act comp)
  "Display and Audio format the list of screens in mini-buffer."
  (interactive)
  (let ((screen-list (sort (elscreen-get-screen-list) '<))
        (screen-to-name-alist (elscreen-get-screen-to-name-alist))
        (msg nil))
    (setq msg
          (mapconcat
           (lambda (screen)
             (ems-with-messages-silenced
              (let ((screen-name (assoc-default screen screen-to-name-alist)))
                (concat
                 (propertize (format "%d" screen) 'face  'font-lock-keyword-face)
                 (elscreen-status-label screen "")
                 (propertize screen-name 'face 'font-lock-string-face)))))
           screen-list "  "))
    (dtk-speak-and-echo msg)))

;;}}}
(provide 'emacspeak-elscreen)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

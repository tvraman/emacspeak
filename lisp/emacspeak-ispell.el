;;; emacspeak-ispell.el --- Speech enable Ispell -- Emacs' interactive spell checker
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak extension to speech enable ispell
;;; Keywords: Emacspeak, Ispell, Spoken Output, Ispell version 2.30
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Introduction:

;;; This module speech enables ispell.
;;; Implementation note: This is hard because of how  ispell.el is written
;;; Namely, all of the work is done by one huge hairy function.
;;; This makes advising it hard.

;;; Original version of this extension was written under emacs-19.28
;;; for ispell.el version 2.30
;;; Now updating it for ispell.el version 2.37.
;;; Support for 2.30 will wither away

;;}}}
;;{{{ requires
(require 'emacspeak-preamble)

;;}}}
;;{{{  define personalities

(voice-setup-add-map
 '(
   (ispell-highlight-face voice-bolden)
   ))

;;}}}
;;{{{  first set up voice  highlighting

(defadvice ispell-highlight-spelling-error (after emacspeak act )
  "Use voice locking to highlight the error.
Will clobber any existing personality property defined on start end"
  (let ((start (ad-get-arg 0))
        (end (ad-get-arg 1 ))
        (highlight (ad-get-arg 2 )))
    (if highlight
        (put-text-property  start end
                            'personality  voice-bolden)
      (put-text-property start end
                         'personality  nil ))))

;;}}}
;;{{{  ispell command loop:

;;;Signature for  ispell-command-loop in 2.30
;;;defun ispell-command-loop (miss guess word)
;;;Signature in 2.37:
;;; defun ispell-command-loop (miss guess word start end)

;;; Advice speaks the line containing the error with the erroneous
;;; word highlighted.

;;{{{  new version
(defgroup emacspeak-ispell nil
  "Spell checking group."
  :group  'emacspeak)

(defcustom emacspeak-ispell-max-choices 10
  "Emacspeak will not speak the choices if there are more than this
many available corrections."
  :type 'number
  :group 'emacspeak-ispell)

(defadvice ispell-command-loop (before emacspeak pre act )
  "Speak the line containing the incorrect word.
 Then speak  the possible corrections. "
  (let ((scratch-buffer (get-buffer-create " *dtk-scratch-buffer* "))
        (choices  (ad-get-arg 0 ))
        (line nil)
        (start (ad-get-arg 3))
        (end (ad-get-arg 4))
        (position 0))
    (setq line
          (ems-set-personality-temporarily start end voice-bolden
                                           (thing-at-point
                                            'line)))
    (save-excursion
      (set-buffer scratch-buffer)
      (setq voice-lock-mode t)
      (setq buffer-undo-list t)
      (dtk-set-punctuations 'all)
      (erase-buffer)
      (insert line)
      (cond
       ((< (length choices)
           emacspeak-ispell-max-choices)
        (loop for choice in choices
              do
              (insert (format "%s %s\n" position choice))
              (incf position)))
       (t (insert
           (format "There were %s corrections available."
                   (length choices)))))
      (modify-syntax-entry 10 ">")
      (dtk-speak (buffer-string )))))

;;}}}

(defadvice ispell-comments-and-strings (around emacspeak pre act comp)
  "Stop chatter by turning off messages"
  (cond
   ((interactive-p)
    (let ((dtk-stop-immediately t )
          (voice-lock-mode t)
          (emacspeak-speak-messages nil))
      ad-do-it
      (emacspeak-auditory-icon 'task-done)))
   (t ad-do-it)))

(defadvice ispell-help (before emacspeak pre act)
  "Speak the help message. "
  (let ((dtk-stop-immediately nil))
    (dtk-speak (documentation 'ispell-help ))))

;;}}}
;;{{{  Advice top-level ispell commands:

(defadvice ispell-buffer (around emacspeak pre act comp)
  "Produce auditory icons for ispell."
  (cond
   ((interactive-p)
    (let ((dtk-stop-immediately t )
          (voice-lock-mode t)
          (emacspeak-speak-messages nil))
      ad-do-it
      (emacspeak-auditory-icon 'task-done)))
   (t ad-do-it))
  ad-return-value)

(defadvice ispell-region (around emacspeak pre act comp)
  "Produce auditory icons for ispell."
  (cond
   ((interactive-p)
    (let ((dtk-stop-immediately t )
          (voice-lock-mode t)
          (emacspeak-speak-messages nil))
      ad-do-it
      (emacspeak-auditory-icon 'task-done)))
   (t ad-do-it))
  ad-return-value)

(defadvice ispell-word (around emacspeak pre act comp)
  "Produce auditory icons for ispell."
  (declare (special emacspeak-last-message))
  (cond
   ((interactive-p)
    (let ((dtk-stop-immediately t )
          (voice-lock-mode t)
          (emacspeak-speak-messages nil))
      (setq emacspeak-last-message nil)
      ad-do-it
      (when (interactive-p)
        (emacspeak-speak-message-again))
      (emacspeak-auditory-icon 'task-done)))
   (t ad-do-it))
  ad-return-value)

;;}}}
(provide 'emacspeak-ispell)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

;;; emacspeak-iswitchb.el --- speech-enable iswitchb buffer selection
;;; $Id$
;;; $Author$
;;; Description:   extension to speech enable iswitchb
;;; Keywords: Emacspeak, Audio Desktop
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision: 24.0 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1995 -- 2004, T. V. Raman<raman@cs.cornell.edu>
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

;;{{{ required modules

(require 'emacspeak-preamble)

;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; speech-enable iswitchb.el
;;; This is an interesting task since most of the value-add
;;; provided by package iswitchb.el  is visual feedback.
;;; Speech UI Challenge: What  is the most efficient means of
;;; conveying a dynamically updating set of choices?
;;; current strategy is to walk the list using c-s and c-r as
;;; provided by iswitchb

;;; Code:

;;}}}
;;{{{ speech-enable feedback routines

(defadvice iswitchb-exhibit (after emacspeak pre act comp)
  "Speak first of the displayed matches."
  (dtk-speak
   (format
    "%s %d Choices: %s"
    (car iswitchb-matches)
    (length iswitchb-matches)
    (or iswitchb-text ""))))

;;}}}
;;{{{ speech-enable interactive commands:

(defadvice iswitchb-toggle-case (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon
     (if iswitchb-case 'on 'off))
    (dtk-speak
     (format "Case %s"
             (if iswitchb-case 'on 'off)))))

(defadvice iswitchb-toggle-regexp (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon
     (if iswitchb-regexp 'on 'off))
    (dtk-speak
     (format "Case %s"
             (if iswitchb-regexp 'on 'off)))))

(defadvice iswitchb-toggle-ignore (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon
     (if iswitchb-ignore 'on 'off))
    (dtk-speak
     (format "Case %s"
             (if iswitchb-ignore 'on 'off)))))

(defadvice iswitchb-complete (after emacspeak pre act comp)
  "Speak completion at the head of the list."
  (when (interactive-p)
    (dtk-speak (car iswitchb-matches))))

(defadvice  iswitchb-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

;;; note that though these are after advice fragments,
;;; iswitchb-matches does not reflect the change at the time we
;;; get called.
;;; hence the off-by-one hack

(defadvice iswitchb-next-match (after emacspeak pre act comp)
  "Speak match at the front of the list."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (dtk-speak
     (second iswitchb-matches))))

(defadvice iswitchb-prev-match (after emacspeak pre act comp)
  "Speak match at the front of the list."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (dtk-speak
     (car (last iswitchb-matches)))))
(defadvice iswitchb-kill-buffer (after emacspeak pre act comp)
  "Provide auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)))

;;}}}
;;{{{ update keybindings

(defun emacspeak-iswitchb-keys ()
  "Add emacspeak keybindings for iswitchb.
Place this on hook iswitchb-minibuffer-setup-hook."
  (declare (special iswitchb-mode-map))
  (define-key iswitchb-mode-map  [left]  'iswitchb-prev-match)
  (define-key iswitchb-mode-map  [right]  'iswitchb-next-match))
;;}}}
(provide 'emacspeak-iswitchb)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

;;; emacspeak-erc.el --- speech-enable erc irc client
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak module for speech-enabling erc.el
;;; Keywords: Emacspeak, erc
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision$ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1999 T. V. Raman <raman@cs.cornell.edu>
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

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'advice)
(require 'thingatpt)
(require 'emacspeak-speak)
(require 'voice-lock)
(require 'emacspeak-sounds)

;;}}}
;;{{{  Introduction:

;;; Commentary:
;;; erc.el is a modern Emacs client for IRC including color
;;; and font locking support. 
;;; erc.el - an Emacs IRC client (by Alexander L. Belikoff)
;;; http://www.cs.cmu.edu/~berez/irc/erc.el

;;}}}
;;{{{  variables

(declaim (special emacspeak-sounds-directory))

(add-to-list 'erc-sound-path emacspeak-sounds-directory)

;;}}}
;;{{{ personalities 

(defvar emacspeak-erc-default-personality 'paul
  "Default personality for erc.")

(defvar emacspeak-erc-direct-msg-personality
  'paul-animated
  "Personality for direct messages.")

(defvar emacspeak-erc-input-personality 
  'paul-smooth
  "Personality for input.")

(defvar emacspeak-erc-bold-personality
  'paul-bold
  "Personality for bold in erc.")

(defvar emacspeak-erc-inverse-personality
  'betty
  "Inverse personality in ERC.")

(defvar emacspeak-erc-underline-personality  'ursula
  "Persnality for underlining in erc.")

(defvar emacspeak-erc-prompt-personality  'harry
  "Personality for prompting in erc.")
(defvar emacspeak-erc-notice-personality 
  'paul-italic
  "Personality for notices in Erc.")

(defvar emacspeak-erc-action-personality 
  'paul-monotone
  "Personality for actions in erc.")

(defvar emacspeak-erc-error-personality 
  'kid
  "Personality for errors n ERC.")

(defvar emacspeak-erc-host-danger-personality 
  'paul-surprized
  "Personality for marking dangerous hosts.")

(defvar emacspeak-erc-pal-personality
  'paul-animated
  "Personality for marking pals.")

;;}}}
;;{{{  helpers

;;}}}
;;{{{ advice interactive commands
(defadvice erc-mode (after emacspeak pre act comp)
  "Turn on voice lock mode."
  (declare (special voice-lock-mode))
  (setq voice-lock-mode t))

(defadvice erc-select (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))
(defadvice erc-send-current-line (after emacspeak pre act
                                        comp)
  "Provide auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))
(defadvice erc-send-paragraph (after emacspeak pre act
                                        comp)
  "Provide auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'paragraph)))

;;}}}
(provide 'emacspeak-erc)
;;{{{ advice for voicefication 

(defadvice erc-highlight-error (after emacspeak pre act
                                      comp)
  "Apply aural highlighting as well."
  (put-text-property 0 (length ad-return-value)
'personality emacspeak-erc-error-personality
ad-return-value)
  ad-return-value)

(defadvice erc-highlight-notice (after emacspeak pre act
                                       comp)
  "Apply aural highlighting as well."
  (put-text-property 0 (length ad-return-value)
                     'personality emacspeak-erc-notice-personality
                     ad-return-value)
  ad-return-value)

;;}}}
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

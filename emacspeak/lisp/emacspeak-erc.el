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
(require 'custom)
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

(defgroup emacspeak-erc nil
  "Emacspeak extension for IRC client ERC."
  :group 'emacspeak
  :prefix "emacspeak-erc-")

(defcustom emacspeak-erc-default-personality 'paul
  "Default personality for erc."
  :type 'symbol
  :group 'emacspeak-erc)

(defcustom emacspeak-erc-direct-msg-personality
  'paul-animated
  "Personality for direct messages."
  :type 'symbol
  :group 'emacspeak-erc)

(defcustom emacspeak-erc-input-personality 
  'paul-smooth
  "Personality for input."
  :type 'symbol
  :group 'emacspeak-erc)

(defcustom emacspeak-erc-bold-personality
  'paul-bold
  "Personality for bold in erc."
  :type 'symbol
  :group 'emacspeak-erc)

(defcustom emacspeak-erc-inverse-personality
  'betty
  "Inverse personality in ERC."
  :type 'symbol
  :group 'emacspeak-erc)

(defcustom emacspeak-erc-underline-personality  'ursula
  "Persnality for underlining in erc."
  :type 'symbol
  :group 'emacspeak-erc)

(defcustom emacspeak-erc-prompt-personality  'harry
  "Personality for prompting in erc."
  :type 'symbol
  :group 'emacspeak-erc)

(defcustom emacspeak-erc-notice-personality 
  'paul-italic
  "Personality for notices in Erc."
  :type 'symbol
  :group 'emacspeak-erc)

(defcustom emacspeak-erc-action-personality 
  'paul-monotone
  "Personality for actions in erc."
  :type 'symbol
  :group 'emacspeak-erc)

(defcustom emacspeak-erc-error-personality 
  'kid
  "Personality for errors n ERC."
  :type 'symbol
  :group 'emacspeak-erc)

(defcustom emacspeak-erc-host-danger-personality 
  'paul-surprized
  "Personality for marking dangerous hosts."
  :type 'symbol
  :group 'emacspeak-erc)

(defcustom emacspeak-erc-pal-personality
  'paul-animated
  "Personality for marking pals."
  :type 'symbol
  :group 'emacspeak-erc)

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
;;{{{ monitoring chatrooms 
(defvar emacspeak-erc-room-monitor nil
  "*Local to each chat room. If turned on,
user is notified about activity in the room.")
(make-variable-buffer-local 'emacspeak-erc-room-monitor)



(defvar emacspeak-erc-people-to-monitor nil
  "List of strings specifying people to monitor in a given room.")

(make-variable-buffer-local
 'emacspeak-erc-people-to-monitor)


(defsubst emacspeak-erc-read-person (action)
  "Helper to prompt for and read person in ERC."
  (read-from-minibuffer
   (format "%s person" action)
   (save-excursion
     (search-backward  "<" (point-min) nil)
     (thing-at-point 'sexp))))

(defun emacspeak-erc-add-name-to-monitor (name)
  "Add people to monitor in this room."
  (interactive
   (list
    (emacspeak-erc-read-person "Add ")))
  (declare (special emacspeak-erc-people-to-monitor))
  (unless (eq major-mode 'erc-mode)
    (error "Not in an ERC buffer."))
  (pushnew name emacspeak-erc-people-to-monitor
           :test #'string-equal)
  (emacspeak-auditory-icon 'select-object)
  (message "monitoring %s"
           (mapconcat #'identity 
                      emacspeak-erc-people-to-monitor " ")))

(defun emacspeak-erc-delete-name-from-monitor (name)
  "Remove name to monitor in this room."
  (interactive
   (list
    (emacspeak-erc-read-person "Delete ")))
  (declare (special emacspeak-erc-people-to-monitor))
  (unless (eq major-mode 'erc-mode)
    (error "Not in an ERC buffer."))
  (setq emacspeak-erc-people-to-monitor
        (remove-if
         (function
          (lambda (x)
            (string-equal x name)))
         emacspeak-erc-people-to-monitor))
  (emacspeak-auditory-icon 'delete-object)
  (message "monitoring %s"
           (mapconcat #'identity 
                      emacspeak-erc-people-to-monitor " ")))


(defun emacspeak-erc-compute-message (string buffer)
  "Uses environment of buffer to decide what message to
display. String is the original message."
  (declare (special emacspeak-erc-people-to-monitor))
  (let ((who-from (car (split-string string )))
        (case-fold-search t))
    (cond
     ((and
       (not (string-match "^\\*\\*\\*" who-from))
       emacspeak-erc-people-to-monitor
       (find
        who-from
        emacspeak-erc-people-to-monitor
        :test #'string-equal))
      string)
     (t nil))))



(defadvice erc-display-line-buffer  (after emacspeak pre act comp)
  (declare (special emacspeak-erc-room-monitor))
  (let ( (buffer (ad-get-arg 1))
         (case-fold-search t))
    (save-excursion
      (set-buffer buffer)
      (when emacspeak-erc-room-monitor
        (let ((emacspeak-speak-messages nil)
              (msg (emacspeak-erc-compute-message (ad-get-arg 0)
                                                  buffer)))
          (when msg
            (emacspeak-auditory-icon 'progress)
            (message msg)
            (tts-with-punctuations dtk-punctuation-mode
            (dtk-speak  msg))))))))

(defun emacspeak-erc-toggle-room-monitor  (&optional prefix)
  "Toggle state of ERC room monitor.
Interactive 
PREFIX arg means toggle the global default value, and then
set the current local value to the result."

  (interactive  "P")
  (declare  (special  emacspeak-erc-room-monitor))
  (cond
   (prefix
    (setq-default  emacspeak-erc-room-monitor
                   (not  (default-value 'emacspeak-erc-room-monitor )))
    (setq emacspeak-erc-room-monitor (default-value 'emacspeak-comint-autospeak )))
   (t
    (setq emacspeak-erc-room-monitor
          (not emacspeak-erc-room-monitor ))))
  (and emacspeak-erc-room-monitor
       
       )
  (emacspeak-auditory-icon
   (if emacspeak-erc-room-monitor 'on 'off))
  (message "Turned %s room monitor  %s "
           (if emacspeak-erc-room-monitor "on" "off" )
	   (if prefix "" "locally")))

;;}}}
;;{{{ define emacspeak keys
(declaim (special erc-mode-map))
(define-key erc-mode-map "\C-c\C-m"
  'emacspeak-erc-toggle-room-monitor)
(define-key erc-mode-map "\C-c\C-a"
  'emacspeak-erc-add-name-to-monitor)
(define-key erc-mode-map "\C-c\C-d" 'emacspeak-erc-delete-name-from-monitor)
;;}}}
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

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
(require 'emacspeak-preamble)
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

;;}}}
;;{{{ personalities 

(defgroup emacspeak-erc nil
  "Emacspeak extension for IRC client ERC."
  :group 'emacspeak
  :prefix "emacspeak-erc-")

(defcustom emacspeak-erc-ignore-notices nil
  "Set to T if you dont want to see notifcation messages from the
server."
  :type 'boolean
  :group 'eamcspeak-erc)

(def-voice-font emacspeak-erc-direct-msg-personality
  voice-animate
  'erc-direct-msg-face
  "Personality for direct messages.")

(def-voice-font  emacspeak-erc-input-personality 
  voice-smoothen
  'erc-input-face
  "personality for input.")

(def-voice-font emacspeak-erc-bold-personality
  voice-bolden 'erc-bold-face
  "Bold personality for ERC.")

(def-voice-font emacspeak-erc-inverse-personality
  voice-lighten-extra  'erc-inverse-face
  "Inverse highlight in ERC.")

(def-voice-font emacsepak-erc-underline 'underline
  'erc-underline-face
  "Underline in ERC.")

(def-voice-font emacspeak-erc-prompt-personality voice-bolden
  'erc-prompt-face
  "Personality for prompts.")

(def-voice-font emacspeak-erc-notice-personality
  'inaudible
  'erc-notice-face
  "Personality for notices.")

(def-voice-font emacspeak-erc-action-personality
  voice-monotone
  'erc-action-face
  "Personality for actions.")

(def-voice-font emacspeak-erc-error-face
  voice-bolden-and-animate 'erc-error-face
  "Error personality for ERC.")

(def-voice-font emacspeak-erc-dangerous-host-personality 
  voice-brighten-extra 'erc-dangerous-host-face
  "Personality for dangerous hosts.")

      
(def-voice-font emacspeak-erc-pal-personality 
  voice-animate-extra 'erc-pal-face
  "Personality for pals.")
    
(def-voice-font emacspeak-erc-keyword-personality 
  voice-animate 'erc-keyword-face
  "Personality for keywords.")

;;}}}
;;{{{  helpers

;;}}}
;;{{{ advice interactive commands
(declaim (special emacspeak-pronounce-internet-smileys-pronunciations))
(emacspeak-pronounce-augment-pronunciations 'erc-mode
                                            emacspeak-pronounce-internet-smileys-pronunciations)

(defadvice erc-mode (after emacspeak pre act comp)
  "Turn on voice lock mode."
  (declare (special voice-lock-mode))
  (emacspeak-pronounce-refresh-pronunciations)
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
;;{{{ monitoring chatrooms 
(defvar emacspeak-erc-room-monitor nil
  "*Local to each chat room. If turned on,
user is notified about activity in the room.")
(make-variable-buffer-local 'emacspeak-erc-room-monitor)

(defvar emacspeak-erc-people-to-monitor nil
  "List of strings specifying people to monitor in a given room.")

(make-variable-buffer-local
 'emacspeak-erc-people-to-monitor)

(defvar emacspeak-erc-monitor-my-messages t
  "If T, then messages to your specified nick will be
spoken.")

(make-variable-buffer-local 'emacspeak-erc-monitor-my-messages)

(defcustom emacspeak-erc-my-nick nil
  "My IRC nick."
  :type 'string
  :group 'emacspeak-erc)

(defsubst emacspeak-erc-read-person (action)
  "Helper to prompt for and read person in ERC."
  (read-from-minibuffer
   (format "%s person" action)
   (save-excursion
     (let ((start (point)))
       (search-backward  "<" (point-min) t)
       (when (not (= start (point)))
	 (setq start (point))
	 (search-forward " ")
	 (buffer-substring start (1- (point))))))))
     

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
  (declare (special emacspeak-erc-people-to-monitor
                    emacspeak-erc-my-nick
                    emacspeak-erc-monitor-my-messages))
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
     ((and emacspeak-erc-monitor-my-messages
           (stringp emacspeak-erc-my-nick)
           (string-match emacspeak-erc-my-nick string))
      string)
     (t nil))))

(defadvice erc-display-line-buffer  (after emacspeak pre act
                                           comp)
  "Speech-enable ERC."
  (declare (special emacspeak-erc-room-monitor
                    emacspeak-erc-monitor-my-messages
                    emacspeak-erc-my-nick))
  (let ( (buffer (ad-get-arg 1))
         (case-fold-search t))
    (save-excursion
      (set-buffer buffer)
      (when (and emacspeak-erc-room-monitor
		 emacspeak-erc-monitor-my-messages)
        (let ((emacspeak-speak-messages nil)
              (msg (emacspeak-erc-compute-message (ad-get-arg 0)
                                                  buffer)))
          (when msg
            (emacspeak-auditory-icon 'progress)
            (message msg)
            (tts-with-punctuations dtk-punctuation-mode
				   (dtk-speak  msg))))))))

(defadvice erc-display-line-1  (after emacspeak pre act comp)
  "Speech-enable ERC."
  (declare (special emacspeak-erc-room-monitor
                    emacspeak-erc-monitor-my-messages
                    emacspeak-erc-my-nick))
  (let ( (buffer (ad-get-arg 1))
         (case-fold-search t))
    (save-excursion
      (set-buffer buffer)
      (when (and emacspeak-erc-room-monitor
		 emacspeak-erc-monitor-my-messages)
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

(defun emacspeak-erc-toggle-my-monitor  (&optional prefix)
  "Toggle state of ERC  monitor of my messages.
Interactive PREFIX arg means toggle the global default value, and then
set the current local value to the result."
  (interactive  "P")
  (declare  (special  emacspeak-erc-monitor-my-messages))
  (cond
   (prefix
    (setq-default  emacspeak-erc-monitor-my-messages
                   (not  (default-value 'emacspeak-erc-monitor-my-messages )))
    (setq emacspeak-erc-monitor-my-messages (default-value 'emacspeak-comint-autospeak )))
   (t
    (setq emacspeak-erc-monitor-my-messages
          (not emacspeak-erc-monitor-my-messages ))))
  (and emacspeak-erc-monitor-my-messages
       
       )
  (emacspeak-auditory-icon
   (if emacspeak-erc-monitor-my-messages 'on 'off))
  (message "Turned %s messages monitor  %s "
           (if emacspeak-erc-monitor-my-messages "on" "off" )
	   (if prefix "" "locally")))

;;}}}
;;{{{ silence server messages 

(defadvice erc-parse-line-from-server (around emacspeak pre
                                              act comp)
  "Silence server messages."
  (let ((emacspeak-speak-messages nil))
    ad-do-it
    ad-return-value))

(defadvice erc-make-notice (around emacspeak  pre act comp)
  "Ignore notices from server is emacspeak-erc-ignore-notices it set."
  ad-do-it
  (cond
   ((not emacspeak-erc-ignore-notices ) ad-return-value)
   (t " ")))

;;}}}
;;{{{ define emacspeak keys
(declaim (special erc-mode-map))
(define-key erc-mode-map "\C-cm"
  'emacspeak-erc-toggle-my-monitor)
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

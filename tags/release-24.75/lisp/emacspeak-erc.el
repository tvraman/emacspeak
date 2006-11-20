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

;;{{{  Introduction:

;;; Commentary:
;;; erc.el is a modern Emacs client for IRC including color
;;; and font locking support. 
;;; erc.el - an Emacs IRC client (by Alexander L. Belikoff)
;;; http://www.cs.cmu.edu/~berez/irc/erc.el

;;; Code:

;;}}}
;;{{{ required modules
(require 'emacspeak-preamble)
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
  "Set to T if you dont want to see notification  messages from the
server."
  :type 'boolean
  :group 'emacspeak-erc)

(voice-setup-add-map
 '(
   (erc-direct-msg-face voice-animate)
   (erc-input-face voice-smoothen)
   (erc-bold-face voice-bolden)
   (erc-inverse-face voice-lighten-extra)
   (erc-underline-face voice-brighten-medium)
   (erc-prompt-face voice-bolden)
   (erc-notice-face (quote inaudible))
   (erc-action-face voice-monotone)
   (erc-error-face voice-bolden-and-animate)
   (erc-dangerous-host-face voice-brighten-extra)
   (erc-pal-face voice-animate-extra)
   (erc-keyword-face voice-animate)
   ))

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

(defcustom emacspeak-erc-my-nick ""
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
     
(defun emacspeak-erc-add-name-to-monitor (name &optional
                                               quiten-pronunciation)
  "Add people to moniter in this room.
Optional interactive prefix  arg defines a pronunciation that
  silences speaking of this perso's name."
  (interactive
   (list
    (emacspeak-erc-read-person "Add ")
    current-prefix-arg))
  (declare (special emacspeak-erc-people-to-monitor))
  (unless (eq major-mode 'erc-mode)
    (error "Not in an ERC buffer."))
  (pushnew name emacspeak-erc-people-to-monitor
           :test #'string-equal)
  (when quiten-pronunciation
    (emacspeak-pronounce-add-buffer-local-dictionary-entry name ""))
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
(defcustom emacspeak-erc-speak-all-participants nil
  "Speak all things said if t."
  :type 'boolean
  :group 'emacspeak-erc)

(make-variable-buffer-local 'emacspeak-erc-speak-all-participants)

(defun emacspeak-erc-compute-message (string buffer)
  "Uses environment of buffer to decide what message to
display. String is the original message."
  (declare (special emacspeak-erc-people-to-monitor
                    emacspeak-erc-my-nick
                    emacspeak-erc-speak-all-participants
                    emacspeak-erc-monitor-my-messages))
  (let ((who-from (car (split-string string )))
        (case-fold-search t))
    (cond
     (emacspeak-erc-speak-all-participants string)
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

(ems-generate-switcher
 'emacspeak-erc-toggle-speak-all-participants
 'emacspeak-erc-speak-all-participants
 "Toggle state of ERC speak all participants..
Interactive 
PREFIX arg means toggle the global default value, and then
set the current local value to the result.")

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

(ems-generate-switcher 'emacspeak-erc-toggle-room-monitor
                       'emacspeak-erc-room-monitor
                       "Toggle state of ERC room monitor.
Interactive 
PREFIX arg means toggle the global default value, and then
set the current local value to the result.")

(ems-generate-switcher 'emacspeak-erc-toggle-my-monitor
                       'emacspeak-erc-monitor-my-messages
                       "Toggle state of ERC  monitor of my messages.
Interactive PREFIX arg means toggle the global default value, and then
set the current local value to the result.")

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
(define-key erc-mode-map "\C-c "
  'emacspeak-erc-toggle-speak-all-participants)
(define-key erc-mode-map "\C-cm"
  'emacspeak-erc-toggle-my-monitor)
(define-key erc-mode-map "\C-c\C-m"
  'emacspeak-erc-toggle-room-monitor)
(define-key erc-mode-map "\C-c\C-a"
  'emacspeak-erc-add-name-to-monitor)
(define-key erc-mode-map "\C-c\C-d" 'emacspeak-erc-delete-name-from-monitor)
;;}}}
 
;;{{{ cricket rules 
(defvar emacspeak-erc-cricket-bowling-figures-pattern
  " [0-9]+-[0-9]+-[0-9]+-[0-9] "
  "Pattern for matching bowling figures.")

(defun emacspeak-erc-cricket-convert-bowling-figures (pattern)
  "Pronounce bowling figures in cricket."
  (let ((fields (split-string pattern "-")))
    (format " %s for %s off %s overs with %s maidens "
            (cond
             ((string-equal "0" (fourth fields)) 
              "none")
             (t (fourth fields)))
            (third fields)
            (first fields)
            (cond
             ((string-equal "0" (second fields)) 
              "no")
             (t (second fields))))))

(defvar emacspeak-erc-cricket-4-6-pattern
  " [0-9]+x\[46]"
  "Matches pattern used to  score number of fours and sixes in IRC #cricket.")

(defun emacspeak-erc-cricket-convert-4-6-pattern (pattern)
  "Convert 4/6 pattern for IRC cricket channels."
  (format "%s %s"
          (substring pattern 0 -2)
          (cond
           ((string-equal "4" 
                          (substring pattern -1))
            "fours")
           (t "sixes"))))
(defun emacspeak-erc-setup-cricket-rules ()
  "Set up #cricket channels."
  (interactive)
  (emacspeak-pronounce-add-buffer-local-dictionary-entry
   "km/h," " kays, ")
  (emacspeak-pronounce-add-buffer-local-dictionary-entry
   emacspeak-erc-cricket-bowling-figures-pattern
   (cons 're-search-forward
         'emacspeak-erc-cricket-convert-bowling-figures))
  (emacspeak-pronounce-add-buffer-local-dictionary-entry
   emacspeak-erc-cricket-4-6-pattern
   (cons 're-search-forward
         'emacspeak-erc-cricket-convert-4-6-pattern))
  (emacspeak-pronounce-add-buffer-local-dictionary-entry
   " [0-9]+nb"
   (cons
    're-search-forward
    #'(lambda (pattern)
        (format "%s no balls "
                (substring pattern 0 -2)))))
  (emacspeak-pronounce-add-buffer-local-dictionary-entry
   "[0-9]+b"
   (cons
    're-search-forward
    #'(lambda (pattern)
        (format "%s balls "
                (substring pattern 0 -1)))))
  (emacspeak-pronounce-add-buffer-local-dictionary-entry
   " [0-9]+w "
   (cons
    're-search-forward
    #'(lambda (pattern)
        (format "%s wides "
                (substring pattern 0 -1)))))
  (dtk-set-punctuations 'some))

;;}}}
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

;;; emacspeak-jabber.el --- Speech-Enable jabber  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description: speech-enable jabber
;;; Keywords: Emacspeak, jabber
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2008-04-15 06:25:36 -0700 (Tue, 15 Apr 2008) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;; 

;;}}}
;;{{{  Copyright:

;;; Copyright (c) 1995 -- 2021, T. V. Raman
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ Introduction:

;;; Commentary:
;;; emacs-jabber.el implements a  jabber client for emacs
;;; emacs-jabber is hosted at sourceforge.
;;; I use emacs-jabber with my gmail.com account

;;; Code:

;;}}}
;;{{{  Required modules

(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(with-no-warnings (require 'jabber "jabber" 'no-error))
;;}}}
;;{{{ map voices

(voice-setup-add-map
 '(
   (jabber-activity-face        voice-animate-extra)
   (jabber-chat-error           voice-monotone)
   (jabber-chat-prompt-foreign  voice-brighten-medium)
   (jabber-chat-prompt-local    voice-smoothen-medium)
   (jabber-chat-prompt-system   voice-brighten-extra)
   (jabber-chat-text-foreign    voice-lighten) 
   (jabber-chat-text-local      voice-smoothen)
   (jabber-rare-time-face       voice-animate-extra)
   (jabber-roster-user-away     voice-smoothen-extra)
   (jabber-roster-user-chatty   voice-brighten)
   (jabber-roster-user-dnd      voice-lighten-medium)
   (jabber-roster-user-error    voice-monotone)
   (jabber-roster-user-offline  voice-smoothen-extra)
   (jabber-roster-user-online   voice-bolden)
   (jabber-roster-user-xa       voice-lighten)
   (jabber-title-large          voice-bolden-extra)
   (jabber-title-medium         voice-bolden)
   (jabber-title-small          voice-lighten)
   ))
;;}}}
;;{{{ Advice interactive commands:

(defadvice jabber-switch-to-roster-buffer (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{ silence keepalive

(cl-loop
 for f in
 '(
   image-type jabber-chat-with jabber-chat-with-jid-at-point
   jabber-keepalive-do jabber-fsm-handle-sentinel jabber-xml-resolve-namespace-prefixes
   jabber-process-roster jabber-keepalive-got-response)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Silence  messages."
     (ems-with-messages-silenced
      ad-do-it
      ad-return-value))))

;;}}}
;;{{{ jabber activity:

(defadvice jabber-activity-switch-to (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{ chat buffer:

(defadvice jabber-chat-buffer-send (after emacspeak pre act comp)
  "Produce auditory icon."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)))

;;}}}
;;{{{ alerts

(defcustom emacspeak-jabber-speak-presence-alerts nil
  "Set to T if you want to hear presence alerts."
  :type  'boolean
  :group 'emacspeak-jabber)
(defadvice jabber-send-default-presence (after emacspeak pre act
                                               comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Sent default presence.")))

(defadvice jabber-send-away-presence (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Set to be away.")))

(defadvice jabber-send-xa-presence (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Set extended  away.")))

(defadvice jabber-go-to-next-jid (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice jabber-go-to-previous-jid (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defun emacspeak-jabber-presence-default-message (&rest _ignore)
  "Default presence alert used by Emacspeak.
Silently drops alerts on the floor --- Google Talk is too chatty otherwise."
  nil)
(cl-declaim (special jabber-alert-presence-message-function))
(setq
 jabber-alert-presence-message-function
 #'emacspeak-jabber-presence-default-message)

;;; this is what I use as my jabber alert function:
(defun emacspeak-jabber-message-default-message (from buffer text)
  "Speak the message."
  (cl-declare (special jabber-message-alert-same-buffer))
  (when (or jabber-message-alert-same-buffer
            (not (memq (selected-window) (get-buffer-window-list buffer))))
    (emacspeak-auditory-icon 'item)
    (dtk-notify-speak
     (if (jabber-muc-sender-p from)
         (format "Private message from %s in %s"
                 (jabber-jid-resource from)
                 (jabber-jid-displayname (jabber-jid-user from)))
       (format "%s: %s" (jabber-jid-displayname from) text)))))

;;{{{ interactive commands:

(defun emacspeak-jabber-popup-roster ()
  "Pop to Jabber roster."
  (interactive)
  (cl-declare (special jabber-roster-buffer jabber-connections))
  (unless jabber-connections  (call-interactively 'jabber-connect))
  (unless (buffer-live-p jabber-roster-buffer) (call-interactively 'jabber-display-roster))
  (pop-to-buffer jabber-roster-buffer)
  (goto-char (point-min))
  (forward-line 4)
  (emacspeak-auditory-icon 'select-object)
  (emacspeak-speak-line))

(defadvice jabber-connect-all (after emacspeak pre act comp)
  "switch to roster so we give it a chance to update."
  (when (ems-interactive-p) (switch-to-buffer jabber-roster-buffer)))

(defadvice jabber-roster-update (around emacspeak    pre act  comp)
  "Make this operation a No-Op unless the roster is visible."
  (when (get-buffer-window-list jabber-roster-buffer) ad-do-it))

(defadvice jabber-display-roster (around emacspeak    pre act  comp)
  "Make this operation a No-Op unless called interactively."
  (when (ems-interactive-p) ad-do-it))

(add-hook 'jabber-post-connect-hook 'jabber-switch-to-roster-buffer)

;;}}}
(defun emacspeak-jabber-connected ()
  "Function to add to jabber-post-connection-hook."
  (emacspeak-auditory-icon 'task-done)
  (dtk-notify-say "Connected to jabber."))
(add-hook 'jabber-post-connect-hook #'emacspeak-jabber-connected)

;;}}}
;;{{{ Pronunciations
(cl-declaim (special emacspeak-pronounce-internet-smileys-pronunciations))
(emacspeak-pronounce-augment-pronunciations 'jabber-chat-mode
                                            emacspeak-pronounce-internet-smileys-pronunciations)
(emacspeak-pronounce-augment-pronunciations 'jabber-mode
                                            emacspeak-pronounce-internet-smileys-pronunciations)

;;}}}
;;{{{ Browse chat buffers:
(defun emacspeak-jabber-chat-speak-this-message(&optional copy-as-kill)
  "Speak chat message under point.
With optional interactive prefix arg `copy-as-kill', copy it to
the kill ring as well."
  (interactive "P")
  (let ((range (emacspeak-speak-range)))
    (when copy-as-kill (kill-new range))
    (dtk-speak range)))

(defun emacspeak-jabber-chat-next-message ()
  "Move forward to and speak the next message in this chat session."
  (interactive)
  (cl-assert  (eq major-mode 'jabber-chat-mode) nil  "Not in a Jabber chat buffer.")
  (end-of-line)
  (goto-char (next-single-property-change (point) 'face nil(point-max)))
  (while (and (not (eobp))
              (or (null (get-text-property (point) 'face))
                  (get-text-property (point) 'field)))
    (goto-char (next-single-property-change (point) 'face  nil  (point-max))))
  (cond
   ((eobp)
    (message "On last message")
    (emacspeak-auditory-icon 'warn-user))
   (t(emacspeak-auditory-icon 'select-object)
     (emacspeak-speak-range))))

(defun emacspeak-jabber-chat-previous-message ()
  "Move backward to and speak the previous message in this chat session."
  (interactive)
  (cl-assert (eq major-mode 'jabber-chat-mode) nil "Not in a Jabber chat buffer.")
  (beginning-of-line)
  (goto-char (previous-single-property-change (point) 'face nil  (point-min)))
  (while  (and (not (bobp))
               (or (null (get-text-property (point) 'face))
                   (get-text-property (point) 'field)))
    (goto-char (previous-single-property-change (point) 'face  nil  (point-min))))
  (cond
   ((bobp)
    (message "On first message")
    (emacspeak-auditory-icon 'warn-user))
   (t(emacspeak-auditory-icon 'select-object)
     (emacspeak-speak-range))))

(when (boundp 'jabber-chat-mode-map)
  (cl-loop
   for k in
   '(
     ("M-n" emacspeak-jabber-chat-next-message)
     ("M-p" emacspeak-jabber-chat-previous-message)
     ("M-SPC " emacspeak-jabber-chat-speak-this-message))
   do
   (emacspeak-keymap-update  jabber-chat-mode-map k)))

;;}}}
;;{{{ Speak recent message:

(defun emacspeak-jabber-speak-recent-message ()
  "Speak most recent message if one exists."
  (interactive)
  (cl-declare (special jabber-activity-jids))
  (cond
   (jabber-activity-jids
    (save-excursion
      (jabber-activity-switch-to)
      (goto-char (point-max))
      (emacspeak-jabber-chat-previous-message)))
   (t (message "No recent message."))))

;;}}}
(provide 'emacspeak-jabber)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

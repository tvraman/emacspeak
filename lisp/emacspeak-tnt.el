;;; emacspeak-tnt.el --- Instant Messenger 
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable AOL Instant Messenger Client TNT
;;; Keywords: Emacspeak, Instant Messaging 
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

;;; Copyright (C) 1995 -- 2007, T. V. Raman<raman@cs.cornell.edu>
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

;;; Speech-enables TNT -- the Emacs AOL Instant Messenger
;;; client 

;;; Code:

;;}}}
;;{{{ Advice interactive commands

(defadvice tnt-kill (after emacspeak pre act comp)
  "Provide additional auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)))
(declaim (special emacspeak-pronounce-internet-smileys-pronunciations))
(emacspeak-pronounce-augment-pronunciations 'tnt-im-mode
                                            emacspeak-pronounce-internet-smileys-pronunciations)
(emacspeak-pronounce-augment-pronunciations 'tnt-chat-mode
                                            emacspeak-pronounce-internet-smileys-pronunciations)

(defadvice tnt-open (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (timer-activate tnt-idle-timer)
    (emacspeak-pronounce-refresh-pronunciations)
    (emacspeak-auditory-icon 'open-object)))

(defadvice tnt-im (after emacspeak pre act comp)
  "Provide auditory feedback."
  (emacspeak-pronounce-refresh-pronunciations)
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice tnt-send-text-as-instant-message (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))

(defadvice tnt-join-chat (after emacspeak pre act comp)
  "Provide auditory feedback."
  (emacspeak-pronounce-refresh-pronunciations)
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice tnt-send-text-as-chat-message (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Sent chat message.")))

(defadvice tnt-send-text-as-chat-whisper (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Sent whisper message.")))

(defadvice tnt-send-text-as-chat-invitation (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Sent invitation message.")))

(defadvice tnt-show-chat-participants (around emacspeak pre
                                              act comp)
  "Speak the participant list. "
  (cond
   ((interactive-p)
    (let ((start (point)))
      ad-do-it
      (emacspeak-speak-region start (point))))
   (t ad-do-it))
  ad-return-value)

(defadvice tnt-show-buddies (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice tnt-next-buddy (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))
(defadvice tnt-prev-buddy (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice tnt-next-group (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice tnt-prev-group (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice tnt-edit-buddies (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice tnt-save-buddy-list (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'save-object)
    (emacspeak-speak-mode-line)))

(defadvice tnt-accept (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))

(defadvice tnt-reject (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'warn-user)))

(defadvice tnt-prev-event (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))

(defadvice tnt-next-event (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))

;;}}}

;;{{{  advice builtins
(defadvice tnt-push-event (after emacspeak pre act comp)
  "Alert user to event being pushed."
  (message (ad-get-arg 0))
  (emacspeak-auditory-icon 'item))

(defadvice tnt-im-mode (after emacspeak pre act comp)
  "Turn on outline minor mode to enable navigation. "
  (outline-minor-mode 1)
  (setq outline-regexp "^\\([0-9:]+ \\)?[0-9a-zA-Z]+: ")
  (define-key tnt-im-mode-map
    (concat emacspeak-prefix "\C-q")
    'emacspeak-tnt-toggle-autospeak)
  (define-key tnt-im-mode-map "\M-p"
    'emacspeak-outline-speak-previous-heading)
  (define-key tnt-im-mode-map "\M-n" 'emacspeak-outline-speak-next-heading))

;;}}}
;;{{{  autospeak messages
(defcustom emacspeak-tnt-autospeak t
  "True means messages in this chat session will be spoken
automatically."
  :type 'boolean
  :group 'emacspeak-tnt)

(make-variable-buffer-local 'emacspeak-tnt-autospeak)

(defun emacspeak-tnt-toggle-autospeak (&optional prefix)
  "Toggle TNT autospeak for this chat session."
  (interactive  "P")
  (declare  (special  emacspeak-tnt-autospeak ))
  (cond
   (prefix
    (setq-default  emacspeak-tnt-autospeak
                   (not  (default-value 'emacspeak-tnt-autospeak )))
    (setq emacspeak-tnt-autospeak (default-value 'emacspeak-tnt-autospeak )))
   (t (make-local-variable 'emacspeak-tnt-autospeak)
      (setq emacspeak-tnt-autospeak
            (not emacspeak-tnt-autospeak ))))
  (when (interactive-p)
    (emacspeak-auditory-icon
     (if emacspeak-tnt-autospeak 'on 'off))
    (message "Turned %s TNT autospeak  %s "
             (if emacspeak-tnt-autospeak "on" "off" )
             (if prefix "" " locally"))))

(defadvice tnt-append-message-and-adjust-window (after emacspeak pre act comp)
  "Speak messages if autospeak is on, and the conversation buffer is selected."
  (let ((buffer  (ad-get-arg 0))
        (message (tnt-strip-html (ad-get-arg 1))))
    (when (and emacspeak-tnt-autospeak
               (eq (current-buffer)
                   buffer))
      (dtk-speak message))))

;;}}}
;;{{{ set up face to voice mapping
(voice-setup-add-map
 '(
   (tnt-my-name-face voice-smoothen)
   (tnt-buddy-list-active-face voice-animate)
   (tnt-buddy-list-inactive-face voice-monotone-medium)
   (tnt-buddy-list-idle-face voice-smoothen-medium)
   (tnt-buddy-list-away-face voice-monotone)
   (tnt-buddy-list-pounce-face (quote ursula))
   ))
;;}}}
;;{{{  Activate pronunciations 
(add-hook 'tnt-buddy-list-mode-hook
          'emacspeak-pronounce-refresh-pronunciations)

;;}}}
;;{{{ avoid chatter
(defadvice tnt-handle-closed(around emacspeak pre act comp)
  "Silence messages."
  (let ((emacspeak-speak-messages nil))
    ad-do-it))

(defadvice tnt-handle-opened(around emacspeak pre act comp)
  "Silence messages."
  (let ((emacspeak-speak-messages nil))
    ad-do-it))
(defadvice tnt-handle-sign-on(around emacspeak pre act comp)
  "Silence messages."
  (let ((emacspeak-speak-messages nil))
    ad-do-it))

;;}}}
(provide 'emacspeak-tnt)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

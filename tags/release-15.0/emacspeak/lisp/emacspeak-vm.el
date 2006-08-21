;;; emacspeak-vm.el --- Speech enable VM -- A powerful mail agent (and the one I use)
;;; $Id$
;;; $Author$ 
;;; Description:  Emacspeak extension to speech enhance vm
;;; Keywords: Emacspeak, VM, Email, Spoken Output, Voice annotations
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

;;;Copyright (C) 1995 -- 2001, T. V. Raman 
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
(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'custom)
(require 'dtk-voices)
(require 'voice-lock)
(require 'emacspeak-keymap)
(require 'dtk-speak)
(require 'emacspeak-sounds)
(require 'emacspeak-speak)
;;{{{  Introduction:
;;; This module extends the mail reader vm.
;;; Uses voice locking for message headers and cited messages

;;}}}
;;{{{ voice locking:

(defgroup  emacspeak-vm nil
  "VM mail reader on the Emacspeak Desktop."
  :group 'emacspeak
  :group 'vm
  :prefix "emacspeak-vm-")


(defcustom emacspeak-vm-voice-lock-messages nil
  "Set this to T if you want messages automatically voice locked.
Note that some badly formed mime messages  cause trouble."
  :type 'boolean
  :group 'emacspeak-vm)

(defvar vm-voice-lock-keywords nil
  "Keywords to highlight in vm")

(defvar vm-summary-voice-lock-keywords
  "Additional expressions to highlight in vm  Summary mode.")

;;;  Set vm-voice-lock-keywords

(setq vm-voice-lock-keywords
      (append vm-voice-lock-keywords
              '(("^From: \\(.*\\)$" 1  emacspeak-vm-from-voice )
                ("^To: \\(.*\\)$" 1 emacspeak-vm-to-voice)
                ("^Subject: \\(.*\\)$" 1 emacspeak-vm-subject-voice)
                ("^|?[a-zA-Z]*>+\\(.*\\)$" 1 emacspeak-vm-cite-voice )
                )))

(voice-lock-set-major-mode-keywords 'vm-mode      'vm-voice-lock-keywords)

(voice-lock-set-major-mode-keywords 'vm-summary-mode
                                    'vm-summary-voice-lock-keywords)

(add-hook 'vm-mode-hook
          (function
           (lambda ()
             (setq dtk-punctuation-mode "some")
             (when dtk-allcaps-beep
               (dtk-toggle-allcaps-beep))
             (emacspeak-dtk-sync)
             (when emacspeak-vm-voice-lock-messages
               (condition-case nil
                   (voice-lock-mode 1)
                 (error nil ))))))

;;}}}
;;{{{  vm voices:

(defcustom emacspeak-vm-from-voice  'harry
  "Personality for From field. "
  :type 'symbol
  :group 'emacspeak-vm)

(defcustom emacspeak-vm-to-voice  'paul-animated
  "Personality for To field. "
  :type 'symbol
  :group 'emacspeak-vm)
(defcustom emacspeak-vm-subject-voice  'paul-smooth 
  "Personality for Subject field. "
  :type 'symbol
  :group 'emacspeak-vm)

(defcustom emacspeak-vm-cite-voice  'paul-smooth
  "Personality for citation lines. "
  :type 'symbol
  :group 'emacspeak-vm)

;;}}}
;;{{{ inline helpers

(defsubst emacspeak-vm-number-of (message) (aref (aref message 1) 0))

;;}}}
;;{{{ Advice completions

(defadvice vm-minibuffer-complete-word (around emacspeak pre act)
  "Say what you completed."
  (let ((prior (point ))
        (dtk-stop-immediately t))
    (emacspeak-kill-buffer-carefully "*Completions*")
    ad-do-it
    (let ((completions-buffer (get-buffer "*Completions*")))
      (if (> (point) prior)
          (dtk-speak (buffer-substring prior (point )))
        (when (and completions-buffer
                   (window-live-p (get-buffer-window completions-buffer )))
          (save-excursion
            (set-buffer completions-buffer )
            (emacspeak-prepare-completions-buffer)
            (dtk-speak (buffer-string ))))))
    ad-return-value))

(defadvice vm-minibuffer-complete-word-and-exit (around emacspeak pre act)
  "Say what you completed."
  (let ((prior (point ))
        (dtk-stop-immediately t))
    (emacspeak-kill-buffer-carefully "*Completions*")
    ad-do-it
    (let ((completions-buffer (get-buffer "*Completions*")))
      (if (> (point) prior)
          (dtk-speak (buffer-substring prior (point )))
        (when (and completions-buffer
                   (window-live-p (get-buffer-window completions-buffer )))
          (save-excursion
            (set-buffer completions-buffer )
            (emacspeak-prepare-completions-buffer)
            (dtk-speak (buffer-string ))))))
    ad-return-value))

;;}}}
;;{{{  Helper functions:
(defvar emacspeak-vm-user-full-name (user-full-name)
  "Full name of user using this session")

(defvar emacspeak-vm-user-login-name  (user-login-name)
  "Login name of this user")

(defun emacspeak-vm-summarize-message ()
  "Summarize the current vm message. "
  (declare (special vm-message-pointer
                    emacspeak-vm-user-full-name emacspeak-vm-user-login-name))
  (when vm-message-pointer
    (dtk-stop)
    (let*  ((dtk-stop-immediately t )
            (message (car vm-message-pointer ))
            (number (emacspeak-vm-number-of  message))
            (from(or (vm-su-full-name message)
                     (vm-su-from message )))
            (subject (vm-so-sortable-subject message ))
            (to(or (vm-su-to-names message)
                   (vm-su-to message )))
            (self-p (or
                     (string-match emacspeak-vm-user-full-name to)
                     (string-match  (user-login-name) to)))
            (lines (vm-su-line-count message)))
      (dtk-speak
       (format "%s %s %s   %s %s "
               number
               (or from "")
               (if subject (format "on %s" subject) "")
               (if (and to (< (length to) 80))
                   (format "to %s" to) "")
               (if lines (format "%s lines" lines) "")))
      (cond 
       ((and self-p
             (= 0 self-p)                    ) ;mail to me and others 
        (emacspeak-auditory-icon 'item))
       (self-p                          ;mail to others including me
        (emacspeak-auditory-icon 'mark-object))
       (t                               ;got it because of a mailing list
        (emacspeak-auditory-icon 'select-object ))))))

(defun emacspeak-vm-speak-labels ()
  "Speak a message's labels"
  (interactive)
  (declare (special vm-message-pointer))
  (when vm-message-pointer
    (message "Labels: %s"
             (vm-su-labels (car vm-message-pointer )))))

(defun emacspeak-vm-mode-line ()
  "VM mode line information. "
  (interactive)
  (declare (special vm-ml-message-attributes-alist
                    vm-ml-message-read vm-ml-message-unread
                    vm-virtual-folder-definition
                    vm-ml-message-new
                    vm-ml-message-number vm-ml-highest-message-number ))
  (dtk-stop)
  (emacspeak-dtk-sync)
  (let ((dtk-stop-immediately nil ))
    (when (buffer-modified-p )
      (dtk-tone 700 70))
    (cond
     (vm-virtual-folder-definition
      (dtk-speak
       (format "Message %s of %s from virtual folder %s"
               vm-ml-message-number vm-ml-highest-message-number
               (car vm-virtual-folder-definition))))
     (t (dtk-speak 
         (format "Message %s of %s,    %s %s %s  %s"
                 vm-ml-message-number vm-ml-highest-message-number
                 (if vm-ml-message-new "new" "")
                 (if vm-ml-message-unread "unread" "")
                 (if vm-ml-message-read "read" "")
                 (mapconcat
                  (function (lambda(item)
                              (let ((var (car item))
                                    (value (cadr item )))
                                (cond
                                 ((and (boundp var) (eval var ))
                                  (if (symbolp value)
                                      (eval value)
                                    value))
                                 (t "")))))
                  (cdr vm-ml-message-attributes-alist)   " ")))))))
    
    

;;}}}
;;{{{  Moving between messages

(add-hook 'vm-select-message-hook
          (function (lambda nil 
                      (emacspeak-vm-summarize-message))))

;;}}}
;;{{{  Scrolling messages:

(defun emacspeak-vm-locate-subject-line()
  "Locates the subject line in a message being read.
Useful when you're reading a message
that has been forwarded multiple times."
  (interactive)
  (re-search-forward "^Subject:" nil t )
  (emacspeak-speak-line))

(defadvice vm-scroll-forward (after emacspeak pre act)
  "Produce auditory feedback.
Then speak the screenful. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (save-excursion
      (let ((start  (point ))
            (window (get-buffer-window (current-buffer ))))
        (forward-line (window-height window))
        (emacspeak-speak-region start (point ))))))

(defadvice vm-scroll-backward (after emacspeak pre act)
  "Produce auditory feedback.
Then speak the screenful. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (save-excursion
      (let ((start  (point ))
            (window (get-buffer-window (current-buffer ))))
        (forward-line(-  (window-height window)))
        (emacspeak-speak-region start (point ))))))
(defun emacspeak-vm-browse-message ()
  "Browse an email message --read it paragraph at a time. "
  (interactive)
  (emacspeak-execute-repeatedly 'forward-paragraph ))

(declaim (special vm-mode-map))
(eval-when (load)
  (load-library "vm-vars")
  (emacspeak-keymap-remove-emacspeak-edit-commands
   vm-mode-map))

(declaim (special vm-mode-map))
(define-key vm-mode-map  "j"
  'emacspeak-hide-or-expose-all-blocks)
(define-key vm-mode-map  "\M-g" 'vm-goto-message)
(define-key vm-mode-map "J" 'vm-discard-cached-data)
(define-key vm-mode-map "." 'emacspeak-vm-browse-message)
(define-key vm-mode-map "," 'emacspeak-speak-rest-of-buffer)
(define-key vm-mode-map "'" 'emacspeak-speak-rest-of-buffer)
;;}}}
;;{{{  deleting and killing

(defadvice vm-delete-message (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (message "Message discarded.")))

(defadvice vm-undelete-message (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p)
    (message "Message recovered.")))

(defadvice vm-kill-subject (after emacspeak pre act)
  "Provide auditory feedback. "
  (when (interactive-p)
    (dtk-speak "Killed this thread ")
    (emacspeak-auditory-icon 'delete-object)))

;;}}}
;;{{{  Sending mail:

(defadvice vm-forward-message (around emacspeak pre act)
  "Provide aural feedback."
  (if (interactive-p)
      (let ((dtk-stop-immediately nil))
        (message "Forwarding message")
        (emacspeak-vm-summarize-message)
        ad-do-it
        (emacspeak-speak-line ))
    ad-do-it)
  ad-return-value )

(defadvice vm-reply (after emacspeak pre act)
  "Provide aural feedback."
  (when (interactive-p)
    (emacspeak-speak-mode-line)))

(defadvice vm-followup (after emacspeak pre act)
  "Provide aural feedback."
  (when (interactive-p)
    (message "Folluwing up")
    (emacspeak-speak-mode-line)))
      
(defadvice vm-reply-include-text (after emacspeak pre act)
  "Provide aural feedback."
  (when (interactive-p)
    (emacspeak-speak-mode-line )))

(defadvice vm-followup-include-text (after emacspeak pre act)
  "Provide aural feedback."
  (when (interactive-p)
    (message "Following up")
    (emacspeak-speak-mode-line )))
(defadvice vm-mail-send (after emacspeak pre act comp)
  "Provide auditory context"
  (when  (interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'close-object)))

(defadvice vm-mail-send-and-exit (after emacspeak pre act comp)
  "Provide auditory context"
  (when  (interactive-p)
    (emacspeak-auditory-icon 'close-object)))

(defadvice vm-mail (after emacspeak pre act)
  "Provide aural feedback."
  (when (interactive-p)
    (let ((dtk-stop-immediately nil))
      (message "Composing a message")
      (emacspeak-speak-line ))))

;;}}}
;;{{{ quitting


(defadvice vm-quit (after emacspeak pre act )
  "Provide an auditory icon if requested"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)))

;;}}}
;;{{{  Keybindings:
(declaim  (special vm-mode-map
                   global-map
                   emacspeak-prefix
                   emacspeak-keymap))
(define-key vm-mode-map "\M-j" 'emacspeak-vm-locate-subject-line)
(define-key vm-mode-map "\M-l" 'emacspeak-vm-speak-labels)
(define-key vm-mode-map
  (concat emacspeak-prefix "m")
  'emacspeak-vm-mode-line)
;;}}}
;;{{{ advise searching:
(defadvice vm-isearch-forward (around emacspeak pre act comp)
  "Provide auditory feedback"
  (declare (special vm-message-pointer))
  (cond
   ((interactive-p)
    (let ((orig (point)))
      ad-do-it
      (cond
       ((not (= orig (point)))
        (emacspeak-auditory-icon 'search-hit)
        (emacspeak-speak-line))
       (t (emacspeak-auditory-icon 'search-miss)))))
   (t ad-do-it))
  ad-return-value)


(defadvice vm-isearch-backward (around emacspeak pre act comp)
  "Provide auditory feedback"
  (declare (special vm-message-pointer))
  (cond
   ((interactive-p)
    (let ((orig (point)))
      ad-do-it
      (cond
       ((not (= orig (point)))
        (emacspeak-auditory-icon 'search-hit)
        (emacspeak-speak-line))
       (t (emacspeak-auditory-icon 'search-miss)))))
   (t ad-do-it))
  ad-return-value)

;;}}}
;;{{{  silence mime parsing in vm 6.0 and above

(defadvice vm-mime-parse-entity (around emacspeak pre act comp)
  (let ((emacspeak-speak-messages nil))
    ad-do-it))

(defadvice vm-decode-mime-message (around emacspeak pre act comp)
  (let ((emacspeak-speak-messages nil))
    ad-do-it))

(defadvice vm-mime-run-display-function-at-point (around emacspeak pre act comp)
  "Provide auditory feedback.
Leave point at front of decoded attachment."
  (cond
   ((interactive-p)
    (let ((orig (point )))
      ad-do-it
      (emacspeak-auditory-icon 'task-done)
      (goto-char orig)
      (message "Decoded attachment")))
   (t ad-do-it))
  ad-return-value)

;;}}}
;;{{{ silence unnecessary chatter

(defadvice vm-emit-eom-blurb (around emacspeak pre act comp)
  "Stop chattering"
  (let ((emacspeak-speak-messages nil))
    ad-do-it))

;;}}}
;;{{{ advice password prompt 

(defadvice vm-read-password(before emacspeak pre act comp)
  "Speak the prompt"
  (let ((prompt (ad-get-arg 0))
        (confirm (ad-get-arg 1)))
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak 
     (format "%s %s"
             prompt
             (if confirm "Confirm by retyping" "")))))
;;}}}
;;{{{ setup presentation buffer correctly
(add-hook 'vm-presentation-mode-hook
          (function
           (lambda nil
             (modify-syntax-entry 10 " "))))

;;}}}
;;{{{ advice button motion 
(defadvice vm-next-button (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-text-range  'w3-hyperlink-info)))


;;}}}
;;{{{  misc 

(defadvice vm-count-messages-in-file (around emacspeak-fix pre act comp)
  (ad-set-arg 1 'quiet)
  ad-do-it)

;;}}}
(provide 'emacspeak-vm)
;;{{{  local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

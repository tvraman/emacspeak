;;; emacspeak-vm.el --- Speech enable VMMail    -*- lexical-binding: t; -*-
;;;(and the one I use)
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak extension to speech enhance vm
;;; Keywords: Emacspeak, VM, Email, Spoken Output, Voice annotations
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2008-07-31 10:49:44 -0700 (Thu, 31 Jul 2008) $ |
;;;  $Revision: 4557 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2021, T. V. Raman
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}

;;{{{  Introduction:

;;; Commentary:
;;; This module extends the mail reader vm.
;;; Uses voice locking for message headers and cited messages
;;; Code:
;;}}}
;;{{{ requires
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require  'vm "vm" 'no-error)

;;}}}
;;{{{ Forward Decls:

(defsubst ems--vm-from-of (message) (aref (aref message 3) 8))
(defsubst ems--vm-subject-of (message) (aref (aref message 3) 11))
(defsubst ems--vm-to-of (message) (aref (aref message 3) 13))
(declare-function  vm-su-full-name "vm-summary" (msg))
(declare-function  vm-su-from "vm-summary" (msg))
(declare-function vm-su-subject "vm-summary" (msg))
(declare-function  vm-su-to-names "vm-summary" (msg))
(declare-function  vm-su-to "vm-summary" (msg))
(declare-function  vm-su-line-count "vm-summary" (msg))
(declare-function vm-decode-mime-encoded-words-in-string "vm-mime" (s))
(defsubst vm-labels-of (message) (aref (aref message 4) 3))
(declare-function  vm-goto-message "vm-message" (msg))
(declare-function vm-delete-message "vm-message" (msg))
(declare-function  u-vm-color-fontify-buffer "u-vm-color" nil)
(declare-function  u-vm-color-summary-mode "u-vm-color" (&optional arg))

;;}}}
;;{{{ voice locking:

(defgroup emacspeak-vm nil
  "VM mail reader on the Emacspeak Desktop."
  :group 'emacspeak
  :group 'vm
  :prefix "emacspeak-vm-")

(defcustom emacspeak-vm-voice-loc nil
  "Set this to T if you want messages automatically voice locked.
Note that some badly formed mime messages  cause trouble."
  :type 'boolean
  :group 'emacspeak-vm)

(add-hook 'vm-mode-hook 'emacspeak-vm-mode-setup)

(defun emacspeak-vm-mode-setup ()
  "Setup function placed on vm-mode-hook by Emacspeak."
  (cl-declare (special  dtk-punctuation-mode dtk-caps))
  (setq dtk-punctuation-mode 'all)
  (when dtk-caps
    (dtk-toggle-caps)))

;;}}}
;;{{{ inline helpers

(defun emacspeak-vm-number-of (message) (aref (aref message 1) 0))

;;}}}
;;{{{ Advice completions

(defadvice vm-minibuffer-complete-word (around emacspeak pre act comp)
  "Say what you completed."
  (let ((prior (save-excursion (skip-syntax-backward "^ >") (point)))
        (dtk-stop-immediately t))
    (emacspeak-kill-buffer-carefully "*Completions*")
    ad-do-it
    (if (> (point) prior)
        (tts-with-punctuations
         'all
         (if (> (length (emacspeak-get-minibuffer-contents)) 0)
             (dtk-speak (emacspeak-get-minibuffer-contents))
           (emacspeak-speak-line)))
      (emacspeak-speak-completions-if-available))
    ad-return-value))

(defadvice vm-minibuffer-complete-word-and-exit (around emacspeak pre act comp)
  "Say what you completed."
  (let ((prior (save-excursion (skip-syntax-backward "^ >") (point)))
        (dtk-stop-immediately t))
    (emacspeak-kill-buffer-carefully "*Completions*")
    ad-do-it
    (if (> (point) prior)
        (tts-with-punctuations
         'all
         (if (> (length (emacspeak-get-minibuffer-contents)) 0)
             (dtk-speak (emacspeak-get-minibuffer-contents))
           (emacspeak-speak-line)))
      (emacspeak-speak-completions-if-available))
    ad-return-value))

;;}}}
;;{{{  Helper functions:

(defvar emacspeak-vm-user-full-name (user-full-name)
  "Full name of user using this session")

(defvar emacspeak-vm-user-login-name  (user-login-name)
  "Login name of this user")
(defun emacspeak-vm-yank-header ()
  "Yank specified header into kill ring."
  (interactive)
  (cl-declare (special vm-message-pointer))
  (cond
   (vm-message-pointer
    (dtk-stop)
    (let*  ((message (car vm-message-pointer))
            (from (ems--vm-from-of message))
            (subject (ems--vm-subject-of  message))
            (to (ems--vm-to-of message))
            (url  (browse-url-url-at-point))
            (header nil))
      (while (not header)
        (setq header
              (cl-case (read-char "f From s Subject t To u URL")
                (?s subject)
                (?f from)
                (?u url)
                (?t to))))
      (when header (kill-new header))
      (dtk-speak-and-echo  (format  "%s" header))))
   (t (error "No current message."))))

(defvar emacspeak-vm-headers-strip-octals t
  "Specify whether non-ascii chars should be stripped when
  speaking email headers.")

(defun emacspeak-vm-speak-message ()
  "Move point to the message body."
  (interactive)
  (goto-char (point-min))
  (search-forward  (format "%c%c" 10 10) nil)
  (condition-case nil
      (emacspeak-hide-all-blocks-in-buffer)
    (error nil))
  (emacspeak-speak-rest-of-buffer))

(defun emacspeak-vm-summarize-message ()
  "Summarize the current vm message. "
  (cl-declare (special vm-message-pointer smtpmail-local-domain
                       vm-presentation-buffer  emacspeak-vm-headers-strip-octals))
  (when vm-message-pointer
    (let*  ((message (car vm-message-pointer))
            (number (emacspeak-vm-number-of  message))
            (from (or (vm-su-full-name message) (vm-su-from message)))
            (subject (vm-su-subject message))
            (to (or (vm-su-to-names message) (vm-su-to message)))
            (self-p
             (or (string-match user-mail-address to)
                 (string-match (user-full-name) to)
                 (string-match  (user-login-name) to)))
            (lines (vm-su-line-count message)))
      (with-current-buffer vm-presentation-buffer
        (dtk-speak
         (vm-decode-mime-encoded-words-in-string
          (concat
           number
           (if from
               (propertize from   'personality voice-brighten)
             "")
           (if subject
               (propertize subject 'personality voice-lighten)
             " ")
           (if (and to (< (length to) 80))
               (concat
                (propertize " to " 'personality voice-smoothen)
                (propertize  to 'personality voice-annotate))
             "")
           (if lines (format "%s lines" lines) "")))))
      (goto-char (point-min))
      (search-forward  (format "%c%c" 10 10) nil)
      (cond
       ((and self-p
             (= 0 self-p)) ;mail to me and others
        (emacspeak-auditory-icon 'select-object))
       (self-p                          ;mail to others including me
        (emacspeak-auditory-icon 'mark-object))
       (t                            ;got it because of a mailing list
        (emacspeak-auditory-icon 'item))))))

(defun emacspeak-vm-speak-labels ()
  "Speak a message's labels"
  (interactive)
  (cl-declare (special vm-message-pointer))
  (when vm-message-pointer
    (message "Labels: %s"
             (vm-labels-of (car vm-message-pointer)))))

(defun emacspeak-vm-mode-line ()
  "VM mode line information. "
  (interactive)
  (cl-declare (special vm-ml-message-attributes-alist
                       vm-ml-message-read vm-ml-message-unread
                       vm-virtual-folder-definition vm-ml-message-new
                       vm-ml-message-number vm-ml-highest-message-number))
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
                #'(lambda(item)
                    (let ((var (car item))
                          (value (cadr item)))
                      (cond
                       ((and (boundp var) (eval var))
                        (if (symbolp value)
                            (eval value)
                          value))
                       (t ""))))
                (cdr vm-ml-message-attributes-alist)   " "))))))

;;}}}
;;{{{  Moving between messages

(add-hook 'vm-select-message-hook 'emacspeak-vm-summarize-message)

;;}}}
;;{{{  Scrolling messages:

(defun emacspeak-vm-locate-subject-line()
  "Locates the subject line in a message being read.
Useful when you're reading a message
that has been forwarded multiple times."
  (interactive)
  (re-search-forward "^ *Subject:" nil t)
  (emacspeak-speak-line))

(defadvice vm-scroll-forward (after emacspeak pre act comp)
  "Produce auditory feedback.
Then speak the screenful. "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (save-excursion
      (let ((start  (point))
            (window (get-buffer-window (current-buffer))))
        (forward-line (window-height window))
        (emacspeak-speak-region start (point))))))

(defadvice vm-scroll-backward (after emacspeak pre act comp)
  "Produce auditory feedback.
Then speak the screenful. "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (save-excursion
      (let ((start  (point))
            (window (get-buffer-window (current-buffer))))
        (forward-line(-  (window-height window)))
        (emacspeak-speak-region start (point))))))
(defun emacspeak-vm-browse-message ()
  "Browse an email message --read it paragraph at a time. "
  (interactive)
  (emacspeak-execute-repeatedly 'forward-paragraph))

;;}}}
;;{{{  deleting and killing

(defadvice vm-delete-message (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (message "Message discarded.")))

(defadvice vm-undelete-message (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (message "Message recovered.")))

(defadvice vm-kill-subject (after emacspeak pre act comp)
  "speak. "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (call-interactively 'vm-next-message)))

;;}}}
;;{{{  Sending mail:

(defadvice vm-forward-message (around emacspeak pre act comp)
  "Provide aural feedback."
  (cond
   ((ems-interactive-p)
      (emacspeak-auditory-icon 'open-object)
      (message "Forwarding message")
      ad-do-it
      (emacspeak-speak-line))
        (t
         ad-do-it))
  ad-return-value)

(defadvice vm-reply (after emacspeak pre act comp)
  "Provide aural feedback."
  (when (ems-interactive-p)
    (emacspeak-speak-mode-line)))

(defadvice vm-followup (after emacspeak pre act comp)
  "Provide aural feedback."
  (when (ems-interactive-p)
    (message "Folluwing up")
    (emacspeak-speak-mode-line)))

(defadvice vm-reply-include-text (after emacspeak pre act comp)
  "Provide aural feedback."
  (when (ems-interactive-p)
    (emacspeak-speak-mode-line)))

(defadvice vm-followup-include-text (after emacspeak pre act comp)
  "Provide aural feedback."
  (when (ems-interactive-p)
    (message "Following up")
    (emacspeak-speak-mode-line)))
(defadvice vm-mail-send (after emacspeak pre act comp)
  "Provide auditory context"
  (when  (ems-interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'close-object)))

(defadvice vm-mail-send-and-exit (after emacspeak pre act comp)
  "Provide auditory context"
  (when  (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)))

(cl-loop
 for f in
 '(vm-mail vm-mail-from-folder)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak."
     (when (ems-interactive-p)
       (let ((dtk-stop-immediately nil))
         (message "Composing a message")
         (emacspeak-speak-line))))))

;;}}}
;;{{{ quitting

(defadvice vm-quit (after emacspeak pre act comp)
  "Provide an auditory icon if requested"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (with-current-buffer (window-buffer (selected-window))
      (emacspeak-speak-mode-line))))

;;}}}
;;{{{ catching up on folders

(defun emacspeak-vm-catch-up-all-messages ()
  "Mark all messages in folder to be deleted. Use with caution."
  (interactive)
  (cl-declare (special vm-ml-highest-message-number))
  (vm-goto-message 1)
  (vm-delete-message
   (read vm-ml-highest-message-number))
  (message "All messages have been marked as deleted.")
  (emacspeak-auditory-icon 'delete-object))

;;}}}
;;{{{  Keybindings:
(when (boundp 'vm-mode-map)
  (cl-declaim  (special
                vm-mode-map
                global-map emacspeak-prefix emacspeak-keymap))
  (define-key vm-mode-map "C" 'vm-chromium)
  (define-key vm-mode-map "\M-\C-m" 'widget-button-press)
  (define-key vm-mode-map "y" 'emacspeak-vm-yank-header)
  (define-key vm-mode-map  "j" 'emacspeak-hide-or-expose-all-blocks)
  (define-key vm-mode-map  "\M-g" 'vm-goto-message)
  (define-key vm-mode-map "J" 'vm-discard-cached-data)
  (define-key vm-mode-map "." 'emacspeak-vm-browse-message)
  (define-key vm-mode-map "'" 'emacspeak-speak-rest-of-buffer)
  (define-key vm-mode-map "\M-c" 'emacspeak-vm-catch-up-all-messages)
  (define-key vm-mode-map "\M-j" 'emacspeak-vm-locate-subject-line)
  (define-key vm-mode-map "," 'emacspeak-vm-speak-message)
  (define-key vm-mode-map "\M-l" 'emacspeak-vm-speak-labels)
  (define-key vm-mode-map
    (concat emacspeak-prefix "m")
    'emacspeak-vm-mode-line)
  )
;;}}}
;;{{{ advise searching:
(defadvice vm-isearch-forward (around emacspeak pre act comp)
  "speak"
  (cl-declare (special vm-message-pointer))
  (cond
   ((ems-interactive-p)
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
  "speak"
  (cl-declare (special vm-message-pointer))
  (cond
   ((ems-interactive-p)
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
  (ems-with-messages-silenced
   ad-do-it))

(defadvice vm-decode-mime-message (around emacspeak pre act comp)
  (ems-with-messages-silenced
   ad-do-it))

(defadvice vm-mime-run-display-function-at-point (around emacspeak pre act comp)
  "speak.
Leave point at front of decoded attachment."
  (cond
   ((ems-interactive-p)
    (let ((orig (point)))
      ad-do-it
      (goto-char orig)
      (message "Decoded attachment")))
   (t ad-do-it))
  ad-return-value)

;;}}}
;;{{{ silence unnecessary chatter

(defadvice vm-emit-eom-blurb (around emacspeak pre act comp)
  "Stop chattering"
  (ems-with-messages-silenced
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
          #'(lambda nil
              (emacspeak-pronounce-refresh-pronunciations)))

(cl-declaim (special emacspeak-pronounce-internet-smileys-pronunciations))
(cl-loop
 for hook in
 '(mail-mode-hook vm-presentation-mode-hook)
 do
 (add-hook hook 'emacspeak-pronounce-refresh-pronunciations 'append))

(defvar emacspeak-speak-embedded-url-pattern
  "<https?:[^ \t]*>"
  "Pattern to recognize embedded URLs.")

(cl-loop
 for mode in
 '(vm-presentation-mode mail-mode)
 do
 (emacspeak-pronounce-augment-pronunciations
  mode
  emacspeak-pronounce-internet-smileys-pronunciations)
 (emacspeak-pronounce-add-dictionary-entry
  mode
  emacspeak-speak-embedded-url-pattern
  (cons
   #'re-search-forward
   #'(lambda (_url) " Link ")))
 (emacspeak-pronounce-add-dictionary-entry
  mode
  emacspeak-speak-rfc-3339-datetime-pattern
  (cons #'re-search-forward #'emacspeak-speak-decode-rfc-3339-datetime))
 (emacspeak-pronounce-add-dictionary-entry
  mode
  emacspeak-speak-iso-datetime-pattern
  (cons #'re-search-forward #'emacspeak-speak-decode-iso-datetime)))

;;}}}
;;{{{ advice button motion

;;}}}
;;{{{  misc

(defadvice vm (around emacspeak pre act comp)
  "Silence chatter."
  (let ((emacspeak-speak-messages nil))
    ad-do-it
    (emacspeak-vm-mode-line)))

(defadvice vm-count-messages-in-file (around emacspeak-fix pre act comp)
  (ad-set-arg 1 'quiet)
  ad-do-it)

;;}}}
;;{{{  button motion in vm

;;}}}
;;{{{ saving mime attachment under point

;;}}}
;;{{{ Voice Lock:

(when (locate-library "u-vm-color")
  (require 'u-vm-color)
  (voice-setup-add-map
   '(
     (u-vm-color-author-face voice-brighten)
     (u-vm-color-citation-1-face voice-smoothen)
     (u-vm-color-citation-2-face voice-smoothen-extra)
     (u-vm-color-date-face voice-monotone-medium)
     (u-vm-color-header-face voice-bolden-medium)
     (u-vm-color-recipient-face voice-annotate)
     (u-vm-color-signature-face voice-smoothen)
     (u-vm-color-spamassassin-face voice-monotone-extra)
     (u-vm-color-subject-face voice-lighten)
     (u-vm-color-time-face voice-monotone-extra)
     (u-vm-color-user-face voice-animate)

     )
   )
  (add-hook 'vm-showing-message-hook #'u-vm-color-fontify-buffer)
  (add-hook 'vm-presentation-mode-hook #'u-vm-color-fontify-buffer)
  (add-hook 'vm-summary-mode-hook #'u-vm-color-summary-mode)
  (add-hook 'vm-select-message-hook #'u-vm-color-fontify-buffer)
  )

;;}}}
;;{{{ configure and customize vm

;;; This is how I customize VM
;;; First, Configure VM into using shr instead of w3m:

(defun vm-mime-display-internal-shr-text/html (start end _layout)
  "Use shr to inline HTML mails in the VM presentation buffer."
    (shr-render-region start (1- end))
    (put-text-property start end 'text-rendered-by-shr t))
     
;;; has to be done indirectly
;;; Fake emacs-w3m, though we actually use shr
     (defalias 'vm-mime-display-internal-emacs-w3m-text/html  'vm-mime-display-internal-shr-text/html)

(defun vm-chromium ()
       "Run Chromium on current link."
       (interactive)
       (let ((url (browse-url-url-at-point)))
         (unless url (error "No link here."))
         (dtk-stop)
         (browse-url-chrome url)
         (message "Opening url with Chrome")))

     

(defcustom emacspeak-vm-use-raman-settings t
  "Should VM  use the customizations used by the author of Emacspeak."
  :type 'boolean
  :group 'emacspeak-vm)

(defvar emacspeak-vm-demote-html-attachments
  '(
    favorite-internal  "text/plain" "text/enriched"
    "text/html" "application/xml+xhtml")
  "Setting that prefers text/plain alternatives over html/xhtml.")

(defvar emacspeak-vm-promote-html-attachments
  '(
    favorite-internal   "text/html" "application/xml+xhtml"
    "text/plain" "text/enriched")
  "Setting that prefers  alternatives  html/xhtml over text/plain.")

(defun emacspeak-vm-use-raman-settings ()
  "Customization settings for VM used by the author of Emacspeak."
  (cl-declare (special emacspeak-vm-demote-html-attachments
                       emacspeak-vm-promote-html-attachments
                       vm-mime-charset-converter-alist vm-mime-default-face-charsets
                       vm-frame-per-folder vm-frame-per-composition
                       vm-frame-per-edit vm-frame-per-help
                       vm-frame-per-summary vm-index-file-suffix
                       vm-crash-box vm-primary-inbox vm-folder-directory
                       vm-forwarding-subject-format vm-startup-with-summary
                       vm-inhibit-startup-message vm-visible-headers
                       vm-delete-after-saving vm-url-browser
                       vm-confirm-new-folders vm-mime-alternative-select-method
                       vm-mime-text/html-handler vm-move-after-deleting))
  (setq vm-mime-text/html-handler'emacs-w3m  )
  (setq vm-mime-alternative-select-method emacspeak-vm-demote-html-attachments)
  (setq vm-mime-charset-converter-alist
        '(
          ("utf-8" "iso-8859-1" "iconv -f utf-8 -t iso-8859-1")))
  (setq vm-mime-default-face-charsets t)
  (setq vm-frame-per-folder nil
        vm-frame-per-composition nil
        vm-frame-per-edit nil
        vm-frame-per-help nil
        vm-frame-per-summary nil)
  (setq vm-index-file-suffix ".idx"
        vm-primary-inbox "~/mbox"
        vm-folder-directory "~/Mail/"
        vm-crash-box "mbox.crash"
        vm-forwarding-subject-format "[%s]"
        vm-startup-with-summary nil
        vm-inhibit-startup-message t
        vm-visible-headers '("From:" "To:" "Subject:" "Date:" "Cc:")
        vm-delete-after-saving t
        vm-url-browser 'eww-browse-url
        vm-confirm-new-folders t
        vm-move-after-deleting nil)
  t)

(defun emacspeak-vm-toggle-html-mime-demotion ()
  "Toggle state of HTML Mime promotion/Demotion."
  (interactive)
  (cl-declare (special emacspeak-vm-demote-html-attachments
                       emacspeak-vm-promote-html-attachments
                       vm-mime-alternative-select-method))
  (cond
   ((eq vm-mime-alternative-select-method emacspeak-vm-demote-html-attachments)
    (setq vm-mime-alternative-select-method
          emacspeak-vm-promote-html-attachments)
    (message "Preferring HTML Mime alternative."))
   ((eq vm-mime-alternative-select-method emacspeak-vm-promote-html-attachments)
    (setq vm-mime-alternative-select-method
          emacspeak-vm-demote-html-attachments)
    (message "Preferring Text/Plain Mime alternative."))
   (t (message "Resetting state to HTML Mime demotion.")
      (setq vm-mime-alternative-select-method
            emacspeak-vm-demote-html-attachments))))

(when emacspeak-vm-use-raman-settings
  (emacspeak-vm-use-raman-settings))

(defcustom emacspeak-vm-customize-mime-settings t
  "Non-nil will cause Emacspeak to configure VM mime
settings to match what the author of Emacspeak uses."
  :type 'boolean
  :group 'emacspeak-vm)

(defcustom emacspeak-vm-pdf2text
  (expand-file-name "pdf2text" emacspeak-etc-directory)
  "Executable that converts PDF on standard input to plain
text using pdftotext."
  :type 'string
  :group 'emacspeak-vm)


(defcustom emacspeak-vm-cal2text
  (expand-file-name "cal2text" emacspeak-etc-directory)
  "Executable that converts calendar invitations    on
  standard input to plain text."
  :type 'string
  :group 'emacspeak-vm)

(defun emacspeak-vm-add-mime-converter (converter)
  "Helper to add a converter specification."
  (cl-declare (special vm-mime-type-converter-alist))
  (unless
      (cl-find-if
       #'(lambda  (i)
           (string-equal (car i) (car converter)))
       vm-mime-type-converter-alist)
    (push   converter
            vm-mime-type-converter-alist)))

(defun emacspeak-vm-customize-mime-settings ()
  "Customize VM mime settings."
  (cl-declare (special vm-preview-lines
                       vm-infer-mime-types
                       vm-mime-decode-for-preview
                       vm-auto-decode-mime-messages
                       vm-auto-displayed-mime-content-type-exceptions
                       vm-mime-attachment-save-directory
                       vm-mime-base64-encoder-program
                       vm-mime-base64-decoder-program
                       vm-mime-attachment-auto-type-alist
                       vm-mime-type-converter-alist
                       emacspeak-vm-pdf2text
                       emacspeak-vm-cal2text))
  (emacspeak-vm-add-mime-converter
   (list "text/calendar" "text/plain" emacspeak-vm-cal2text))
  (emacspeak-vm-add-mime-converter
   (list "application/pdf" "text/plain"
         emacspeak-vm-pdf2text))
  (setq vm-preview-lines nil
        vm-infer-mime-types t
        vm-mime-decode-for-preview nil
        vm-auto-decode-mime-messages t
        vm-auto-displayed-mime-content-type-exceptions '("text/html")
        vm-mime-attachment-save-directory
        (expand-file-name "~/Mail/attachments/")
        vm-mime-base64-encoder-program "base64-encode"
        vm-mime-base64-decoder-program "base64-decode")
  t)

(when (and (featurep 'vm)emacspeak-vm-customize-mime-settings)
  (emacspeak-vm-customize-mime-settings))

;;}}}
(provide 'emacspeak-vm)
;;{{{  local variables

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

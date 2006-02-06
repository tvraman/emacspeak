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

;;;Copyright (C) 1995 -- 2004, T. V. Raman 
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

;;{{{  Introduction:
;;; This module extends the mail reader vm.
;;; Uses voice locking for message headers and cited messages

;;}}}
;;{{{ requires
(require 'emacspeak-preamble)

;;}}}
;;{{{ voice locking:

(defgroup emacspeak-vm nil
  "VM mail reader on the Emacspeak Desktop."
  :group 'emacspeak
  :group 'vm
  :prefix "emacspeak-vm-")

(defcustom emacspeak-vm-voice-lock-messages nil
  "Set this to T if you want messages automatically voice locked.
Note that some badly formed mime messages  cause trouble."
  :type 'boolean
  :group 'emacspeak-vm)

(add-hook 'vm-mode-hook
          'emacspeak-vm-mode-setup)

(defun emacspeak-vm-mode-setup ()
  "Setup function placed on vm-mode-hook by Emacspeak."
  (declare (special  dtk-punctuation-mode emacspeak-vm-voice-lock-messages
                     dtk-allcaps-beep))
  (setq dtk-punctuation-mode 'some)
  (when dtk-allcaps-beep
    (dtk-toggle-allcaps-beep))
  (emacspeak-dtk-sync)
  (when emacspeak-vm-voice-lock-messages
    (voice-lock-mode 1)))

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
(defun emacspeak-vm-yank-header ()
  "Yank specified header into kill ring."
  (interactive)
  (declare (special vm-message-pointer))
  (cond
   (vm-message-pointer
    (dtk-stop)
    (let*  ((message (car vm-message-pointer ))
            (from (vm-from-of message ))
            (subject (vm-subject-of  message ))
            (to (vm-to-of message ))
            (header nil))
      (while (not header)
	(setq header 
	      (case
		  (read-char "f From s Subject t To")
		(?s subject)
		(?f from)
		(?t to))))
      (kill-new header)
      (message header)))
   (t (error "No current message." ))))
(defcustom emacspeak-vm-headers-strip-octals t
  "Specify whether non-ascii chars should be stripped when
  speaking email headers."
  :type 'boolean
  :group 'emacspeak-vm)

(defun emacspeak-vm-summarize-message ()
  "Summarize the current vm message. "
  (declare (special vm-message-pointer
                    emacspeak-vm-headers-strip-octals
                    emacspeak-vm-user-full-name emacspeak-vm-user-login-name))
  (when vm-message-pointer
    (dtk-stop)
    (let*  ((dtk-stop-immediately t )
            (message (car vm-message-pointer ))
            (number (emacspeak-vm-number-of  message))
            (from(or (vm-su-full-name message)
                     (vm-su-from message )))
            (subject (vm-su-subject message ))
            (to(or (vm-su-to-names message)
                   (vm-su-to message )))
            (self-p (or
                     (string-match emacspeak-vm-user-full-name to)
                     (string-match  (user-login-name) to)))
            (lines (vm-su-line-count message))
            (summary nil))
      (dtk-speak
       (vm-decode-mime-encoded-words-in-string
        (format "%s %s %s   %s %s "
                number
                (or from "")
                (if subject (format "on %s" subject) "")
                (if (and to (< (length to) 80))
                    (format "to %s" to) "")
                (if lines (format "%s lines" lines) ""))))
      (cond 
       ((and self-p
             (= 0 self-p)                    ) ;mail to me and others 
        (emacspeak-auditory-icon 'item))
       (self-p				;mail to others including me
        (emacspeak-auditory-icon 'mark-object))
       (t			     ;got it because of a mailing list
        (emacspeak-auditory-icon 'select-object ))))))

(defun emacspeak-vm-speak-labels ()
  "Speak a message's labels"
  (interactive)
  (declare (special vm-message-pointer))
  (when vm-message-pointer
    (message "Labels: %s"
             (vm-labels-of (car vm-message-pointer )))))

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
          'emacspeak-vm-summarize-message)

;;}}}
;;{{{  Scrolling messages:

(defun emacspeak-vm-locate-subject-line()
  "Locates the subject line in a message being read.
Useful when you're reading a message
that has been forwarded multiple times."
  (interactive)
  (re-search-forward "^ *Subject:" nil t )
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
(define-key vm-mode-map "\M-\C-m" 'widget-button-press)
(define-key vm-mode-map "y" 'emacspeak-vm-yank-header)
(define-key vm-mode-map "\M-\t"
  'emacspeak-vm-next-button)
(define-key vm-mode-map  "j"
  'emacspeak-hide-or-expose-all-blocks)
(define-key vm-mode-map  "\M-g" 'vm-goto-message)
(define-key vm-mode-map "J" 'vm-discard-cached-data)
(define-key vm-mode-map "." 'emacspeak-vm-browse-message)
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
;;{{{ catching up on folders

(defun emacspeak-vm-catch-up-all-messages ()
  "Mark all messages in folder to be deleted. Use with caution."
  (interactive)
  (declare (special vm-ml-highest-message-number))
  (vm-goto-message 1)
  (vm-delete-message
   (read vm-ml-highest-message-number))
  (message "All messages have been marked as deleted.")
  (emacspeak-auditory-icon 'delete-object))

;;}}}
;;{{{  Keybindings:
(declaim  (special vm-mode-map
                   global-map
                   emacspeak-prefix
                   emacspeak-keymap))
(define-key vm-mode-map "C" 'emacspeak-vm-catch-up-all-messages)
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
             (emacspeak-pronounce-refresh-pronunciations))))
(declaim (special emacspeak-pronounce-internet-smileys-pronunciations))
(loop for hook in 
      (list 'mail-mode-hook
            'vm-presentation-mode-hook)
      do 
      (add-hook hook 'emacspeak-pronounce-refresh-pronunciations
                'append ))

(loop for mode in 
      '(vm-presentation-mode 
        mail-mode)
      do 
      (emacspeak-pronounce-augment-pronunciations mode
                                                  emacspeak-pronounce-internet-smileys-pronunciations)
      (emacspeak-pronounce-add-dictionary-entry mode
                                                emacspeak-speak-embedded-url-pattern
                                                (cons 're-search-forward
                                                      #'(lambda
                                                          (url) " Link ")))
      (emacspeak-pronounce-add-dictionary-entry mode
                                                emacspeak-speak-iso-datetime-pattern
                                                (cons 're-search-forward
                                                      'emacspeak-speak-decode-iso-datetime)))          

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
;;{{{  button motion in vm 

(defun emacspeak-vm-next-button (n)
  "Move point to N buttons forward.
If N is negative, move backward instead."
  (interactive "p")
  (let ((function (if (< n 0) 'previous-single-property-change
		    'next-single-property-change))
	(inhibit-point-motion-hooks t)
	(backward (< n 0))
	(limit (if (< n 0) (point-min) (point-max))))
    (setq n (abs n))
    (while (and (not (= limit (point)))
		(> n 0))
      ;; Skip past the current button.
      (when (get-text-property (point) 'w3-hyperlink-info)
	(goto-char (funcall function (point) 'w3-hyperlink-info nil limit)))
      ;; Go to the next (or previous) button.
      (goto-char (funcall function (point) 'w3-hyperlink-info nil limit))
      ;; Put point at the start of the button.
      (when (and backward (not (get-text-property (point) 'w3-hyperlink-info)))
	(goto-char (funcall function (point) 'w3-hyperlink-info nil limit)))
      ;; Skip past intangible buttons.
      (when (get-text-property (point) 'intangible)
	(incf n))
      (decf n))
    (unless (zerop n)
      (message  "No more buttons"))
    n))

;;}}}
;;{{{ saving mime attachment under point 

;;}}}
;;{{{ configure and customize vm 

;;; This is how I customize VM

(defcustom emacspeak-vm-use-raman-settings t
  "Should VM  use the customizations used by the author of Emacspeak."
  :type 'boolean
  :group 'emacspeak-vm)

(defun emacspeak-vm-use-raman-settings ()
  "Customization settings for VM used by the author of
Emacspeak."
  (declare (special
            vm-mime-charset-converter-alist
            vm-mime-default-face-charsets 
            vm-frame-per-folder 
            vm-frame-per-composition 
            vm-frame-per-edit 
            vm-frame-per-help 
            vm-frame-per-summary 
            vm-index-file-suffix
            vm-primary-inbox
            vm-folder-directory
            vm-forwarding-subject-format
            vm-startup-with-summary
            vm-inhibit-startup-message
            vm-visible-headers
            vm-delete-after-saving
            vm-url-browser
            vm-confirm-new-folders
            vm-move-after-deleting
            emacspeak-vm-voice-lock-messages))
  (setq vm-mime-charset-converter-alist
	'(
	  ("utf-8" "iso-8859-1" "iconv -f utf-8 -t iso-8859-1")
	  )
	)
  (setq vm-mime-default-face-charsets t)
  (setq vm-frame-per-folder nil
	vm-frame-per-composition nil
	vm-frame-per-edit nil
	vm-frame-per-help nil
	vm-frame-per-summary nil)

  (setq vm-index-file-suffix ".idx"
        vm-primary-inbox "~/mbox"
        vm-folder-directory "~/Mail/"
        vm-forwarding-subject-format "[%s]"
        vm-startup-with-summary nil
        vm-inhibit-startup-message t
        vm-visible-headers '("From:" "To:" "Subject:" "Date:" "Cc:")
        vm-delete-after-saving t
        vm-url-browser 'browse-url
        vm-confirm-new-folders t
        vm-move-after-deleting nil
        emacspeak-vm-voice-lock-messages t)
  t)
  
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

(defcustom emacspeak-vm-doc2text
  (expand-file-name "doc2text" emacspeak-etc-directory)
  "Executable that converts MSWord documents on standard input to plain
text using wvText."
  :type 'string
  :group 'emacspeak-vm)
(defcustom emacspeak-vm-cal2text
  (expand-file-name "cal2text" emacspeak-etc-directory)
  "Executable that converts calendar invitations    on
  standard input to plain text."
  :type 'string
  :group 'emacspeak-vm)

(defcustom emacspeak-vm-xls2html
  (expand-file-name "xls2html" emacspeak-etc-directory)
  "Executable that converts MSXL documents on standard input to HTML
 using xlhtml."
  :type 'string
  :group 'emacspeak-vm)

(defcustom emacspeak-vm-ppt2html
  (expand-file-name "ppt2html" emacspeak-etc-directory)
  "Executable that converts MSPPT documents on standard input to HTML
 using xlhtml."
  :type 'string
  :group 'emacspeak-vm)

(defsubst emacspeak-vm-add-mime-convertor (convertor)
  "Helper to add a convertor specification."
  (declare (special vm-mime-type-converter-alist))
  (unless
      (find-if
       #'(lambda  (i)
           (string-equal (car i) (car convertor)))
       vm-mime-type-converter-alist)
    (push   convertor 
            vm-mime-type-converter-alist)))

(defun emacspeak-vm-customize-mime-settings ()
  "Customize VM mime settings."
  (declare (special vm-preview-lines
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
		    emacspeak-vm-ppt2html
		    emacspeak-vm-xls2html
		    emacspeak-vm-doc2text
		    emacspeak-vm-cal2text))
  (emacspeak-vm-add-mime-convertor
   (list "text/calendar" "text/plain" emacspeak-vm-cal2text))
  (emacspeak-vm-add-mime-convertor
   (list "application/pdf" "text/plain"
         emacspeak-vm-pdf2text))
  (emacspeak-vm-add-mime-convertor
   (list "application/vnd.ms-excel" "text/html"
         emacspeak-vm-xls2html))
  (emacspeak-vm-add-mime-convertor
   (list "application/vnd.ms-powerpoint" "text/html" emacspeak-vm-ppt2html))
  (emacspeak-vm-add-mime-convertor
   (list "application/msword" "text/plain" emacspeak-vm-doc2text))
  (setq vm-preview-lines nil
        vm-infer-mime-types t
        vm-mime-decode-for-preview nil
        vm-auto-decode-mime-messages t
        vm-auto-displayed-mime-content-type-exceptions '("text/html")
        vm-mime-attachment-save-directory (expand-file-name "~/Mail/attachments/")
        vm-mime-base64-encoder-program "base64-encode"
        vm-mime-base64-decoder-program "base64-decode")
  t)
   

(when emacspeak-vm-customize-mime-settings
  (emacspeak-vm-customize-mime-settings))

;;}}}
(provide 'emacspeak-vm)
;;{{{  local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

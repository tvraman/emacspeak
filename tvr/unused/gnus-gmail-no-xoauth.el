;;;  Gnus Setup For GMail imap:  -*- lexical-binding: nil; -*-
;; GMail without 2-factor
;; This module sets things up for GMail.

;;{{{ Requires:

(eval-after-load "gnus"
  `(progn

     (require 'cl-lib)
     (require 'smtpmail)
     (require 'auth-source)
     (cl-pushnew (expand-file-name "~/.authinfo.gpg") auth-sources)
     ;;}}}
     ;;{{{ Sending Mail:

;;  Set send-mail-function via custom.
     (setq
                                        ;smtpmail-debug-info t
                                        ;smtpmail-debug-verb t
      smtpmail-stream-type 'ssl
      smtpmail-smtp-user "raman@google.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465)

     ;;}}}
     ;;{{{GMail Using app-specific passwords via authinfo 

     (cl-declaim (special gnus-select-method gnus-secondary-select-methods))
     (setq
      gnus-select-method
      `(nnimap
        "gmail"
        (nnimap-address "imap.gmail.com")
        (nnimap-server-port 993)
        (nnimap-user "raman@google.com")
                                        ;(nnimap-fetch-partial-articles "text/")
        (nnimap-expunge always)
        (nnmail-expiry-wait immediate)
        (nnimap-streaming t)
        (nnimap-stream ssl)))

     (defun gm-user-to-nnimap (user)
       "Return nnimap select method for sspecified user."
       `(nnimap
         ,user
         (nnimap-user ,(format "%s@gmail.com" user))
         (nnimap-address "imap.gmail.com")
         (nnimap-server-port 993)
                                        ;(nnimap-fetch-partial-articles "text/")
         (nnmail-expiry-wait immediate)
         (nnimap-streaming t)
         (nnimap-stream ssl)))

     (setq gnus-secondary-select-methods
           (mapcar #'gm-user-to-nnimap
                   '( "tv.raman.tv" "emacspeak")))

     ;;}}}
     ;;{{{Additional gnus settings:

     (setq gnus-auto-subscribed-groups nil)
     (defun gmail-report-spam ()
       "Report the current or marked mails as spam.
This moves them into the Spam folder."
       (interactive)
       (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/Spam")
       (emacspeak-auditory-icon 'task-done))
     (define-key gnus-summary-mode-map "$" 'gmail-report-spam)

     ;;}}}
     ))

;;{{{ Utils:
;; local variables:
;; folded-file: t
;; end:

;;}}}

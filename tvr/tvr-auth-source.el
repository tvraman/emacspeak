;;; Uses auth-source-xoauth2:
;;; https://github.com/ccrusius/auth-source-xoauth2
;;; That module extends Emacs' auth-source with xoauth2 support.
;;; This module sets things up for GMail.
;;;
;;{{{ Requires:

(require 'cl-lib)
(require 'auth-source-xoauth2)
(require 'smtpmail)

;;;  Personal Creds:
(defvar tvr-xoauth2-creds-location
  (expand-file-name "~/.elisp/xoauth2-creds.el.gpg")
  "Where we store our tokens.
This file should be GPG encrypted --- Emacs is smart enough to decrypt
on load.")

(unless
    (and (bound-and-true-p auth-xoauth2-tvr-creds)
         (hash-table-p auth-xoauth2-tvr-creds))
  (load-file tvr-xoauth2-creds-location))

;;}}}
;;{{{  Configure XOauth2:

(defun tvr-auth-source-oauth (_host user _port)
  "Read and return OAuth2 tokens from secure store"
  (cl-declare (special auth-xoauth2-tvr-creds
                       tvr-xoauth2-creds-location))
  (unless
      (and (bound-and-true-p auth-xoauth2-tvr-creds)
           (hash-table-p auth-xoauth2-tvr-creds))
    (condition-case nil
        (progn
          (message "Loading xoauth secrets.")
          (load-file tvr-xoauth2-creds-location))
      (error "Could not find OAuth2 secrets store")))
  (gethash user auth-xoauth2-tvr-creds))

(setq auth-source-xoauth2-creds  #'tvr-auth-source-oauth)
;(setq auth-source-xoauth2-creds (expand-file-name "~/.xoauth2-creds.gpg"))
(auth-source-xoauth2-enable)
(add-to-list 'smtpmail-auth-supported 'xoauth2)

;;}}}
;;{{{ Tests:

;;; (auth-source-xoauth2--search nil nil nil "raman@google.com" nil)
;;; (auth-source-search :host "smtp.gmail.com" :user "raman@google.com" :type 'xoauth2 :max 1)

;;}}}
;;{{{ Sending Mail:

;;  Set send-mail-function via custom.
(setq
;smtpmail-debug-info t
                                        ;smtpmail-debug-verb t
                                        ;  smtpmail-stream-type 'ssl
 smtpmail-smtp-user user-mail-address
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 465)

;;}}}
;;{{{GMail Using xoauth2  and Gnus:
 (cl-declaim (special gnus-select-method gnus-secondary-select-methods))
 (setq
  gnus-select-method
  `(nnimap
    "gmail"
    (nnimap-address "imap.gmail.com")
    (nnimap-server-port 993)
    (nnimap-user ,user-mail-address)
    (nnimap-authenticator xoauth2)
    (nnimap-fetch-partial-articles "text/")
    (nnimap-expunge always)
    (nnmail-expiry-wait immediate)
    (nnimap-streaming t)
    (nnimap-stream ssl)))

(defun gm-user-to-nnimap (user)
  "Return nnimap select method for sspecified user."
  `(nnimap
    ,user
    (nnimap-user ,(format "%s@gmail.com" user))
    (nnimap-authenticator xoauth2)
    (nnimap-address "imap.gmail.com")
    (nnimap-server-port 993)
    (nnimap-fetch-partial-articles "text/")
    (nnmail-expiry-wait immediate)
    (nnimap-streaming t)
    (nnimap-stream ssl)))

(setq gnus-secondary-select-methods
      (mapcar #'gm-user-to-nnimap
              '( "tv.raman.tv" "emacspeak")))

;;}}}
;;{{{ Utils:

(defun google-py-oauth2-cli (user app-secret)
  "generate command-line for pasting into a shell."
  (format
   "python oauth2.py --user %s --client_id %s --client_secret %s   --generate_oauth2_token"
   user
   (plist-get app-secret :client-id)
   (plist-get app-secret :client-secret)))

;;; Usage:
;;;(google-py-oauth2-cli "tv.raman.tv@gmail.com" tvr-app-secrets)
;;;(google-py-oauth2-cli "emacspeak@gmail.com" tvr-app-secrets)

;;}}}
;; (provide 'tvr-auth-source)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

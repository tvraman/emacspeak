;;; Configuring Emacs to send GMail:

(require 'smtpmail)(setq send-mail-function 'smtpmail-send-it)
(setq
 send-mail-function 'smtpmail-send-it
 user-mail-address "username@gmail.com"
 smtpmail-default-smtp-server "smtp.gmail.com"
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587
 smtpmail-auth-credentials ; passwd set to nil  will be prompted
 '(("smtp.gmail.com" 587 "username" nil))
 smtpmail-starttls-credentials
 '(("smtp.gmail.com" 587
    "/etc/ssl/certs/cacert.pem" "/etc/ssl/private/cakey.pem")))

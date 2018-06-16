;;; Configuring Emacs to send GMail:
;;; In addition:
;;; authinfo.gpg should contain:
;;; 1 line for outgoing smtp 
;;; 1 line for incoming imap (for reading mail)
(require 'smtpmail)

(setq
 send-mail-function 'smtpmail-send-it
 user-mail-address "tv.raman.tv@gmail.com"
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587
 )
(defun tvr ()
  "Send email as TVR."
  (interactive)
  (let ((user-mail-address "tv.raman.tv@gmail.com"))
    (compose-mail)))

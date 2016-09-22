;;; Configuring Emacs to send GMail:  -*- lexical-binding: t; -*-
;;; Auth credentials are taken from .authinfo.gpg

(require 'smtpmail)

(setq
 send-mail-function 'smtpmail-send-it
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587)
;;; Have had mail go awol with async (commenting out)
;; (when (locate-library "smtpmail-async")
;;   (require 'smtpmail-async)
;;   (setq send-mail-function 'async-smtpmail-send-it
;;         message-send-mail-function 'async-smtpmail-send-it))

(provide 'gm-smtp)

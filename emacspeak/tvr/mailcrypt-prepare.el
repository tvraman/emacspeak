;;; setting up mailcrypt to use gpg:
;;;
(augment-load-path "mailcrypt" "mailcrypt")
(setq mc-gpg-comment nil)
(autoload 'mc-encrypt-message "mailcrypt" nil t)
(autoload 'mc-sign-message "mailcrypt" nil t)
(autoload 'mc-insert-public-key "mailcrypt" nil t)
(autoload 'mc-install-write-mode "mailcrypt" "mailcrypt" nil t)
(autoload 'mc-install-read-mode "mailcrypt" "mailcrypt" nil t)
(add-hook 'mail-mode-hook 'mc-install-write-mode)
(add-hook 'news-reply-mode-hook 'mc-install-write-mode)

;;; reading mode setup:
(autoload 'mc-snarf-keys "mailcrypt" nil t)

(add-hook 'vm-mode-hook  'mc-install-read-mode)
(add-hook 'vm-presentation-mode-hook  'mc-install-read-mode)
(setq mc-gpg-path "gpg")
(setq mc-gpg-keydir "/home/tvraman/.gnupg/")
(setq mc-gpg-user-id "tvraman@almaden.ibm.com")
(autoload 'mc-setversion "mc-setversion" nil t)
(mc-setversion "gpg")

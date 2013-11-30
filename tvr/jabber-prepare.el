(augment-load-path "emacs-jabber")

(load-library "jabber-autoloads")
(load-library "jabber")
(load-library "ssl")
(load-library "nm")
(add-hook 'nm-connected-hook 'jabber-connect-all)
(setq fsm-debug nil)
(add-hook 'nm-disconnected-hook 'jabber-disconnect)
(nm-enable)





(require 'tls)
;(setq tls-program '("gnutls-cli --insecure -p %p %h"))
;(setq tls-program '("gnutls-cli -p %p %h"))

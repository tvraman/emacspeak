;; -*- lexical-binding: nil; -*-
(defalias 'assoc-ignore-case 'assoc)
(define-key ctl-x-map "\C-j" jabber-global-keymap)
(with-eval-after-load "jabber"
  (setq fsm-debug nil)
  (setq jabber-mode-line-string
        (list " " 'jabber-mode-line-presence))
  (setq
   jabber-network-server "talk.google.com"
   jabber-account-list
   `((
      "raman@google.com" ;;; hard-wired for now 
      (:network-server . "talk.google.com")
      (:port . 5223)
      (:connection-type . ssl)))))

;; -*- lexical-binding: nil; -*-

(define-key ctl-x-map "\C-j" jabber-global-keymap)

(setq fsm-debug nil)
(add-hook 'jabber-roster-mode-hook
          #'(lambda nil
              (setq jabber-mode-line-string
      (list " "
              'jabber-mode-line-presence
              ;" "
              ;'jabber-mode-line-contacts
              ))
              (defalias 'assoc-ignore-case 'assoc)
              (setq buffer-undo-list t)))

(setq jabber-account-list
      `((
         ,(concat (user-login-name) "@" (or mail-host-address (system-name)))
         (:network-server . "talk.google.com")
         (:port . 5223)
         (:connection-type . ssl))))

;; -*- lexical-binding: t; -*-

;(load-library "jabber-autoloads")
(global-set-key (kbd "C-x C-j C-c") 'jabber-connect-all)
(setq fsm-debug nil)
(add-hook 'jabber-roster-mode-hook
          #'(lambda nil
              (setq jabber-mode-line-string
      (list " "
              'jabber-mode-line-presence
              ;" "
              ;'jabber-mode-line-contacts
              ))
              (setq buffer-undo-list t)))

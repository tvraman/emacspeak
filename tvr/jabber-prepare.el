;; -*- lexical-binding: nil; -*-

;(load-library "jabber-autoloads")
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

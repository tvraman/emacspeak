(augment-load-path "jabber")
(load-library "jabber")
(load-library "jabber-autoloads")
(load-library "ssl")
(load-library "nm")
(add-hook 'nm-connected-hook 'jabber-connect-all)
(add-hook 'nm-disconnected-hook 'jabber-disconnect)
(nm-enable)
(setq fsm-debug nil)
(add-hook 'jabber-roster-mode-hook
          #'(lambda nil
              (setq buffer-undo-list t)))

;;; terse modeline
(require 'jabber-modeline)
(setq jabber-mode-line-string (list " "
              'jabber-mode-line-presence
              ;" "
              ;'jabber-mode-line-contacts
              ))

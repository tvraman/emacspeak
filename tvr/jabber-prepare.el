;; -*- lexical-binding: nil; -*-
(define-key ctl-x-map (kbd "C-j") jabber-global-keymap)
(eval-after-load "jabber"
  `(progn
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

     (setq
      jabber-network-server "talk.google.com"
      jabber-account-list
      `((
         "raman@google.com" ;;; hard-wired for now 
         (:network-server . "talk.google.com")
         (:port . 5223)
         (:connection-type . ssl))))
     ))

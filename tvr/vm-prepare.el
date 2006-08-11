;;; Augment load path:
(augment-load-path "vm" "vm")
(load-library "vm")
(global-set-key "\M-\C-v" 'vm-visit-folder)
(defadvice vm-check-emacs-version(around work-in-20-emacs pre act com) t)

(add-hook 'vm-quit-hook 'vm-expunge-folder)
(add-hook 'vm-quit-hook 'display-time)
(global-set-key "\C-xm" 'vm-mail)
(add-hook 'vm-mode-hook
          (function
           (lambda nil
             (and (featurep 'emacspeak)
                  (define-key vm-mode-map '[delete]
                    'dtk-toggle-punctuation-mode)))))
;;{{{ w3m
;;;w3-region is broken --kluge to use w3m.
(defalias 'w3-region 'w3m-region)

;;}}}
;;{{{ spamassassin

(defun  vm-spam-assassinate ()
  "Assassinate spam using spamassassin."
  (interactive)
  (vm-pipe-message-to-command "spamassassin -r -w 'spamtrap1' 1>&- 2>&- &" nil)
  (vm-delete-message 1)
  (emacspeak-auditory-icon 'delete-object)
  (call-interactively 'vm-next-message))

(define-key vm-mode-map "\C-\M-s" 'vm-spam-assassinate)

 ;;}}}

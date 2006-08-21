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

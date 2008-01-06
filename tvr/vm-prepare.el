;;; Augment load path:
(augment-load-path "vm/lisp" "vm")
(load-library "vm")

(global-set-key "\M-\C-v" 'vm-visit-folder)
(defalias 'w3-region 'w3m-region)
(defadvice vm-check-emacs-version(around work-in-20-emacs pre act com) t)

(add-hook 'vm-quit-hook 'vm-expunge-folder)
(global-set-key "\C-xm" 'vm-mail)
(add-hook 'vm-mode-hook
          (function
           (lambda nil
             (and (featurep 'emacspeak)
                  (define-key vm-mode-map '[delete]
                    'dtk-toggle-punctuation-mode)))))

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

;;{{{ GMail:
;; I set these via custom, 
;; and in addition, I push 
;; (list "~/gbox" (first vm-imap-server-list) "~/gbox.crash")
;; on to vm-spool-files.
;; Required
;(setq vm-imap-server-list
      ;'("imap-ssl:imap.gmail.com:993:inbox:login:tv.raman.tv@gmail.com:*"))

;; Optional
;(setq vm-imap-folder-cache-directory (expand-file-name "IMAP"
;vm-folder-directory))
;(setq vm-imap-save-to-server t)
(defcustom vm-gmail-email-address
  "tv.raman.tv@gmail.com"
  "Set this to your GMail address.")

(defun vm-gmail-spam (&optional count)
  "Save to IMAP Spam folder."
  (interactive (list (prefix-numeric-value current-prefix-arg)))
  (vm-save-message-to-imap-folder
   (format "imap-ssl:imap.gmail.com:993:[Gmail]/Spam:login:%s:*"
           vm-gmail-email-address)
           count)
  (call-interactively 'vm-next-message))

(define-key vm-mode-map "!" 'vm-gmail-spam)
(define-key vm-mode-map "ls" 'vm-gmail-spam)

 ;;}}}

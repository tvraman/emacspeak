;;; vm-prepare.l :  -*- lexical-binding: t; -*-
(augment-load-path "vm/lisp" "vm-autoloads")
;(load-library "vm-autoloads")
(autoload 'vm "vm" "vm" t)
(autoload 'vm-visit-folder "vm" "vm" t)
(global-set-key "\M-\C-v" 'vm-visit-folder)
(eval-after-load "vm"
  `(progn
(add-hook 'vm-quit-hook #'vm-expunge-folder)
;(global-set-key "\C-xm" 'vm-mail)


(setq vm-postponed-messages (expand-file-name "~/Mail/crash"))

(setq vm-auto-displayed-mime-content-type-exceptions nil)
(defun vm-mime-display-internal-shr-text/html (start end layout)
  "Use shr to inline HTML mails in the VM presentation buffer."
    (shr-render-region start (1- end))
    (put-text-property start end
                       'text-rendered-by-shr t))

;;; has to be done indirectly
;;; Fake emacs-w3m, though we actually use shr

(setq vm-mime-text/html-handler'emacs-w3m  )
(defalias 'vm-mime-display-internal-emacs-w3m-text/html  'vm-mime-display-internal-shr-text/html)

(defun vm-chromium ()
  "Run Chromium on current link."
  (interactive)
  (let ((url
         (or (get-text-property (point) 'shr-url) (browse-url-url-at-point))))
    (unless url (error "No link here."))
    (browse-url-chrome url)
    (message "Opening %s with Chrome" url)))

(define-key vm-mode-map "C" 'vm-chromium)
))

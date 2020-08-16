;;; vm-prepare.l :  -*- lexical-binding: nil; -*-
(push (expand-file-name "vm/lisp/" emacs-personal-library) load-path)
(autoload 'vm "vm" "vm" t)
(autoload 'vm-visit-folder "vm" "vm" t)
(global-set-key "\M-\C-v" 'vm-visit-folder)

(eval-after-load "vm"
  `(progn
     (load "vm-autoloads")
     (when (require 'bbdb) (bbdb-insinuate-vm))

     (global-set-key "\C-xm" 'vm-mail)


(defun vm-mime-display-internal-shr-text/html (start end _layout)
  "Use shr to inline HTML mails in the VM presentation buffer."
    (shr-render-region start (1- end))
    (put-text-property start end 'text-rendered-by-shr t))
     
;;; has to be done indirectly
;;; Fake emacs-w3m, though we actually use shr

     (setq vm-mime-text/html-handler'emacs-w3m  )
     (defalias 'vm-mime-display-internal-emacs-w3m-text/html  'vm-mime-display-internal-shr-text/html)

     (defun vm-chromium ()
       "Run Chromium on current link."
       (interactive)
       (let ((url (browse-url-url-at-point)))
         (unless url (error "No link here."))
         (dtk-stop)
         (browse-url-chrome url)
         (message "Opening url with Chrome")))

     (define-key vm-mode-map "C" 'vm-chromium)
     (define-key vm-mode-map "o" 'mspools-show)
     (load-library "mspools")
     ))

(eval-after-load
    "mspools"
  `(progn
     (defun mspools-compute-size (file)
       (let ((message-log-max nil)
             (inhibit-message t)
             (emacspeak-speak-messages nil)))
         (read (shell-command-to-string (format "grep '^From ' %s | wc -l" file))))

     (defun mspools-size-folder (spool)
    "Return (SPOOL . SIZE ) iff SIZE of spool file is non-zero."
    (let ((size (mspools-compute-size (expand-file-name  spool mspools-folder-directory))))
      (unless (zerop size)
        (cons spool size))))))

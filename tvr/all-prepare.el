;;; vm-prepare.l :  -*- lexical-binding: nil; -*-

(autoload 'vm "vm" "vm mail reader" t nil)
(autoload 'vm-visit-folder "vm" "Open VM folder" t nil)
(with-eval-after-load "vm"
  (global-set-key "\C-xm" 'vm-mail)
  (when (require 'bbdb) (bbdb-insinuate-vm)))

(defun make-local-hook (hook)
  "compatibility"
  (if (local-variable-p hook)
      nil
    (or (boundp hook) (set hook nil))
    (make-local-variable hook)
    (set hook (list t)))
  hook)
;;;  Gnus Setup For GMail imap:  -*- lexical-binding: nil; -*-
;;; Read GMailusing gnus  with 2-factor (Oauth2) authentication.
;;; Uses auth-source-xoauth2:
;;; https://github.com/ccrusius/auth-source-xoauth2
;;; That module extends Emacs' auth-source with xoauth2 support.
;;; This module sets things up for GMail.
;;;Using  a file-based creds store.
;;{{{ Requires:

(eval-after-load "gnus"
  `(progn

     (require 'cl-lib)
     (require 'auth-source-xoauth2)
     (require 'smtpmail)

     (defvar file-xoauth2-creds-location
       (expand-file-name "~/.xoauth2-creds.gpg")
       "Where we store our tokens.
This file should be GPG encrypted --- Emacs will  decrypt on load.")

     (setq auth-source-xoauth2-creds  file-xoauth2-creds-location)
     (auth-source-xoauth2-enable)
     (add-to-list 'smtpmail-auth-supported 'xoauth2)

     ;;}}}
     ;;{{{ Tests:

;;; (auth-source-xoauth2--search nil nil "gmail" "raman@google.com""993")
;;; (auth-source-search :host "smtp.gmail.com" :user "raman@google.com" :type 'xoauth2 :max 1 :port "465")

     ;;}}}
     ;;{{{ Sending Mail:

;;;  Set send-mail-function via custom.
     (setq
                                        ;smtpmail-debug-info t
                                        ;smtpmail-debug-verb t
      smtpmail-stream-type 'ssl
      smtpmail-smtp-user "raman@google.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465)

     ;;}}}
     ;;{{{GMail Using xoauth2  and Gnus:
     (cl-declaim (special gnus-select-method gnus-secondary-select-methods))
     (setq
      gnus-select-method
      `(nnimap
        "gmail"
        (nnimap-address "imap.gmail.com")
        (nnimap-server-port 993)
        (nnimap-user "raman@google.com")
        (nnimap-authenticator xoauth2)
        (nnimap-expunge always)
        (nnmail-expiry-wait immediate)
        (nnimap-streaming t)
        (nnimap-stream ssl)))

     (defun gm-user-to-nnimap (user)
       "Return nnimap select method for sspecified user."
       `(nnimap
         ,user
         (nnimap-user ,(format "%s@gmail.com" user))
         (nnimap-authenticator xoauth2)
         (nnimap-address "imap.gmail.com")
         (nnimap-server-port 993)
         (nnmail-expiry-wait immediate)
         (nnimap-streaming t)
         (nnimap-stream ssl)))

     (setq gnus-secondary-select-methods
           (mapcar #'gm-user-to-nnimap
                   '( "tv.raman.tv" "emacspeak")))

     ;;}}}
     ;;{{{Additional gnus settings:

     (setq gnus-auto-subscribed-groups nil)
     (defun gmail-report-spam ()
       "Report the current or marked mails as spam.
This moves them into the Spam folder."
       (interactive)
       (make-thread
        #'(lambda ()
            (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/Spam")
            (emacspeak-auditory-icon 'task-done))))
     (defun gmail-unspam ()
       "Move incorrectly marked spam to inbox"
       (interactive)
       (make-thread
        #'(lambda ()
            (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/inbox")
            (emacspeak-auditory-icon 'task-done))))
     
     
     (define-key gnus-summary-mode-map "$" 'gmail-report-spam)
     ;;}}}
     ))
(defun tvr-unlock-xoauth ()
  "Unlock xoauth creds if gpg-agent has timed out."
  (interactive )
  (cl-declare (special file-xoauth2-creds-location))
  (kill-buffer (find-file-noselect file-xoauth2-creds-location)))
(when (keymapp emacspeak-ctl-z-keymap )
  
  (define-key emacspeak-ctl-z-keymap "u" 'tvr-unlock-xoauth))
(setq mm-file-name-rewrite-functions
                '(mm-file-name-trim-whitespace
                  mm-file-name-collapse-whitespace
                  mm-file-name-replace-whitespace))

;;{{{ Utils:

(defun google-py-oauth2-cli (user app-secret)
  "generate command-line for pasting into a shell."
  (kill-new
   (format
    "python oauth2.py --user %s --client_id %s --client_secret %s   --generate_oauth2_token"
    user
    (plist-get app-secret :client-id)
    (plist-get app-secret :client-secret))))

;;; Usage:
;;;(google-py-oauth2-cli "tv.raman.tv@gmail.com" file-app-secrets)
;;;(google-py-oauth2-cli "emacspeak@gmail.com" file-app-secrets)


(defadvice auth-source-xoauth2--file-creds (around emacspeak pre act comp)
  "Silence messages"
  (let ((emacspeak-speak-messages nil))
    ad-do-it
    ad-return-value))

;;; For message.el in emacs 28
(with-eval-after-load "message"
  (push 'signature message-shoot-gnksa-feet))

(provide 'file-xoauth2)
;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
;; -*- lexical-binding: nil; -*-
(defalias 'assoc-ignore-case 'assoc)
(define-key ctl-x-map "\C-j" jabber-global-keymap)
(with-eval-after-load "jabber"
  (setq fsm-debug nil)
  (setq jabber-mode-line-string
        (list " " 'jabber-mode-line-presence))
  (setq
   jabber-network-server "talk.google.com"
   jabber-account-list
   `((
      "raman@google.com" ;;; hard-wired for now 
      (:network-server . "talk.google.com")
      (:port . 5223)
      (:connection-type . ssl)))))
(with-eval-after-load "smartparens"
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (define-key  smartparens-mode-map "\C-\M-a" 'beginning-of-defun)
  (define-key  smartparens-mode-map "\C-\M-e" 'end-of-defun)
  (define-key  smartparens-mode-map "\C-\M-k" 'kill-sexp)
  (define-key smartparens-mode-map "\M-a" 'sp-backward-down-sexp)
  (define-key smartparens-mode-map "\M-e" 'sp-up-sexp)
  (define-key smartparens-mode-map "\M-k" 'sp-kill-sexp)
  (define-key smartparens-mode-map "\C-\M-f" 'forward-sexp)
  (define-key smartparens-mode-map "\C-\M-b" 'backward-sexp))
(define-key emacs-lisp-mode-map (ems-kbd "C-c e") 'macrostep-expand)
(defun conditionally-enable-lispy ()
  (when (memq this-command '(eval-expression emacspeak-wizards-show-eval-result))
    (lispy-mode 1)))
(with-eval-after-load "lispy"
  (cl-declare (special lispy-mode-map lispy-mode-map-lispy))
  (define-key lispy-mode-map (ems-kbd "C-,") nil)
  (define-key lispy-mode-map-lispy (ems-kbd "C-,") nil)
  (define-key lispy-mode-map "\M-m" nil)
  (define-key lispy-mode-map "\C-y" 'emacspeak-muggles-yank-pop/yank)
  (define-key lispy-mode-map ";" 'self-insert-command)
  (define-key lispy-mode-map ":" 'self-insert-command)
  (define-key lispy-mode-map "\M-;" 'lispy-comment)
  (define-key lispy-mode-map "\C-d" 'delete-char)
  (define-key lispy-mode-map "\M-\C-d" 'lispy-delete)
  (define-key lispy-mode-map "\M-d" 'kill-word)
  (define-key lispy-mode-map "a" 'special-lispy-beginning-of-defun)
;;; lispy in ielm
  (add-hook 'ielm-mode-hook 'lispy-mode)
;;;  Lispy for eval-expression:
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy))
(with-eval-after-load "slime"
  (setq inferior-lisp-program (executable-find "sbcl"))
  (setq common-lisp-hyperspec-root
        (if (file-exists-p "/usr/share/doc/hyperspec/")
            "file:///usr/share/doc/hyperspec/"
          "http://www.lispworks.com/reference/HyperSpec/"))
  (global-set-key (ems-kbd "C-c s") 'slime-selector)
  (setq slime-contribs '(slime-fancy slime-hyperdoc slime-quicklisp))
  (slime-setup)
  (slime-autodoc--disable)
  (setq slime-use-autodoc-mode nil)
  (setq
   slime-lisp-implementations
   `((sbcl ("sbcl" "--core"
            ,(expand-file-name "sbcl-core-for-slime" user-emacs-directory))))))
;;;$Id: org-prepare.el 6727 2011-01-14 23:22:20Z tv.raman.tv $  -*- lexical-binding: nil; -*-

(with-eval-after-load "org"
  (require 'org-tempo)
  (require 'ol-eww)
  (require 'ox-md)
  (define-key org-mode-map (ems-kbd "C-c m") 'org-md-export-as-markdown)
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-cb" 'org-switchb)
  (define-key global-map  "\C-cc" 'org-capture)
  )

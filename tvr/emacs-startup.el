;;; Emacs initialization file for Raman:  -*- lexical-binding: t; -*-
;;{{{ History:
;;; Segre March 22 1991
;;; July 15, 2001 finally cutting over to custom.
;;; August 12, 2007: Cleaned up for Emacs 22

;;}}}
;;{{{ personal lib
(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))


(defvar emacs-personal-library
  (expand-file-name "~/emacs/lisp/site-lisp")
  "Directory where we keep site libraries. Mostly superceded by elpa.")

(defvar tvr-libs
  '(
    "kbd-setup" "emacspeak-muggles-autoloads"
    "vm-prepare" "gnus-prepare"  "bbdb-prepare" "elfeed-prepare"
    "vdiff-prepare"  "sp-prepare"
    "auctex-prepare"  "folding-prepare" "org-prepare"
    "calc-prepare" "helm-prepare"                      ;helm not activated
    "js-prepare" "tcl-prepare" "slime-prepare" "yasnippet-prepare"
    "python-mode-prepare" "projectile-prepare" "iplayer-prepare"
    "erc-prepare" "jabber-prepare" "twittering-prepare")
  "Libraries we load.")

;;}}}
;;{{{ helper functions:





(defsubst tvr-time-it (start what)
  "Emit timing information."
  (message "<%s %.4f %d gcs %.4f>"
           what (float-time (time-subtract (current-time) start))
           gcs-done gc-elapsed))

(defsubst load-library-if-available (lib)
  "Safe load library."
  (let ((start (current-time))
        (file-name-handler-alist nil)
        (inhibit-message t)
        (gc-cons-threshold 64000000)
        (emacspeak-speak-messages nil))
    (condition-case err
        (progn
          (load-library lib)
          (tvr-time-it start lib))
      (error (message "Error loading %s: %s" lib (error-message-string err))))))

;;}}}
;;{{{ tvr-shell-bind-keys:

(defsubst tvr-shell-bind-keys ()
  "Set up additional shell mode keys."
  (cl-declare (special shell-mode-map))
  (cl-loop ;;; global keys
   for i from 0 to 9 do
   (global-set-key (kbd (format "C-c %s" i)) 'emacspeak-wizards-shell-by-key))
  (cl-loop ;;; global  keys
   for  key in
   '(
     ("C-c <" emacspeak-wizards-previous-shell)
     ("C-c >" emacspeak-wizards-next-shell))
   do
   (global-set-key (kbd (cl-first key)) (cl-second key)))
  (cl-loop ;;; shell mode bindings
   for b in
   '(
     ("SPC" comint-magic-space)
     ("C-c k" comint-clear-buffer)
     ("C-c r" comint-redirect-send-command))
   do
   (define-key shell-mode-map (kbd (cl-first b)) (cl-second b))))

;;}}}
;;{{{ customize custom

(defun tvr-customize ()
  "Load my customizations from my custom-file."
  (cl-declare (special custom-file))
  (let ((file-name-handler-alist nil)
        (gc-cons-threshold  32000000)
        (inhibit-message t)
        (emacspeak-speak-messages nil))
    (setq-default custom-file (expand-file-name "~/.customize-emacs"))
    (define-key esc-map "\M-:" 'emacspeak-wizards-show-eval-result)
    (when (file-exists-p custom-file) (load custom-file))))

(defun tvr-defer-muggles ()
  "Defered muggles loader."
  (unless (featurep 'emacspeak-muggles)
    (make-thread
     #'(lambda ()
         (let ((file-name-handler-alist nil)
               (gc-cons-threshold 64000000))
           (load-library-if-available "emacspeak-muggles"))))))

;;}}}
(defun start-up-my-emacs()
  "Start up emacs for me. "
  (cl-declare (special emacs-personal-library emacspeak-directory
                       outline-mode-prefix-map))
  (let ((gc-cons-threshold 64000000)
        (file-name-handler-alist nil)   ; to speed up, avoid tramp etc
        (emacspeak-speak-messages nil)
        (inhibit-message t)
        (tvr-start (current-time)))
    ;;{{{ Basic Look And Feel:

    (setq inhibit-startup-echo-area-message user-login-name
          initial-scratch-message ""
          initial-buffer-choice t
          text-quoting-style 'grave)
    (tooltip-mode -1)
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (fringe-mode 0)

    (put 'upcase-region 'disabled nil)
    (put 'downcase-region 'disabled nil)
    (put 'narrow-to-region 'disabled nil)
    (put 'eval-expression 'disabled nil)
    (put 'timer-list 'disabled nil)

    ;;}}}
    ;;{{{ Extend  Load Path:
    
    (when (file-exists-p  emacs-personal-library)
      (push emacs-personal-library load-path))
    (package-initialize)
    ;;}}}
    ;;{{{  set up terminal codes and global keys

    (prefer-coding-system 'utf-8-emacs)
    (cl-loop
     for  key in
     '(
       ([f3] bury-buffer)
       ([f4] emacspeak-kill-buffer-quietly)
       ("\M--" undo)
       ([f11]shell)
       ([f12]vm)
       ( "\M-r"replace-string)
       ( "\M-e"emacspeak-wizards-end-of-word)
       ( "\M-\C-j"imenu)
       ( "\M-\C-c"calendar))
     do
     (global-set-key (cl-first key) (cl-second key)))
    
;;; Smarten up ctl-x-map
    (define-key ctl-x-map "\C-n" 'forward-page)
    (define-key ctl-x-map "\C-p" 'backward-page)

;;; Shell navigation:
    (eval-after-load "shell" `(progn (tvr-shell-bind-keys)))

    ;;}}}
    ;;{{{ Load and customize emacspeak
    
    (load-library-if-available (expand-file-name"~/emacs/lisp/emacspeak/lisp/emacspeak-setup.elc"))
    (when (file-exists-p (expand-file-name "tvr/" emacspeak-directory))
      (push (expand-file-name "tvr/" emacspeak-directory) load-path))
    ;;}}}
    ;;{{{ outline mode setup:

    (eval-after-load 'outline
      `(progn
;;;restore what we are about to steal
         (define-key outline-mode-prefix-map "o" 'open-line)
         (global-set-key "\C-o"outline-mode-prefix-map)
         ))

    ;;}}}
    ;;{{{ Prepare needed libraries

;;; mail-abbrevs-setup added to mail-mode hooks in custom.

    ;;}}}
    ;;{{{ turn on modes:
    (add-hook 'prog-mode-hook 'company-mode)
    (add-hook 'text-mode-hook 'auto-correct-mode)
    (savehist-mode )
    (save-place-mode)
    (midnight-mode)
    (server-start)
    (pinentry-start)
    (bbdb-insinuate-vm))

  ;;}}}
  ) ;end defun
;;{{{  start it up
(defun tvr-after-init ()
  "Actions to take after Emacs is up and ready."
  (cl-declare (special emacspeak-sounds-directory tvr-libs))
  (let ((after-start (current-time))
        (gc-cons-threshold 64000000)
        (file-name-handler-alist nil)
        (inhibit-message t)
        (emacspeak-speak-messages nil))
    (dynamic-completion-mode 1)
    (completion-initialize)
    (mapc #'load-library-if-available tvr-libs)
    (run-with-idle-timer  0.1  nil  #'tvr-defer-muggles)
    (tvr-customize)
    (soundscape-toggle)
    (setq frame-title-format '(multiple-frames "%b" ( "Emacs")))
    (require 'emacspeak-dbus)
    (when (dbus-list-known-names :session)
      (nm-enable)
      (emacspeak-dbus-sleep-enable)
      (emacspeak-dbus-watch-screen-lock))
    (emacspeak-wizards-project-shells-initialize)
    (start-process
     "play" nil "play"
     (expand-file-name "highbells.au" emacspeak-sounds-directory))
    (tvr-time-it after-start "after-init")))
(add-hook 'after-init-hook #'tvr-after-init)
(add-hook
 'emacs-startup-hook
 #'(lambda ()
     (delete-other-windows)
     (message "<Successfully initialized Emacs for %s in %s with %s gcs (%.4fs)>"
              user-login-name (emacs-init-time) gcs-done gc-elapsed)))


(start-up-my-emacs)
;;}}}
(provide 'emacs-startup)
;;{{{  emacs local variables

;;;local variables:
;;;folded-file: t
;;;end:

;;}}}

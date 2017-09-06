;;; Emacs initialization file for Raman:  -*- lexical-binding: t; -*-
;;{{{ History:
;;; Segre March 22 1991
;;; July 15, 2001 finally cutting over to custom.
;;; August 12, 2007: Cleaned up for Emacs 22

;;}}}
;;{{{ personal lib
(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(defvar emacspeak-speak-messages)
(defvar emacs-private-library (expand-file-name "~/.elisp")
  "Private library directory. ")

(defvar emacs-personal-library
  (expand-file-name "~/emacs/lisp/site-lisp")
  "Directory where we keep site libraries. Mostly superceded by elpa.")

;;}}}
;;{{{ helper functions:

;;; This is for setting variables customized via custom.

(defmacro csetq (variable value)
  "Exactly like setq, but handles custom."
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))

(defun augment-load-path (path &optional library whence at-end)
  "add directory to load path. Path is resolved relative to `whence'
which defaults to emacs-personal-library."
  (interactive "Denter directory name: ")
  (cl-declare (special emacs-personal-library))
  (unless (and library (locate-library library))
    (add-to-list
     'load-path
     (expand-file-name
      path
      (or
       whence
       (and (boundp 'emacs-personal-library) emacs-personal-library)))
     at-end))
  (when library (locate-library library)))

(defsubst tvr-time-it (start what)
  "Emit timing information."
  (message
   "<%s %.4f>"
   what (float-time (time-subtract (current-time) start))))

(defsubst load-library-if-available (lib)
  "Safe load library."
  (let ((start (current-time))
        (file-name-handler-alist nil)
        (inhibit-message t)
        (emacspeak-speak-messages nil))
    (condition-case nil
        (progn
          (load-library lib)
          (tvr-time-it start lib))
      (error (message "Error loading %s" lib)))))

;;}}}
;;{{{ tvr-shell-bind-keys:

(defun tvr-shell-bind-keys ()
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
        (gc-cons-threshold  16000000)
        (inhibit-message t)
        (emacspeak-speak-messages nil))
    (setq-default custom-file (expand-file-name "~/.customize-emacs"))
    (when (file-exists-p custom-file) (load custom-file))))

(defun tvr-defer-muggles ()
  "Defered muggles loader."
  (unless (featurep 'emacspeak-muggles)
    (let ((file-name-handler-alist nil))
      (make-thread #'(lambda () (load-library-if-available "emacspeak-muggles"))))))

;;}}}
(defun start-up-my-emacs()
  "Start up emacs for me. "
  (cl-declare (special emacs-personal-library emacs-private-library
                       emacspeak-directory
                       enable-completion outline-mode-prefix-map))
  (let ((gc-cons-threshold 16000000)
        (file-name-handler-alist nil) ; to speed up, avoid tramp etc
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
    ;;{{{ Augment Load Path:

    (when (file-exists-p  emacs-private-library)
      (push emacs-private-library load-path))

    (when (file-exists-p  emacs-personal-library)
      (push emacs-personal-library load-path))
    (package-initialize)
    ;;}}}
    ;;{{{ Load and customize emacspeak

    (let ((e-start (current-time)))
      (load (expand-file-name"~/emacs/lisp/emacspeak/lisp/emacspeak-setup.elc"))
      (tvr-time-it e-start "emacspeak"))
    (when (file-exists-p (expand-file-name "tvr/" emacspeak-directory))
      (push (expand-file-name "tvr/" emacspeak-directory) load-path))

    ;;}}}
    ;;{{{  set up terminal codes and global keys

    (prefer-coding-system 'utf-8-emacs)
    (cl-loop
     for  key in
     '(
       ([f3] bury-buffer)
       ([f4] emacspeak-kill-buffer-quietly)
       ([pause] dtk-stop)
       ("\M--" undo)
       ([f11]shell)
       ([f12]vm)
       ( "\C-xc"compile)
       ( "\M-r"replace-string)
       ( "\M-e"end-of-word)
       ( "\M-\C-j"imenu)
       ( "\M-\C-c"calendar))
     do
     (global-set-key (cl-first key) (cl-second key)))

    (global-set-key [S-return] 'other-window)

;;; Smarten up ctl-x-map
    (define-key ctl-x-map "\C-n" 'forward-page)
    (define-key ctl-x-map "\C-p" 'backward-page)

;;; Shell navigation:
    (eval-after-load "shell" `(progn (tvr-shell-bind-keys)))

    ;;}}}
    ;;{{{  Basic Support Libraries

    (require 'dired-x)
    (require 'dired-aux)
    (dynamic-completion-mode)
    (unless enable-completion (completion-mode ))

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

    (mapc
     #'load-library-if-available
     '(
       "kbd-setup"
       "emacspeak-m-player" "emacspeak-muggles-autoloads"
       "my-functions"
;;; Mail:
       "vm-prepare" "gnus-prepare"  "bbdb-prepare"  "mspools-prepare"
       "vdiff-prepare" "elfeed-prepare"
;;; Authoring:
       "auctex-prepare"  "folding-prepare"
       "calc-prepare"
       "helm-prepare"   ;helm not activated
       "js-prepare" "tcl-prepare" "slime-prepare" "yasnippet-prepare"
       "python-mode-prepare" "projectile-prepare"
       "org-prepare"
       "erc-prepare" "jabber-prepare" "twittering-prepare"
       "iplayer-prepare"
                                        ;"auto-correct-prepare"
                                        ;"color-theme-prepare"
       ))

    ;;}}}
    ;;{{{ turn on modes:
    (add-hook 'prog-mode-hook 'company-mode)
    (add-hook 'text-mode-hook 'auto-correct-mode)
    (initialize-completions)
    (savehist-mode )
    (save-place-mode)
    (midnight-mode)
    (server-start)
    (pinentry-start)
    (bbdb-insinuate-vm)

    ;;}}}
    (tvr-time-it tvr-start "start-up-my-emacs"))) ;end defun
;;{{{  start it up
(defun tvr-after-init ()
  "Actions to take after Emacs is up and ready."
  (cl-declare (special emacspeak-sounds-directory))
  (let ((after-start (current-time))
        (gc-cons-threshold 16000000)
        (file-name-handler-alist nil)
        (inhibit-message t)
        (emacspeak-speak-messages nil))
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
    (tvr-time-it after-start "after-init")
    (message "<Successfully initialized Emacs for %s in %s>"
             user-login-name (emacs-init-time))))
(add-hook 'after-init-hook #'tvr-after-init)
(add-hook 'emacs-startup-hook #'delete-other-windows)

(start-up-my-emacs)
;;}}}
(provide 'emacs-startup)
;;{{{  emacs local variables

;;;local variables:
;;;folded-file: t
;;;end:

;;}}}

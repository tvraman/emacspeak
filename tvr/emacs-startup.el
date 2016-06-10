;;{{{ History: Emacs initialization file for Raman:

;;; $Id: emacs-startup.el 7753 2012-05-06 22:43:52Z tv.raman.tv $
;;; Segre March 22 1991
;;; July 15, 2001 finally cutting over to custom.
;;; August 12, 2007: Cleaned up for Emacs 22

;;}}}
;;{{{ personal lib

(setq inhibit-startup-echo-area-message user-login-name)

(defvar emacs-private-library (expand-file-name "~/.elisp")
  "Private personalization directory. ")

(defvar emacs-personal-library
  (expand-file-name "~/emacs/lisp/site-lisp")
  "Directory where we keep personal libraries.
These are exported to GitHub under emacspeak/tvr.")

;;}}}
;;{{{ helper functions:

;;; This is for setting variables customized via custom.

(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))

(defsubst augment-load-path (path &optional library whence at-end)
  "add directory to load path.
Path is resolved relative to `whence' which defaults to emacs-personal-library."
  (interactive "Denter directory name: ")
  (declare (special emacs-personal-library))
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

(defsubst augment-auto-mode-alist (ext mode)
  "Add to auto-mode-alist."
  (declare (special auto-mode-alist))
  (setq auto-mode-alist
        (cons
         (cons ext mode)
         auto-mode-alist)))

(defsubst load-library-if-available (lib)
  "Load a library only if it is around"
  (let ((emacspeak-speak-messages nil))
    (condition-case nil
        (cond
         ((locate-library lib)
          (load-library lib)
          (message "Loaded %s" lib)
          t)
         (t (message "Could not locate library %s" lib)
            nil))
      (error (message
              "Error loading %s"
              lib)))))

;;}}}
;;{{{ customize custom

(declare (special custom-file))
(setq custom-file (expand-file-name "~/.customize-emacs"))

;;}}}
(defun start-up-my-emacs()
  "Start up emacs for me. "
  (declare (special emacs-personal-library emacs-private-library))
  (let ((gc-cons-threshold 8000000))
    ;;{{{ Basic Look And Feel:

    (tooltip-mode -1)
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (fringe-mode 0)
    (setq text-quoting-style 'grave)
    (put 'upcase-region 'disabled nil)
    (put 'downcase-region 'disabled nil)
    (put 'narrow-to-region 'disabled nil)
    (put 'eval-expression 'disabled nil)

    ;;}}}
    ;;{{{ Augment Load Path:

    (when (file-exists-p  emacs-private-library)
      (augment-load-path emacs-private-library ))

    (when (file-exists-p  emacs-personal-library)
      (augment-load-path emacs-personal-library))

    ;;}}}
    ;;{{{ Load and customize emacspeak

    (unless (featurep 'emacspeak)
      (load-file
       (expand-file-name "~/emacs/lisp/emacspeak/lisp/emacspeak-setup.el")))
    (when (featurep 'emacspeak)
      (emacspeak-tts-startup-hook)
      (emacspeak-sounds-select-theme "pan-chimes/"))
    (add-to-list 'load-path (expand-file-name "tvr/" emacspeak-directory))

    ;;}}}
    ;;{{{  set up terminal codes and global keys

    (mapc #'load-library-if-available '("console" "screen"))

    (when (eq window-system 'x) (load-library-if-available "x"))

    (loop for  key in
          '(
            ([f3] bury-buffer)
            ([f4] emacspeak-kill-buffer-quietly)
            ([f5] emacspeak-pianobar)
            ([pause] dtk-stop)
            ("\M--" undo)
            ([f6] dtk-cloud)
            ( [f8]emacspeak-remote-quick-connect-to-server)
            ([f11]shell)
            ([f12]vm)
            ( "\C-xc"compile)
            (  "\C-x%"comment-region)
            ( "\M-r"replace-string)
            ( "\M-e"end-of-word)
            ( "\M-\C-j"imenu)
            ( "\M-\C-c"calendar))
          do
          (global-set-key (first key) (second key)))

;;; Smarten up ctl-x-map
    (define-key ctl-x-map "\C-n" 'forward-page)
    (define-key ctl-x-map "\C-p" 'backward-page)

    ;;}}}
    ;;{{{  Basic Support Libraries

    (require 'dired-x)
    (require 'dired-aux)
    (dynamic-completion-mode)
    (unless enable-completion (completion-mode ))

    ;;}}}
    ;;{{{  different mode settings

;;; Mode hooks.

    (eval-after-load
        "shell"
      '(progn
         (define-key shell-mode-map "\C-cr" 'comint-redirect-send-command)
         (define-key shell-mode-map "\C-ch" 'emacspeak-wizards-refresh-shell-history)))

    ;;}}}
    ;;{{{ outline mode setup:

    (load-library "outline")
                                        ;restore what we are about to steal
    (define-key outline-mode-prefix-map "o" 'open-line)
    (global-set-key "\C-o"outline-mode-prefix-map)

    ;;}}}
    ;;{{{ Prepare needed libraries

    (package-initialize)
    (add-hook 'mail-mode-hook 'mail-abbrevs-setup)
    (add-hook 'vm-mail-mode-hook 'mail-abbrevs-setup)
    (mapc
     #'load-library-if-available
     '(
       "my-functions"
;;; Mail:
       "vm-prepare" "gm-smtp" "gnus-prepare" "bbdb-prepare"
       "mspools-prepare" "sigbegone"
;;; Web:
       "w3-prepare" "elfeed-prepare"
;;; Authoring:
       "auctex-prepare" "nxml-prepare" "folding-prepare"
       "calc-prepare"
       "hydra-prepare" "helm-prepare"   ;helm not activated 
       "js-prepare" "tcl-prepare" "slime-prepare" "company-prepare" "python-mode-prepare"
                                        ; jde and ecb will pull in cedet.
                                        ;"jde-prepare" "ecb-prepare"
       "org-prepare"
       "erc-prepare" "jabber-prepare" "twittering-prepare"
       "tramp-prepare"  "fap-prepare"
       "emms-prepare" "iplayer-prepare"
       "auto-correct-setup"
       "color-theme-prepare" "elscreen-prepare"
       "local"))

    ;;}}}
    ;;{{{ turn on modes:

    (initialize-completions)
    (savehist-mode )
    (save-place-mode)
    (midnight-mode)
    (server-start)
    (bbdb-insinuate-vm)

    ;;}}}
    ))                                  ; end defun
;;{{{  start it up

(add-hook
 'after-init-hook
 #'(lambda ()
     ;;; Need to do this for Emacspeak Customizations having :set
     (mapcar
      #'custom-reevaluate-setting
      '(emacspeak-media-location-bindings))
     (soundscape-toggle)
     (shell)
     (setq frame-title-format '(multiple-frames "%b" ( "Emacs")))
     (calendar)
     (shell-command
      (format "aplay %s"
              (expand-file-name "highbells.au" emacspeak-sounds-directory)))
     (message "Successfully initialized Emacs")))
(when (file-exists-p custom-file) (load-file custom-file))
(start-up-my-emacs)

;;}}}
(provide 'emacs-startup)
;;{{{  emacs local variables
;;;local variables:
;;;folded-file: t
;;;end:
;;}}}

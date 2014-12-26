;;{{{ Emacs initialization file for Raman:
;;; $Id$
;;; Segre March 22 1991
;;; July 15, 2001 finally cutting over to custom.
;;; August 12, 2007: Cleaned up for Emacs 22
;;}}}
;;{{{ personal lib

(defvar emacs-private-library
  (expand-file-name "~/.elisp")
  "Private personalization directory. ")
(defvar emacs-personal-library
  (expand-file-name "~/emacs/lisp/site-lisp")
  "Directory where we keep personal libraries")

;;}}}
;;{{{ helper functions:

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
      (or whence (and (boundp 'emacs-personal-library) emacs-personal-library)))
     at-end)))

(defsubst augment-auto-mode-alist (ext mode)
  "Add to auto-mode-alist."
  (declare (special auto-mode-alist))
  (add-to-list 'auto-mode-alist (cons ext mode)))

(defsubst load-library-if-available (lib)
  "Load a library only if it is around"
  (condition-case nil
        (when (locate-library lib)
          (load-library lib) (message "Loaded %s" lib))
      (error (message "Error loading %s" lib))))
;;}}}
;;{{{ customize custom

(declare (special custom-file))
(setq custom-file (expand-file-name "~/.customize-emacs"))
(setq message-log-max 1024)

;;}}}
;;{{{ Configure packages  after load:

(defun emacs-startup-configure-packages ()
  "Called from after-init-hook to configure packages."
  (mapc
   #'load-library-if-available
   '("auctex-prepare" "gm-smtp")))

;;}}}
(defun start-up-my-emacs()
  "Start up emacs for me. "
  (declare (special emacs-personal-library emacs-private-library))
  (let ((gc-cons-threshold 8000000)
        (debug-on-quit t)
        (debug-on-error t))
    (setq outline-minor-mode-prefix "\C-x@h")
    
    (when (file-exists-p  emacs-private-library)
      (augment-load-path emacs-private-library ))
    (when (file-exists-p  emacs-personal-library)
      (augment-load-path emacs-personal-library))
    ;;{{{ Load and customize emacspeak

    (unless (featurep 'emacspeak)
      (load-file
       (expand-file-name "~/emacs/lisp/emacspeak/lisp/emacspeak-setup.el")))
    (when (featurep 'emacspeak)
      (emacspeak-toggle-auditory-icons t)
      (emacspeak-sounds-select-theme "chimes-stereo/")
      (emacspeak-tts-startup-hook))

    ;;}}}
    ;;{{{  set up terminal codes and global keys

    (mapc #'load-library-if-available '("console" "screen"))
    (when (eq window-system 'x) (load-library-if-available "x"))

    (loop for  key in
          '(
            ([f3] bury-buffer)
            ([f4] emacspeak-kill-buffer-quietly)
            ;("\M-s" save-buffer)
            ( [f8]emacspeak-remote-quick-connect-to-server)
            ([f11]shell)
            ([f12]vm)
            (  "\C-x%"comment-region)
            ( "\M-r"replace-string)
            ( "\M-e"end-of-word)
            ( "\M-\C-j"imenu)
            ("\M--" undo-only)
            ( "\M-\C-c"calendar))
          do
          (global-set-key (first key) (second key)))

    ;;}}}
    ;;{{{  initial stuff

    (require 'dired-x)
    (require 'dired-aux)

    (put 'upcase-region 'disabled nil)
    (put 'downcase-region 'disabled nil)
    (put 'narrow-to-region 'disabled nil)
    (put 'eval-expression 'disabled nil)

    (dynamic-completion-mode)
    ;;}}}
    ;;{{{  different mode settings

;;; Mode hooks.

    (eval-after-load "shell"
      '(progn
         (define-key shell-mode-map "\C-cr" 'comint-redirect-send-command)
         (define-key shell-mode-map "\C-ch" 'emacspeak-wizards-refresh-shell-history)))

    ;;}}}
    ;;{{{ Prepare needed libraries
    (package-initialize)
    (mapc
     #'load-library-if-available
     '(
       "my-functions" ;;; personal functions and advice
;;; Mail readers:
       "vm-prepare" "bbdb-prepare" "mspools-prepare"
       "gnus-prepare"
       "smtpmail" "gm-smtp" "sigbegone"
       "w3-prepare"
       ;"w3m-prepare"  ;;; Web Browsers:
       "nxml-prepare"
       "folding-prepare"
       "calc-prepare" 
       "tcl-prepare"   "slime-prepare"
                                        ; jde and ecb will pull in cedet.
                                        ;"jde-prepare" 
       "org-prepare"
        "emms-prepare"
       "erc-prepare" "jabber-prepare"
       "twittering-prepare"
       "fap-prepare"
       "local"
       ))

    ;;}}}
    ))                                  ; end defun
;;{{{  start it up
(add-hook
 #'after-init-hook
 #'(lambda ()
     (emacs-startup-configure-packages)
     (emacspeak-tts-startup-hook)
     (bbdb-insinuate-vm)
     (server-start)
     (shell)
     (calendar)
     (initialize-completions)
(shell-command "aplay ~/cues/highbells.au")
     (message "Successfully initialized Emacs")))

(start-up-my-emacs)
(when (file-exists-p custom-file) (load-file custom-file))
(setq warning-suppress-types nil)
;;}}}
(provide 'emacs-startup)
;;{{{  emacs local variables
;;;local variables:
;;;folded-file: t
;;;end:
;;}}}

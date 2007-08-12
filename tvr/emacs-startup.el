;;; Emacs initialization file for Raman.
;;; $Id$
;;; Segre March 22 1991
;;; July 15, 2001 finally cutting over to custom.
(require 'cl)
(declare  (optimize  (safety 0) (speed 3)))
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
  (unless (and library
               (locate-library library))
    (add-to-list 'load-path
                 (expand-file-name path
                                   (or whence
                                       (and (boundp
                                             'emacs-personal-library)
                                            emacs-personal-library)))
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
(setq outline-minor-mode-prefix "\C-l")
(declare (special custom-file))
(setq custom-file (expand-file-name "~/.customize-emacs"))

;;}}}
(defun start-up-my-emacs()
  "Start up emacs for me. "
  (interactive)
  (declare (special emacs-personal-library
                    emacs-private-library))
  (let ((gc-cons-threshold 8000000))
    (when (file-exists-p  emacs-private-library)
      (augment-load-path emacs-private-library ))
    (when (file-exists-p  emacs-personal-library)
      (augment-load-path emacs-personal-library))
    ;;{{{ Load and customize emacspeak 

    (unless (featurep 'emacspeak)
      (load-file (expand-file-name "~/emacs/lisp/emacspeak/lisp/emacspeak-setup.el")))
    (when (featurep 'emacspeak)
      (emacspeak-toggle-auditory-icons t)
      (when (emacspeak-sounds-theme-p "chimes-stereo/")
        (emacspeak-sounds-select-theme "chimes-stereo/"))
      (tts-configure-synthesis-setup)
      (dtk-set-rate tts-default-speech-rate 'global))

    ;;}}}
    ;;{{{  set up terminal codes and global keys

    (mapcar #'load-library-if-available
            '("console"
              "screen"))


;;{{{  global key definitions
    
(global-set-key [f5] 'bury-buffer)
(global-set-key '[delete] 'dtk-toggle-punctuation-mode)
(global-set-key  '[f8] 'emacspeak-remote-quick-connect-to-server)
(global-set-key '[f11] 'shell)
(global-set-key '[f12] 'vm)

    (global-set-key "\C-xc" 'compile)
    (global-set-key  "\C-x%" 'comment-region)
    (global-set-key "\M-r" 'replace-string)
    (global-set-key "\M-e" 'end-of-word)
    (global-set-key "\M-\C-j" 'imenu)
    ;;}}}

    ;;}}}
    ;;{{{  initial stuff

    (load-library-if-available "advice-setup")
    (load-library-if-available "my-functions")
    
    (put 'upcase-region 'disabled nil)
    (put 'downcase-region 'disabled nil)
    (put 'narrow-to-region 'disabled nil)
    (put 'eval-expression 'disabled nil)
    (add-hook 'find-file-hooks 'turn-on-auto-fill)

    ;;}}}
    ;;{{{  completion: tmc 

    (dynamic-completion-mode)
    (condition-case nil
        (initialize-completions)
      (error (message "Completions not started cleanly.")))

    ;;}}}
    ;;{{{  dired

    (require 'dired)
    (require 'dired-x)
    (require 'dired-aux)

    ;;}}}
    ;;{{{  w3:
    (mapcar #'load-library-if-available
            '("w3-prepare"
              "w3m-prepare"
              "wget-prepare"))
    
    ;;}}}
    ;;{{{  vm setup
    (mapcar 
     #'load-library-if-available
     '("vm-prepare"
       "bbdb-prepare"
       "smtpmail"
       "mailcrypt-prepare"
       "sigbegone"))

    (bbdb-insinuate-vm)
           
    (declaim (special vm-mail-mode-map))           
    (define-key vm-mail-mode-map  "\C-ce" 'expand-mail-aliases)
    (define-key vm-mail-mode-map  "\C-c\C-g" 'my-thanks-mail-signature)
    (define-key vm-mail-mode-map   "\M-\C-i" 'bbdb-complete-name)
                
    
    ;;}}}
    ;;{{{ python setup

    (load-library-if-available "python-mode-prepare")
    ;;}}}
    ;;{{{  different mode settings 

;;; Mode hooks.
    
    (declaim (special  completion-ignored-extensions))
    
    (push  ".class" completion-ignored-extensions)
    (eval-after-load "shell"
      '(progn
         (define-key shell-mode-map "\C-cr" 'comint-redirect-send-command)
         (define-key shell-mode-map "\C-ch" 'emacspeak-wizards-refresh-shell-history))) ;;; emacs lisp mode:
    (add-hook
     'emacs-lisp-mode-hook
     #'(lambda ()
         (define-key emacs-lisp-mode-map "\M-n" 'next-interactive-defun)))

    ;;}}}
;;{{{ aster

                                        ;(augment-load-path "~/aster-clisp/lisp-code/vanila-interface")
                                        ;(autoload 'aster "run-lisp-setup" "Run AsTeR" t)

    ;;}}}
    ;;{{{  auctex:

    (load-library-if-available "auctex-prepare")
    
    ;;}}}
    ;;{{{  folding mode 

    (load-library-if-available "folding-prepare")

    ;;}}}
    ;;{{{  emacs server: 

    
    (server-start)

    ;;}}}
    ;;{{{  calculator: 

    (load-library-if-available "calc-prepare")
    (load-library-if-available "ess-prepare")
    ;;}}}
    ;;{{{  tcl mode: 

    (load-library-if-available "tcl-prepare")

    ;;}}}
    ;;{{{  view processes

    (load-library-if-available "view-ps-prepare")

    ;;}}}
    ;;{{{ file at point 

    (require 'ffap)
    (declare (special  ffap-bindings))
    (setq ffap-bindings 
          '(
            (global-set-key  "\M-M" 'ffap-menu)
            (global-set-key "\M-L" 'ffap-next)))

    
    (ffap-bindings)

    ;;}}}
    ;;{{{ generic

    (require 'generic)
    (require 'generic-x)
    (load-library-if-available "moz-prepare")

    ;;}}}
    ;;{{{ jde

                                        ;(load-library-if-available "jde-prepare")
                                        ;(load-library-if-available "ecb-prepare")

    ;;}}}    
    ;;{{{ mail spools 
    (load-library-if-available "mspools-prepare")
    ;;}}}
    ;;{{{ dismal

    (load-library-if-available "dismal-prepare")

    ;;}}}
    ;;{{{ midnight 
    (require 'midnight)

    ;;}}}
    ;;{{{ CPerl if available

    (load-library-if-available "cperl-mode")
    

    ;;}}}
    ;;{{{  cvs 

    (load-library-if-available "pcl-prepare")

    ;;}}}
    ;;{{{  chat programs 

    (load-library-if-available "erc-prepare")
    (load-library-if-available "jabber-prepare")

    ;;}}}
    ;;{{{ browse-kill-ring
    (load-library-if-available "browse-kill-ring")
    ;;}}}
    ;;{{{  dictionary
    (load-library-if-available "dictionary-prepare")

    ;;}}}
    ;;{{{ fff

                                        ;(load-library-if-available "fff")
    (load-library-if-available "fff-rfc")
    (fff-install-map)
    (fff-rfc-install-map)

    ;;}}}
    ;;{{{ tramp

    (load-library-if-available "tramp-prepare")

    ;;}}}
    ;;{{{  calendar
    (require 'ediary)
    (require 'calendar)
    (global-set-key "\M-\C-c" 'calendar)
    ;;}}}
    ;;{{{ dirvars

    ;;(load-library-if-available "dirvars")

    ;;}}}
    ;;{{{ color

    (global-font-lock-mode t)
    (load-library-if-available "color-theme")
    (color-theme-emacs-21)

    ;;}}}
    ;;{{{ nxml

    (load-library-if-available "nxml-prepare")
                                        ;(load-library-if-available "xae-prepare")
    (load-library-if-available "tdtd-prepare")
                                        
    (load-library-if-available "xslt-process-prepare")
    ;;}}}
    ;;{{{ crontab
    (load-library-if-available "crontab-mode")
    ;;}}}
    ;;{{{ local
    (load-library-if-available "local")

    ;;}}}
    ))                                  ; end defun 
;;{{{  start it up 
(add-hook 'after-init-hook
          #'(lambda ()
              (dtk-set-rate 100 'global)
              (shell)
              (calendar)
              (when (file-exists-p custom-file)
  (load-file custom-file))
              (message "Successfully initialized Emacs")
              (shell-command "aplay ~/cues/highbells.au")))
(start-up-my-emacs)



;;}}}
(provide 'emacs-startup)
;;{{{  emacs local variables

;;;local variables:
;;;folded-file: t
;;;end: 

;;}}}

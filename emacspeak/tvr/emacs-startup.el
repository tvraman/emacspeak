;;; Emacs initialization file for Raman.
;;; $Id$
;;; Segre March 22 1991
;;; July 15, 2001 finally cutting over to custom.
(require 'cl)
(declare  (optimize  (safety 0) (speed 3)))
;;{{{ personal lib

(defvar emacs-private-library-directory 
  (expand-file-name "~/.elisp")
  "Private personalization directory. ")

(defvar emacs-personal-library-directory
  (expand-file-name "~/emacs/lisp/site-lisp")
  "Directory where we keep personal libraries")

;;}}}
;;{{{ helper functions:

(defsubst augment-load-path (path &optional library whence at-end)
  "add directory to load path.
Path is resolved relative to `whence' which defaults to emacs-personal-library-directory."
  (interactive "Denter directory name: ")
  (declare (special emacs-personal-library-directory))
  (unless (and library
               (locate-library library))
    (add-to-list 'load-path
                 (expand-file-name path
                                   (or whence
                                       (and (boundp
                                             'emacs-personal-library-directory)
                                            emacs-personal-library-directory)))
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
(when (file-exists-p custom-file)
  (load-file custom-file))

;;}}}
(defun start-up-my-emacs()
  "Start up emacs for me. "
  (interactive)
  (declare (special emacs-personal-library-directory
                    emacs-private-library-directory))
  (let ((gc-cons-threshold 8000000)
        (message-log-max 1024))
    (when (file-exists-p  emacs-private-library-directory)
      (augment-load-path emacs-private-library-directory ))
    (when (file-exists-p  emacs-personal-library-directory)
      (augment-load-path emacs-personal-library-directory))
    ;;{{{ Load and customize emacspeak 

    (unless (featurep 'emacspeak)
      (load-file (expand-file-name "~/emacs/lisp/emacspeak/lisp/emacspeak-setup.el")))
    (when (featurep 'emacspeak)
      (emacspeak-toggle-auditory-icons t)
                                        ;(when (emacspeak-sounds-theme-p "chimes-mono/")
                                        ;(emacspeak-sounds-select-theme "chimes-mono/"))
      (tts-configure-synthesis-setup)
      (dtk-set-rate tts-default-speech-rate 'global))

    ;;}}}
    ;;{{{  handle terminal weirdnesses and function keys

    (load-library-if-available "console")
    (load-library-if-available "screen")
    (load-library-if-available "function-keys")

    ;;}}}
    ;;{{{  initial stuff

    (load-library-if-available "my-functions")
    
    (put 'upcase-region 'disabled nil)
    (put 'downcase-region 'disabled nil)
    (put 'narrow-to-region 'disabled nil)
    (put 'eval-expression 'disabled nil)
    (add-hook 'find-file-hooks 'turn-on-auto-fill)

    ;;}}}
    ;;{{{  advice:

    (load-library-if-available "advice-setup")

    ;;}}}
    ;;{{{  completion: tmc 

    (require 'completion)
    (dynamic-completion-mode)
    (condition-case nil
        (initialize-completions)
      (error (message "Completions not started cleanly.")))
    (global-set-key "\M- " 'complete)

    ;;}}}
    ;;{{{  gnus: 

    ;;; gnus now customized through custom.
    

    ;;}}}
    ;;{{{  dired

    (require 'dired)
    (require 'dired-x)
    (require 'dired-aux)

    ;;}}}
    ;;{{{  ange ftp:

    (require 'ange-ftp)

    ;;}}}
    ;;{{{  w3:

    (load-library-if-available "w3-prepare")
    (load-library-if-available "w3m-prepare")
    (load-library-if-available "wget-prepare")
    ;;}}}
    ;;{{{  vm setup

    (load-library-if-available "vm-prepare")
    (load-library-if-available "bbdb-prepare")
    (bbdb-insinuate-vm)
    
    (require 'supercite)
    (declare (special sc-mode-map-prefix))
    (setq sc-mode-map-prefix "\C-c\C-o")
    
    (loop for hook in
          (list 'vm-mail-mode-hook 'mail-mode-hook)
          do
          (add-hook
           hook
           #' (lambda ()
                (local-set-key "\C-ce" 'expand-mail-aliases)
                (local-set-key "\C-c\C-g"
                               'my-thanks-mail-signature)
                (local-set-key  "\M-\C-i" 'bbdb-complete-name)
                (auto-fill-mode 1))))
          
    (load-library-if-available "mailcrypt-prepare")
    (load-library-if-available "sigbegone")

    ;;}}}
    ;;{{{ python setup

    (autoload 'python-mode "python-mode" "Python editing mode." t)
    (augment-auto-mode-alist "\\.py$"  'python-mode) 
    (setq interpreter-mode-alist
          (cons '("python" . python-mode) interpreter-mode-alist))

    ;;}}}
    ;;{{{  different mode settings 

;;; Mode hooks.
    
    (declaim (special  completion-ignored-extensions))
    (add-hook 'comint-mode-hook 'emacspeak-toggle-comint-autospeak)
    (push  ".class" completion-ignored-extensions)

    ;;; emacs lisp mode:
    (add-hook 'emacs-lisp-mode-hook
              (function (lambda ()
                          (define-key emacs-lisp-mode-map
                            "\M-n" 'next-interactive-defun))))

    ;;}}}
    ;;{{{ aster

                                        ;(augment-load-path "~/aster-clisp/lisp-code/vanila-interface")
                                        ;(autoload 'aster "run-lisp-setup" "Run AsTeR" t)

    ;;}}}
    ;;{{{  global key definitions 
    (global-set-key "\C-xc" 'compile)

    (global-set-key  "\C-x%" 'comment-region)
    (global-set-key "\M-r" 'replace-string)
    (global-set-key "\M-g" 'goto-line)
    (global-set-key "\M-s" 'save-buffer) ;; use in screen 
    (global-set-key "\M--" 'undo)
    (global-set-key "\M-e" 'end-of-word)
    (global-set-key "\C-xra" 'append-to-register)
    (global-set-key "\C-xrp" 'prepend-to-register)

    ;;}}}
    ;;{{{  auctex:

    (load-library-if-available "auctex-prepare")
    (declare (special TeX-auto-private
                      TeX-macro-private
                      TeX-auto-local))
    
    ;; Personal defaults for AUC-TeX mode
;;; variable settings:
    (declare (special 
              tex-mode-hook))
                                        ; find overful underful boxes in debugger. 

    (setq tex-mode-hook
          #'(lambda()
              (local-set-key "\M-s" 'save-buffer)
              (local-set-key "\C-c," 'comma-at-end-of-word)
              (local-set-key "\C-c~" 'tex-tie-current-word)
              (auto-fill-mode 1)))

    ;;}}}
    ;;{{{  folding mode 

    (load-library-if-available "folding-prepare")

    ;;}}}
    ;;{{{  handle compressed files: 

;;; load compress package.
                                        
    (load "jka-compr")
    (auto-compression-mode t)

    ;;}}}
    ;;{{{  emacs server: 

    
    (server-start)

    ;;}}}
    ;;{{{  calculator: 

    (load-library-if-available "calc-prepare")

    ;;}}}
    ;;{{{  tcl mode: 

    (load-library-if-available "tcl-prepare")

    ;;}}}
    ;;{{{  dmacros: dynamic macros

    ;(load-library-if-available "dmacro-prepare")

    ;;}}}
    ;;{{{  view processes

    (load-library-if-available "view-ps-prepare")

    ;;}}}
    ;;{{{  imenu setup

    (global-set-key "\M-\C-j" 'imenu)

    ;;}}}
    ;;{{{ file at point 

    (require 'ffap)
    (declare (special  ffap-bindings))
    (setq ffap-bindings 
          '(
            (global-set-key  "\M-M" 'ffap-menu)
            (global-set-key "\M-L" 'ffap-next)
            ;(global-set-key "\C-x\C-f" 'find-file-at-point)
            ;;(global-set-key "\C-x4f"   'ffap-other-window)
            ;(global-set-key "\C-x5f"   'ffap-other-frame)
))

    
    (ffap-bindings)

    ;;}}}
    ;;{{{ generic

    (require 'generic)
    (require 'generic-x)

    ;;}}}
    ;;{{{ jde
    
    
    (load-library-if-available "jde-prepare")
    (load-library-if-available "ecb-prepare")
    (load-library-if-available "pmd-emacs-prepare")
    ;;}}}    
;;{{{ bind  ido 
(global-set-key '[insert] 'ido-switch-buffer)
;;}}}
    ;;{{{  load yasb 

    (load-library-if-available "yasb-prepare")
    ;;}}}
    ;;{{{ buffer selection 
    ;(load-library-if-available"buff-sel")
    ;;}}}
    ;;{{{ mail spools 

    (load-library-if-available"mspools")
    (and (featurep 'vm)
         (declare (special vm-mode-map))
         (define-key vm-mode-map "o" 'mspools-show))
    (load-library-if-available "mspools-prepare")
    ;;}}}
    ;;{{{ dismal
    (load-library-if-available "dismal-prepare")
    ;;}}}
    ;;{{{ sql
                                        ;(load-library-if-available "sql-prepare")
    ;;}}}
    ;;{{{ midnight 

    (when (locate-library "midnight")
      (require 'midnight))

    ;;}}}
    ;;{{{ CPerl if available

    (load-library-if-available "cperl-mode")
    (defalias 'perl-mode 'cperl-mode)

    ;;}}}
    ;;{{{ mp3

    (load-library-if-available "mpg123")
    (emacspeak-aumix-reset)

    ;;}}}
    ;;{{{  eudc 

    (load-library-if-available "eudc-prepare")

    ;;}}}
    ;;{{{  sawfish

    (load-library-if-available "sawfish-prepare")

    ;;}}}
    ;;{{{  cvs 

    (load-library-if-available "pcl-prepare")

    ;;}}}
    ;;{{{  eshell

    (load-library-if-available "eshell-prepare")

    ;;}}}
    ;;{{{  chat programs 
    (load-library-if-available "sametime-prepare")
    (load-library-if-available "erc-prepare")
    (load-library-if-available "tnt-prepare")

    ;;}}}
    ;;{{{ kbd macros 

    (load-library-if-available "my-macros")

    ;;}}}
    ;;{{{ browse-kill-ring
    (load-library-if-available "browse-kill-ring")
    (global-set-key "\M-k" 'browse-kill-ring)

    ;;}}}
    ;;{{{ analog
    (load-library-if-available "analog-prepare")
    ;;}}}
    ;;{{{  dictionary
    (load-library-if-available "dictionary-prepare")

    ;;}}}
    ;;{{{ fff

    (load-library-if-available "fff")
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
(load-library-if-available "icalendar")
    ;;}}}
    ;;{{{ dirvars
    (load-library-if-available "dirvars")
    ;;}}}
    ;;{{{ sys-apropos
    (load-library-if-available  "sys-apropos")

    ;;}}}
    ;;{{{ color

    (global-font-lock-mode t)
    (load-library-if-available "color-theme")
    (color-theme-blue-sea)

    ;;}}}
    ;;{{{ newsticker
    (load-library-if-available "newsticker")
    ;;}}}
    ;;{{{ kmacro

    (load-library-if-available "kmacro")

    ;;}}}
    ;;{{{ bib-find

    (load-library-if-available "bibfind")

    ;;}}}
    ;;{{{ igrep

(autoload 'igrep "igrep" "Advanced grep")

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
    ))                                  ; end defun 
;;{{{  start it up 

;;; So actually start it up:
(start-up-my-emacs)
(add-hook 'after-init-hook
          #'(lambda ()
              (declare (special tts-default-speech-rate))
             (emacspeak-aumix-reset)
             (shell)
             (dtk-set-rate tts-default-speech-rate 'global)
             (calendar)
             (message "Successfully initialized Emacs")
             (shell-command "play ~/cues/highbells.au")))

;;}}}
(provide 'emacs-startup)
;;{{{  emacs local variables

;;;local variables:
;;;folded-file: t
;;;end: 

;;}}}

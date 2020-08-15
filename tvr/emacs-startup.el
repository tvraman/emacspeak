;;; Emacs initialization file for Raman:  -*- lexical-binding: t; -*-
;;{{{ History:

;;; Segre March 22 1991
;;; July 15, 2001 finally cutting over to custom.
;;; August 12, 2007: Cleaned up for Emacs 22
;;; September 2017: Optimized and Cleaned Up

;;}}}
;;{{{  libs, vars:

(require 'cl-lib)
(cl-declaim (optimize (safety 0) (speed 3)))
(cl-declaim (special outline-minor-mode-prefix ad-redefinition-action))
(setq ad-redefinition-action 'accept)
(setq outline-minor-mode-prefix (kbd "C-o"))
(defvar emacspeak-speak-messages)
(defvar emacs-personal-library
  (expand-file-name "~/emacs/lisp/site-lisp")
  "Site libs.")

(when (file-exists-p emacs-personal-library)
  (cl-pushnew emacs-personal-library load-path))

(defvar tvr-libs
  '(
    "vm-prepare" "gnus-gmail-prepare" 
    "lispy-prepare" "sp-prepare"       ; "vdiff-prepare"; "iedit-prepare"
    "org-prepare" ;"use-emms"
    "calc-prepare"; "helm-prepare"
    "js-prepare"  "slime-prepare" "yasnippet-prepare"
    "python-mode-prepare"
    "jabber-prepare" "twittering-prepare"
    "auth-source-xoauth2" )
  "Libraries to load.")

;;}}}
;;{{{ Fix system-users:

(defadvice system-users (around fix pre act comp)
  "Just return user real name."
  (list user-real-login-name))

;;}}}
;;{{{ Macro: tvr-fastload:

(defmacro tvr-fastload (&rest body)
  "Execute body with  an environment condusive to fast-loading files."
  `(let ((emacspeak-use-auditory-icons nil)
         (emacspeak-speak-messages nil)
         (inhibit-message t)
         (file-name-handler-alist nil)
         (load-source-file-function nil)
         (inhibit-message t)
         (gc-cons-threshold 64000000))
     ,@body))

;;}}}
;;{{{ helper functions:

(defsubst tvr-time-it (what &optional start)
  "Time code."
  (or start (setq start (current-time)))
  (message "<%s %.4f %d gcs %.4f>"
           (if (stringp what)
               what
             (format "%s" what))
           (float-time (time-subtract (current-time) start))
           gcs-done gc-elapsed))

(defun load-library-if-available (lib)
  "Safe load lib."
  (let ((start (current-time)))
    (tvr-fastload
     (condition-case err
         (progn
           (load-library lib)
           (tvr-time-it lib start))
       (error (message "Error loading %s: %s" lib (error-message-string err)))))))

;;}}}
;;{{{Weekday Colors:

(defconst tvr-weekday-to-color-alist
  '(("light sky blue" "#6FBD87")        ; silver tree
    ("#FFBCC9" "#FFD724")               ;gold on pink
    ("#F4C430" "sea green")             ; saffron
    ("#FFFFDA" "royal blue")
    ("mint cream" "royal blue")
    ("PowderBlue" "gold")
    ("#FFF3FF" "gold"))                 ; lavender blush
  "Alist of color pairs for days of the week")

(defun bw ()
  "set foreground to black"
  (set-foreground-color "black"))

(defun tvr-set-color-for-today ()
  "Return color pair for today."
  (interactive)
  (cl-declare (special tvr-weekday-to-color-alist))
  (let ((pair
         (elt tvr-weekday-to-color-alist (read (format-time-string "%w")))))
    (set-background-color (cl-first pair))
    (set-foreground-color (cl-second pair)))
  (call-interactively #'emacspeak-wizards-color-diff-at-point))

;;}}}
;;{{{ tvr-shell-bind-keys:

(defsubst tvr-shell-bind-keys ()
  "Set up  shell mode keys."
  (cl-declare (special shell-mode-map))
  (cl-loop
   for b in
   '(
     ("SPC" comint-magic-space)
     ("C-c k" comint-clear-buffer))
   do
   (define-key shell-mode-map (kbd (cl-first b)) (cl-second b))))

;;}}}
;;{{{ Handlers: Custom, after-init-hook

(defun tvr-customize ()
  "Load my customizations."
  (cl-declare (special custom-file))
  ;;; basic look and feel
  (setq inhibit-startup-echo-area-message user-login-name
        initial-scratch-message ""
        initial-buffer-choice t)
  (tooltip-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode 0)
  (put 'list-timers 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'eval-expression 'disabled nil)
  (put 'timer-list 'disabled nil)
  (tvr-fastload
   (setq-default custom-file (expand-file-name "~/.customize-emacs"))
   (define-key esc-map "\M-:" 'emacspeak-wizards-show-eval-result)
   (global-set-key (kbd "C-RET") 'hippie-expand)
   (global-set-key (kbd "M-/") 'hippie-expand)
   (tvr-set-color-for-today)
   (when (file-exists-p custom-file) (load custom-file))))

(defun tvr-defer-muggles ()
  "Defered muggles loader."
  (unless (featurep 'emacspeak-muggles)
    (make-thread
     #'(lambda ()
         (tvr-fastload (load "emacspeak-muggles"))))))

(defun tvr-after-init ()
  "Actions to take after Emacs is up and ready."
  (cl-declare (special emacspeak-sounds-directory tvr-libs))
  (tvr-fastload
   (let ((after-start (current-time)))
     (dynamic-completion-mode 1)
     (jka-compr-install)
     (completion-initialize)
     (mapc
      (if (getenv "TVR_TIME_EMS")
          #'load-library-if-available
        #'load)
      tvr-libs)
     (with-eval-after-load "dired" (require 'dired-x))
     (run-with-idle-timer 0.1 nil #'tvr-defer-muggles)
     (tvr-customize)
     (soundscape-toggle)
     (setq frame-title-format '(multiple-frames "%b" ("Emacs")))
;;; prescient and company:
     (when (locate-library "prescient")
       (load-library "prescient")
       (prescient-persist-mode 1)
       (company-prescient-mode 1))

     (require 'emacspeak-dbus)
     (when (dbus-list-known-names :session)
       (nm-enable)
       (emacspeak-dbus-sleep-enable)
       (emacspeak-dbus-udisks-enable)
       (emacspeak-dbus-upower-enable)
       (emacspeak-dbus-watch-screen-lock))
     (emacspeak-wizards-project-shells-initialize)
     (start-process
      "play" nil "aplay"
      (expand-file-name "highbells.au" emacspeak-sounds-directory))
     (tvr-time-it "after-init" after-start)
     (desktop-read))))

(add-hook 'after-init-hook #'tvr-after-init)
(add-hook
 'emacs-startup-hook
 #'(lambda ()
     (delete-other-windows)
     (message
      "<Successfully initialized Emacs for %s in %.2f  seconds with %s
gcs (%.2f seconds)>"
      user-login-name (read (emacs-init-time)) gcs-done gc-elapsed)))

(defun tvr-text-mode-hook ()
  "TVR:text-mode"
  (auto-correct-mode 1)
  (auto-fill-mode)
  (abbrev-mode 1)
  (unless (eq major-mode 'org-mode) (orgalist-mode 1)))

(defun tvr-prog-mode-hook ()
  "TVR:prog-mode"
  (local-set-key "\C-m" 'newline-and-indent)
  (company-mode 1)
  (hs-minor-mode 1)
  (auto-fill-mode)
  (cond
   ((memq major-mode '(emacs-lisp-mode lisp-mode lisp-interaction-mode))
    (lispy-mode 1))
   (t (smartparens-mode 1)))
  (yas-minor-mode 1)
  (abbrev-mode 1))

;;}}}
;;{{{tvr-emacs:

(defun tvr-emacs ()
  "Start up emacs."
  (cl-declare (special emacspeak-directory
                       outloud-default-speech-rate dectalk-default-speech-rate
                       magit-mode-map outline-mode-prefix-map))
  (tvr-fastload
   (setq outloud-default-speech-rate 125 ; because we load custom at the end
         dectalk-default-speech-rate 485)
   (load (expand-file-name "~/emacs/lisp/emacspeak/lisp/emacspeak-setup.elc"))
   (when (file-exists-p (expand-file-name "tvr" emacspeak-directory))
     (push (expand-file-name "tvr/" emacspeak-directory) load-path))

   (prefer-coding-system 'utf-8-emacs)
   (cl-loop
    for key in
    '(
      ("<f3>" bury-buffer)
      ("<f4>" emacspeak-kill-buffer-quietly)
      ("M--" undo-only)
      ("<f11>" shell)
      ("<f12>" vm)
      ("M-r" replace-string)
      ("M-e" emacspeak-wizards-end-of-word)
      ("M-C-j" imenu)
      ("M-C-c" calendar)
      ("C-RET" hippie-expand))
    do
    (global-set-key (kbd (cl-first key)) (cl-second key)))

   (cl-loop                             ; shell wizard
    for i from 0 to 9 do
    (global-set-key (kbd (format "C-c %s" i)) 'emacspeak-wizards-shell-by-key))
   (global-set-key (kbd "C-c <tab>") 'hs-toggle-hiding)
;;; Smarten up ctl-x-map
   (define-key ctl-x-map "c" 'compile)
   (define-key ctl-x-map "\C-d" 'dired-jump)
   (define-key ctl-x-map "\C-n" 'forward-page)
   (define-key ctl-x-map "\C-p" 'backward-page)

;;; Shell mode bindings:
   (eval-after-load "shell" `(progn (tvr-shell-bind-keys)))

;;; Outline Setup:

   (eval-after-load 'outline
     `(progn
;;;restore what we are about to steal
        (define-key outline-mode-prefix-map "o" 'open-line)
        (global-set-key "\C-o" outline-mode-prefix-map)))

;;; turn on modes:

   (add-hook 'prog-mode-hook 'tvr-prog-mode-hook)
   (add-hook 'text-mode-hook 'tvr-text-mode-hook)
   (savehist-mode)
   (save-place-mode)
   (midnight-mode)
   (server-start)
;;; Magit and Forge:

   (with-eval-after-load 'magit (require 'forge))
   )) ;end defun tvr-emacs

;;}}}
(tvr-emacs)
;;{{{ Forward Function Declarations:

(declare-function emacspeak-wizards-color-diff-at-point "emacspeak-wizards" (&optional set))
(declare-function completion-initialize "completion" nil)
(declare-function soundscape-toggle "soundscape" nil)
(declare-function dbus-list-known-names "dbus" (bus))
(declare-function nm-enable "nm" nil)
(declare-function emacspeak-dbus-sleep-enable "emacspeak-dbus" nil)
(declare-function emacspeak-dbus-watch-screen-lock "emacspeak-dbus" nil)
(declare-function emacspeak-dbus-udisks-enable "emacspeak-dbus" nil)
(declare-function emacspeak-dbus-upower-enable "emacspeak-dbus" nil)
(declare-function emacspeak-wizards-project-shells-initialize "emacspeak-wizards" nil)
(declare-function auto-correct-mode "auto-correct" (&optional arg))
(declare-function company-mode "company" (&optional arg))
(declare-function smartparens-mode "smartparens" (&optional arg))
(declare-function  company-prescient-mode "emacs-startup" t)
(declare-function prescient-persist-mode "prescient" (&optional arg))
(declare-function yas-minor-mode "yasnippet" (&optional arg))

;;}}}
(provide 'emacs-startup)
;;{{{  emacs local variables

;;;local variables:
;;;folded-file: t
;;;end:

;;}}}

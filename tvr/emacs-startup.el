;;; Emacs initialization file for Raman:  -*- lexical-binding: t; -*-
;;{{{ History:

;;; Segre March 22 1991
;;; July 15, 2001 finally cutting over to custom.
;;; August 12, 2007: Cleaned up for Emacs 22
;;; September 2017: Optimized and Cleaned Up
;;; August 2020: Limit code at top-level.

;;}}}
;;{{{  libs, vars:

(require 'cl-lib)
(cl-declaim (optimize (safety 0) (speed 3)))

(defvar emacspeak-speak-messages)
(defvar emacs-personal-library
  (expand-file-name "~/emacs/lisp/site-lisp")
  "Site libs.")

(when (file-exists-p emacs-personal-library)
  (push emacs-personal-library load-path))

(defvar tvr-libs
  '(
    "vm-prepare" "gnus-gmail-prepare" "jabber-prepare"
    "lispy-prepare" "sp-prepare" "slime-prepare" "org-prepare")
  "Libraries that need extra setup.")

;;}}}
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

;;}}}
;;{{{ Macro: tvr-fastload:

(defmacro tvr-fastload (&rest body)
  "Execute body with  an environment condusive to fast-loading files."
  `(let ((emacspeak-speak-messages nil)
         (file-name-handler-alist nil)
         (load-source-file-function nil)
         (inhibit-message t)
         (gc-cons-threshold 128000000))
     ,@body))

;;}}}
;;{{{ Fixups:

(defadvice system-users (around fix pre act comp)
  "Just return user real name."
  (ignore ad--addoit-function)
  (setq ad-return-value (list user-real-login-name)))

;;; for twittering-mode:
(defalias 'epa--decode-coding-string 'decode-coding-string)

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
  "Set color pair for today."
  (interactive)
  (cl-declare (special tvr-weekday-to-color-alist))
  (let ((pair
         (elt tvr-weekday-to-color-alist (read (format-time-string "%w")))))
    (set-background-color (cl-first pair))
    (set-foreground-color (cl-second pair))))

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
;;{{{Functions: emacs-startup-hook, after-init-hook, tvr-customize
(defun tvr-emacs-startup-hook ()
  "Emacs startup hook."
  (cl-declare (special emacspeak-sounds-directory))
  (delete-other-windows)
  (setq gc-cons-threshold 64000000)
  (start-process
   "play" nil "aplay"
   (expand-file-name "highbells.au" emacspeak-sounds-directory))
  (message
   "<Emacs started for %s in %.2f  seconds with %s gcs (%.2f seconds)>"
   user-login-name (read (emacs-init-time)) gcs-done gc-elapsed))

(defun tvr-customize ()
  "Customize my emacs.
Use Custom to customize where possible. "
  (cl-declare (special
               custom-file outline-mode-prefix-map outline-minor-mode-prefix))
  (setq outline-minor-mode-prefix (kbd "C-o"))
;;; basic look and feel

  (put 'list-timers 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'eval-expression 'disabled nil)
  (put 'timer-list 'disabled nil)
  (setq-default custom-file (expand-file-name "~/.customize-emacs"))
  (prefer-coding-system 'utf-8-emacs)
  (cl-loop ;;; global key-bindings
   for key in
   '(
     ( "M-#" calc-dispatch)
     ("<f3>" bury-buffer)
     ("<f4>" emacspeak-kill-buffer-quietly)
     ("M--" undo-only)
     ("<f11>" shell)
     ("<f12>" vm)
     ("M-r" replace-string)
     ("M-e" emacspeak-wizards-end-of-word)
     ("M-C-j" imenu)
     ("M-C-c" calendar)
     ("C-c <tab>"  hs-toggle-hiding)
     ("M-/" 'hippie-expand)
     ("C-RET" hippie-expand))
   do
   (global-set-key (kbd (cl-first key)) (cl-second key)))

  (cl-loop ;;; shell wizard
   for i from 0 to 9 do
   (global-set-key (kbd (format "C-c %s" i)) 'emacspeak-wizards-shell-by-key))

;;; Smarten up ctl-x-map
  (define-key ctl-x-map "c" 'compile)
  (define-key ctl-x-map "\C-d" 'dired-jump)
  (define-key ctl-x-map "\C-n" 'forward-page)
  (define-key ctl-x-map "\C-p" 'backward-page)

;;; Shell mode bindings:
  (with-eval-after-load 'shell  (tvr-shell-bind-keys))

;;; Outline Setup:

  (with-eval-after-load 'outline
    (progn ;;;restore what we are about to steal
      (define-key outline-mode-prefix-map "o" 'open-line)
      (global-set-key "\C-o" outline-mode-prefix-map)))
  (server-start)
  (with-eval-after-load 'magit (require 'forge))
  (define-key esc-map "\M-:" 'emacspeak-wizards-show-eval-result)

  (tvr-set-color-for-today)
  (dynamic-completion-mode 1)
  (completion-initialize)
  (jka-compr-install)
  (when (file-exists-p custom-file) (tvr-fastload (load custom-file))))

(defun tvr-defer-muggles ()
  "Defered muggles loader."
  (unless (featurep 'emacspeak-muggles)
    (make-thread #'(lambda () (tvr-fastload (load "emacspeak-muggles"))))))

(defun tvr-after-init ()
  "Actions to take after Emacs is up and ready."
;;; load  library-specific settings, customize, then start things.
  (cl-declare (special emacspeak-sounds-directory tvr-libs))
  (tvr-fastload
   (let ((after-start (current-time))) ;;; to time after-init at the end
     (mapc
      (if (getenv "TVR_TIME_EMS") #'load-library-if-available #'load)
      tvr-libs) ;;; loaded  settings   not  customizable via custom.
     (with-eval-after-load "dired" (require 'dired-x))
     (run-with-idle-timer 0.5 nil #'tvr-defer-muggles)
     (tvr-customize) ;;; customizations
     (soundscape-toggle)
     (when (dbus-list-known-names :session)
       (require 'emacspeak-dbus)
       (nm-enable)
       (emacspeak-dbus-sleep-enable)
       (emacspeak-dbus-udisks-enable)
       (emacspeak-dbus-upower-enable)
       (emacspeak-dbus-watch-screen-lock))
     (make-thread  #'emacspeak-wizards-project-shells-initialize)
     (tvr-time-it "after-init" after-start)
     (make-thread #' (lambda nil (tvr-fastload (desktop-read)))))))

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
  "Start up emacs.
This function loads Emacspeak.
Emacs customization and library configuration happens via the after-init-hook. "
  (cl-declare (special emacspeak-directory
                       outloud-default-speech-rate dectalk-default-speech-rate))
  (setq outloud-default-speech-rate 125 ; because we load custom at the end
        dectalk-default-speech-rate 485)
  (tvr-fastload ;;; load emacspeak:
   (load (expand-file-name "~/emacs/lisp/emacspeak/lisp/emacspeak-setup.elc"))
   (when (file-exists-p (expand-file-name "tvr" emacspeak-directory))
     (push (expand-file-name "tvr/" emacspeak-directory) load-path)))
  (add-hook 'after-init-hook #'tvr-after-init)
  (add-hook 'emacs-startup-hook #'tvr-emacs-startup-hook)) ;end defun tvr-emacs

;;}}}
(tvr-emacs)
(provide 'emacs-startup)
;;{{{  emacs local variables

;;;local variables:
;;;folded-file: t
;;;end:

;;}}}

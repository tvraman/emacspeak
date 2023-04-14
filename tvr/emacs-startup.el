;;; Emacs initialization file for Raman:  -*- lexical-binding: t; -*-
;;{{{ History:

;; Segre March 22 1991
;; July 15, 2001 finally cutting over to custom.
;; August 12, 2007: Cleaned up for Emacs 22

;; September 2017: Optimized and Cleaned Up
;; August 2020: Limit code at top-level.

;;}}}
;;{{{ Introduction:

;;; Commentary:
;; This startup file is set up with the following goals:
;; 1. Speed up emacs startup
;; 2. Customize packages via a custom file as far as possible.
;; 3. Keep the  custom settings  in a separate file
;; Place host-specific non-customizable bits in default.el.
;; 3. Define package-specific settings not available via Custom in a
;;    package-specific <package>-prepare.el file.
;; 4. Install everything from elpa/melpa as far as possible. (vm is an
;;    exception at present) --- I have nearly 200 packages activated.
;; 5. The startup file contains functions with prefix  tvr-.
;; 6. The only top-level call is (tvr-emacs).
;; 7. Function tvr-emacs starts up Emacspeak, and sets up two hooks:
;;    - after-init-hook to do the bulk of the work.
;; Set env var PULSE_SINK to binaural for using bs2b under pulseaudio
;;    - emacs-startup-hook to set up  initial window configuration.
;; 8. Function tvr-after-init-hook on after-init-hook does the
;; following:
;; For efficiency, package-specific setup files are concatenated into
;; a single file all-prepare.el by  make.
;;    - Load package-specific prepare.el files.
;;    - Load the custom settings file.
;;    - Start up things like the emacs server.
;;    - Some of these tasks are done on a separate thread using make-thread.
;;   - The work of loading files etc., is done within macro tvr-time-load
;;   which sets up an efficient environment for loading files and
;; helps in profiling.

;;}}}
;;{{{  libs, vars:

(require 'cl-lib)
(cl-declaim (optimize (safety 0) (speed 3)))
;;; Emacs @HEAD is broken:
(defvar font-lock-reference-face 'font-lock-constant-face)
(defvar tvr-site-lib
  (expand-file-name "~/emacs/lisp/site-lisp")
  "Site libs.")

(when (file-exists-p tvr-site-lib)
  (push tvr-site-lib load-path)
  (push (expand-file-name "vm/lisp/" tvr-site-lib) load-path))

(defvar tvr-libs
  "all-prepare"
  "Libraries that need extra setup.")

;;}}}
;;{{{ Macro: tvr-time-load:

(defmacro tvr-time-load (&rest body)
  "Execute body with  an environment condusive to fast-loading files.
Produce timing information as the last step."
  (declare (indent 1) (debug t))
  `(let ((start (current-time))
         (file-name-handler-alist nil)
         (load-source-file-function nil)
         (inhibit-message t)
         (gc-cons-threshold 128000000)
         (gc-cons-percentage 0.7))
     ,@body
     (message "<%.4f %d gcs %.4f>"
              (float-time (time-subtract (current-time) start))
              gcs-done gc-elapsed)))

;;}}}
;;{{{ Fixups:

;; Put psession startup on a separate thread:

(defadvice psession--restore-objects-from-directory (around ems pre act comp)
  ad-do-it)
(defadvice psession--restore-some-buffers (around ems pre act comp)
  (make-thread ad-do-it))

(defadvice system-users (around fix pre act comp)
  "Just return user real name."
  (ignore ad--addoit-function)
  (setq ad-return-value (list user-real-login-name)))

;; for twittering-mode:
(defalias 'epa--decode-coding-string 'decode-coding-string)

;;}}}
;;{{{ tvr-shell-bind-keys:

(defsubst tvr-shell-bind-keys ()
  "Set up  shell mode keys."
  (cl-declare (special shell-mode-map))
  (cl-loop
   for b in
   '(
     ("C-c r" comint-redirect-send-command )
     ("SPC" comint-magic-space)
     ("C-c k" comint-clear-buffer))
   do
   (define-key shell-mode-map (ems-kbd (cl-first b)) (cl-second b))))

;;}}}
;;{{{tvr-tabs:

(defun tvr-tabs ()
  "Set up  tab-bar"
  (tab-bar-rename-tab "Home")
  (tab-bar-switch-to-tab "Books")
  (tab-bar-switch-to-tab "Home"))

;;}}}
;;{{{Node/NVM Setup:
(defun tvr-nvm-setup ()
  "Set up NVM/NPM."
  (with-eval-after-load "nvm"
    (let ((v (car (sort (mapcar #'car (nvm--installed-versions)) #'string>))))
      (nvm-use v)
      (executable-find "node"))))

(defvar tvr-npm-node
  (tvr-nvm-setup)
  "Find the right Node executable.")

;;}}}
;;{{{Functions: emacs-startup-hook, after-init-hook, tvr-customize

(defun tvr-emacs-startup-hook ()
  "Emacs startup hook.
Configure dbus and set up tabs.
Reset gc-cons-threshold to a smaller value  and play
startup sound."
  (cl-declare (special emacspeak-sounds-directory))
  (run-with-idle-timer 1 nil #'emacspeak-dbus-setup)
  (setq gc-cons-threshold 64000000)
  (start-process
   "play" nil "aplay"
   (expand-file-name "highbells.au" emacspeak-sounds-directory))
  (message
   "<Emacs started for %s in %.2f  seconds with %s gcs (%.2f seconds)>"
   user-login-name (read (emacs-init-time)) gcs-done gc-elapsed)
  (tvr-tabs))

(defun tvr-customize ()
  "Customize my emacs.
Use Custom to customize where possible. "
  (cl-declare (special custom-file
                       global-mode-string outline-minor-mode-prefix
                       python-mode-hook outline-mode-prefix-map
                       completion-auto-select emacspeak-directory))
  (load-library "aster")
  (add-hook 'python-mode-hook #'elpy-enable)
  (setq outline-minor-mode-prefix "\C-co")
  ;; basic look and feel
  (setq frame-title-format '(multiple-frames "%b" ("Emacs")))
  (mapc
   #'(lambda (f) (put f 'disabled nil))
   '(list-threads narrow-to-page list-timers upcase-region
     downcase-region  narrow-to-region eval-expression ))
  (prefer-coding-system 'utf-8-emacs)
  (global-set-key[remap dabbrev-expand] 'hippie-expand)
  (cl-loop ;; global key-bindings
   for key in
   '(
     (  "C-x r a"  append-to-register)
     ("C-x r p"  prepend-to-register)
     ("C-x v ." magit-commit-create)
     ("C-x <tab>"  previous-buffer)
     ("C-c <tab>"  next-buffer)
     ("<f3>" bury-buffer)
     ("<f4>" emacspeak-kill-buffer-quietly)
     ("M--" undo-only)
     ("M-/" hippie-expand)
     ("M-C-c" calendar)
     ("M-C-j" imenu)
     ("M-e" emacspeak-wizards-end-of-word)
     ("M-r" replace-string)
     ( "M-#" calc-dispatch)
     ("M-C-v" vm-visit-folder))
   do
   (global-set-key (ems-kbd (cl-first key)) (cl-second key)))
  (cl-loop ;;; shell wizard
   for i from 0 to 9 do
   (global-set-key
    (ems-kbd (format "C-c %s" i)) 'emacspeak-wizards-shell-by-key))
  ;; Smarten up ctl-x-map
  (define-key ctl-x-map "c" 'compile)
  (define-key ctl-x-map "j" 'pop-global-mark)
  (define-key ctl-x-map "u"  'undo-only)
  (define-key ctl-x-map (ems-kbd "C-u") 'undo-redo)
  (define-key ctl-x-map (ems-kbd "C-d") 'dired-jump)
  ;; Shell mode bindings:
  (with-eval-after-load 'shell  (tvr-shell-bind-keys))
  ;; Outline Setup:
  (with-eval-after-load 'outline
    (global-set-key (ems-kbd "C-o") outline-mode-prefix-map) ;;;restore
    (define-key outline-mode-prefix-map "o" 'open-line))
  (server-start)
  (with-eval-after-load 'magit (require 'forge))
  (make-thread #'(lambda nil (load "eww")))
  (require 'dired-x)
  (setq custom-file (expand-file-name "~/.customize-emacs"))
  (when (file-exists-p custom-file)
    (tvr-time-load (load custom-file)))
  (load-theme 'modus-vivendi-tinted t)

  (mapc
   #'(lambda (m)
       (diminish m ""))
   '(outline-minor-mode reftex-mode voice-lock-mode
     auto-fill-function abbrev-mode auto-correct-mode))

  (setq  global-mode-string '("" display-time-string battery-mode-line-string))
  (bash-completion-setup))

(defun tvr-after-init ()
  "Actions to take after Emacs is up and ready."
  ;; load  library-specific settings, customize, then start things.
  (cl-declare (special  tvr-libs ))
;;; load  settings   not  customizable via custom.
  (tvr-time-load (load tvr-libs))
  (tvr-customize) ;;; customizations
  (with-eval-after-load
    'yasnippet
    (yas-reload-all)
    (diminish 'yas-minor-mode ""))
  (load "emacspeak-muggles")
  (emacspeak-wizards-project-shells-initialize))

(declare-function
 emacspeak-pronounce-toggle-use-of-dictionaries
 "emacspeak-pronounce" (&optional state))

(defun tvr-text-mode-hook ()
  "TVR:text-mode"
  (cl-declare (special auto-correct-predicate))
  (auto-fill-mode)
  (emacspeak-pronounce-toggle-use-of-dictionaries t)
  (setq auto-correct-predicate #'(lambda () t))
  ;; company-wordfreq setup:
  (setq-local company-backends '(company-wordfreq))
  (setq-local company-transformers nil)
  (abbrev-mode)
  (unless (eq major-mode 'org-mode) (orgalist-mode)))

(defun tvr-prog-mode-hook ()
  "TVR:prog-mode"
  (cl-declare (special dtk-caps))
  (local-set-key "\C-m" 'newline-and-indent)
  (company-mode)
  (diminish 'company-mode "")
  (hs-minor-mode)
  (diminish 'hs-minor-mode "")
  (auto-fill-mode)
  (cond
    ((memq major-mode '(emacs-lisp-mode lisp-mode lisp-interaction-mode))
     (when dtk-caps
       (setq dtk-caps nil))
     (lispy-mode ))
    (t (smartparens-mode)))
  (yas-minor-mode)
  (abbrev-mode))

;;}}}
;;{{{tvr-emacs:

(defun tvr-emacs ()
  "Start up emacs.
This function loads Emacspeak.  Emacs customization and library
configuration happens via the after-init-hook. "
  (cl-declare (special emacspeak-directory))
  (setenv "PULSE_SINK" "binaural")
  (unless (featurep 'emacspeak)
    (tvr-time-load ; load emacspeak:
     (load ;; setenv EMACSPEAK_DIR if you want to load a different version
      (expand-file-name
       "lisp/emacspeak-setup"
       (or (getenv  "EMACSPEAK_DIR") "~/emacs/lisp/emacspeak")))))
  (cl-pushnew (expand-file-name "tvr/" emacspeak-directory) load-path
              :test #'string-equal)
  (push (expand-file-name "aster-math/ui" emacspeak-directory) load-path)
  (add-hook 'after-init-hook #'tvr-after-init)
  (add-hook 'emacs-startup-hook #'tvr-emacs-startup-hook))

;;}}}
(tvr-emacs)

;;{{{ Forward Function Declarations:
(declare-function nvm--installed-versions "emacs-startup" t)

(declare-function ems-kbd "emacspeak-keymap" (string))
(declare-function yas-reload-all "yasnippet" (&optional no-jit interactive))
(declare-function soundscape-toggle "soundscape" nil)
(declare-function emacspeak-dbus-setup "emacspeak-dbus" nil)
(declare-function
 emacspeak-wizards-project-shells-initialize
 "emacspeak-wizards" nil)

;;}}}
(provide 'emacs-startup)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}

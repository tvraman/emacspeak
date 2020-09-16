;;; Emacs initialization file for Raman:  -*- lexical-binding: t; -*-
;;{{{ History:

;;; Segre March 22 1991
;;; July 15, 2001 finally cutting over to custom.
;;; August 12, 2007: Cleaned up for Emacs 22
;;; September 2017: Optimized and Cleaned Up
;;; August 2020: Limit code at top-level.

;;}}}
;;{{{ Introduction:

;;; Commentary:
;;; This startup file is set up with the following goals:
;;; 1. Speed up emacs startup --- setting environment variable
;;;    "TVR_TIME_EMS" before starting emacs produces detailed timing
;;;    information in the *Messages* buffer.
;;; 2. Customize packages via a custom file as far as possible.
;;; 3. Keep the  custom settings  in a separate file, with a later goal of
;;;   turning that into a  theme.
;;; 4. After converting to a theme, Move machine-specific custom settings
;;;    into a separate host-specific custom file, thus making the
;;; earlier theme host-independent.  Place host-specific non-customizable bits in default.el.
;;; 4. Define package-specific settings not available via Custom in a
;;;    package-specific <package>-prepare.el file.
;;; 5. Install everything from elpa/melpa as far as possible. (vm is an
;;;    exception at present) --- I have nearly 200 packages activated.
;;; 6. The startup file is a collection of functions with entry-point tvr-emacs.
;;; 7. The only top-level call is (tvr-emacs).
;;; 8. Function tvr-emacs starts up Emacspeak, and sets up two hooks:
;;;    - after-init-hook to do the bulk of the work.
;;;    - emacs-startup-hook to set up  initial window configuration.
;;; 9. Function tvr-after-init-hook on after-init-hook does the
;;; following:
;;; For efficiency, package-specific setup files are concatenated into
;;; a single file all-prepare.el by  make.
;;;    - Load package-specific prepare.el files.
;;;    - Load the custom settings file.
;;;    - Start up things like the emacs server.
;;;    - Some of these tasks are done on a separate thread using make-thread.
;;;   - The work of loading files etc., is done within macro tvr-fastload
;;;   which sets up an efficient environment for loading files.

;;}}}
;;{{{  libs, vars:

(require 'cl-lib)
(cl-declaim (optimize (safety 0) (speed 3)))
(require 'dbus)
(defvar emacspeak-speak-messages)
(defvar emacs-personal-library
  (expand-file-name "~/emacs/lisp/site-lisp")
  "Site libs.")

(when (file-exists-p emacs-personal-library)
  (push emacs-personal-library load-path))
(push (expand-file-name "vm/lisp/" emacs-personal-library) load-path)

(defvar tvr-libs
  "all-prepare"
  "Libraries that need extra setup.")

;;}}}
;;{{{ Forward Function Declarations:
(declare-function yas-reload-all "yasnippet" (&optional no-jit interactive))
(declare-function soundscape-toggle "soundscape" nil)
(declare-function emacspeak-dbus-setup "emacspeak-dbus" nil)
(declare-function emacspeak-wizards-project-shells-initialize "emacspeak-wizards" nil)

;;}}}
;;{{{ Macro: tvr-fastload:

(defmacro tvr-fastload (&rest body)
  "Execute body with  an environment condusive to fast-loading files."
  (declare (indent 1) (debug t))
  `(let ((file-name-handler-alist nil)
         (load-source-file-function nil)
         (inhibit-message t)
         (gc-cons-threshold 128000000)
         (gc-cons-percentage 0.7))
     ,@body))

;;}}}
;;{{{ Fixups:

;;; Put psession startup on a separate thread:

(defadvice psession--restore-objects-from-directory (around ems pre act comp)
  (make-thread ad-do-it))
(defadvice psession--restore-some-buffers (around ems pre act comp)
  (make-thread ad-do-it))

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
              (load lib)
              (tvr-time-it lib start))
          (error (message "Error loading %s: %s" lib (error-message-string err)))))))

;;}}}
;;{{{ tvr-tabs:

(defun tvr-tabs ()
  "Set up my tab-bar."
  (tab-new)
  (tab-rename "Books")
  (tab-next)
  (tab-rename "Home"))

;;}}}
;;{{{Weekday Colors:

(defconst tvr-weekday-to-color-map
  [("light sky blue" . "#6FBD87")       ; silver tree
   ("#FFBCC9" . "#FFD724")              ;gold on pink
   ("#F4C430" . "sea green")            ; saffron
   ("#FFFFDA" . "royal blue")
   ("mint cream" . "royal blue")
   ("PowderBlue" . "gold")
   ("#FFF3FF" . "gold")]                ; lavender blush
  "Alist of color pairs for days of the week")

(defsubst tvr-set-color-for-today ()
  "Set color pair for today."
  (interactive)
  (cl-declare (special tvr-weekday-to-color-map))
  (let ((pair
         (aref tvr-weekday-to-color-map (read (format-time-string "%w")))))
    (set-background-color (car pair))
    (set-foreground-color (cdr pair))))

(defun bw ()
  "set foreground to black"
  (set-foreground-color "black"))

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
  "Emacs startup hook.
Reset gc-cons-threshold to a smaller value, time startup and play
startup sound."
  (cl-declare (special emacspeak-sounds-directory))
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
  (setq frame-title-format '(multiple-frames "%b" ("Emacs")))
  (mapc
   #'(lambda (f) (put f 'disabled nil))
   '(list-threads narrow-to-page list-timers upcase-region
                  downcase-region  narrow-to-region eval-expression ))
  (setq-default custom-file (expand-file-name "~/.customize-emacs"))
  (prefer-coding-system 'utf-8-emacs)
  (cl-loop ;;; global key-bindings
   for key in
   '(
     ("<f3>" bury-buffer)
     ("<f4>" emacspeak-kill-buffer-quietly)
     ("<f5>" find-file)
     ("C-c <tab>"  hs-toggle-hiding)
     ("M--" undo-only)
     ("M-/" hippie-expand)
     ("M-C-c" calendar)
     ("M-C-j" imenu)
     ("M-e" emacspeak-wizards-end-of-word)
     ("M-r" replace-string)
     ( "M-#" calc-dispatch)
     ("M-C-v" vm-visit-folder))
   do
   (global-set-key (kbd (cl-first key)) (cl-second key)))
  (cl-loop ;;; shell wizard
   for i from 0 to 9 do
   (global-set-key (kbd (format "C-c %s" i)) 'emacspeak-wizards-shell-by-key))
;;; Smarten up ctl-x-map
  (define-key ctl-x-map "c" 'compile)
  (define-key ctl-x-map "u"  'undo-only)
  (define-key ctl-x-map (kbd "C-u") 'undo-redo)
  (define-key ctl-x-map (kbd "C-d") 'dired-jump)
  (define-key ctl-x-map (kbd "C-n") 'forward-page)
  (define-key ctl-x-map (kbd "C-p") 'backward-page)
;;; Shell mode bindings:
  (with-eval-after-load 'shell  (tvr-shell-bind-keys))
;;; Outline Setup:
  (with-eval-after-load 'outline
    (define-key outline-mode-prefix-map "o" 'open-line) ;;;restore
    (global-set-key (kbd "C-o") outline-mode-prefix-map))
  (server-start)
  (with-eval-after-load 'magit (require 'forge))
  (define-key esc-map (kbd "M-:") 'emacspeak-wizards-show-eval-result)
  (tvr-set-color-for-today)
  (tvr-fastload
      (when (file-exists-p custom-file)  (load custom-file))))

(defsubst tvr-defer-muggles ()
  "Defered muggles loader."
  (tvr-fastload (load "emacspeak-muggles")))

(defun tvr-after-init ()
  "Actions to take after Emacs is up and ready."
;;; load  library-specific settings, customize, then start things.
  (cl-declare (special  tvr-libs))
  (tvr-fastload
      (let ((start (current-time)))
        (load tvr-libs) ;;; load  settings   not  customizable via custom.
        (tvr-time-it "libs" start)
        (setq start (current-time))
        (tvr-customize) ;;; customizations
        (tvr-tabs)
        (tvr-time-it "tvr-Custom" start)
        (setq start (current-time))
        (run-with-idle-timer 1 nil #'yas-reload-all)
        (run-with-idle-timer 0.5 nil #'tvr-defer-muggles)
        (setq start (current-time))
        (when (dbus-list-known-names :session)
          (make-thread #'emacspeak-dbus-setup)
          (tvr-time-it "dbus" start))
        (setq start (current-time))
        (soundscape-toggle)
        (emacspeak-wizards-project-shells-initialize)
        (tvr-time-it "Finishing Up" start))))

(defun tvr-text-mode-hook ()
  "TVR:text-mode"
  (auto-correct-mode 1)
  (auto-fill-mode 1)
  (abbrev-mode 1)
  (unless (eq major-mode 'org-mode) (orgalist-mode 1)))

(defun tvr-prog-mode-hook ()
  "TVR:prog-mode"
  (local-set-key (kbd "C-m") 'newline-and-indent)
  (company-mode)
  (hs-minor-mode)
  (auto-fill-mode)
  (cond
   ((memq major-mode '(emacs-lisp-mode lisp-mode lisp-interaction-mode))
    (lispy-mode ))
   (t (smartparens-mode)))
  (yas-minor-mode)
  (abbrev-mode))

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

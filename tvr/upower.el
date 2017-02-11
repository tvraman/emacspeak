;; -*- coding: utf-8 -*-
;; Integration with UPower daemon over D-Bus.
;; Listens for suspend and resume signals and runs corresponding
;; hook variables.

;; Author: Øyvind Stegard <oyvind.stegard@ifi.uio.no>
;; License: Public domain, use at own risk, no warranties of any kind.

;; Requires Emacs23 or better with D-Bus bindings and a UPower daemon running.
;;
;; You will need to call the function `upower-enable' for things to start
;; happening. Functions you would like to run when machine is suspended:
;; (add-hook 'upower-sleep-hook (lambda() (save-some-buffers t)))
;; Note the time constraint of about a second before machine is actually suspended.
;; So make your hook functions complete quickly.
;;
;; Functions you would like to run when machine is resumed:
;; (add-hook 'upower-resume-hook (lambda() (message "Yawn, wake up already ?")))


;; TODO Define appropriate error handling in case bus service is missing.

;; TODO Rename to something else: the name "upower.el" suggests a much more generic
;; interface to upower, while this code is solely focused on sleep/resume signal
;; relaying.

(require 'dbus)

(defvar upower-sleep-hook nil
  "Functions called when machine is about to sleep (suspend or hibernate).
   Machine will suspend in approximately one second from the time hooks in this
   variable are called.")
(defvar upower-resume-hook nil
  "Functions called when machine is resumed (from suspend or hibernate)")

(defun upower-sleep-signal-handler()
  (message "Received sleep signal, running sleep hooks ..")
  (run-hooks 'upower-sleep-hook))

(defun upower-resume-signal-handler()
  (message "Received resume signal, running resume hooks ..")
  (run-hooks 'upower-resume-hook))

(defun upower-register()
  "Register signal handlers for sleep/resume. Return list of signal registration objects."
  (if (member "PrepareForSleep" (dbus-introspect-get-signal-names :system
                       "org.freedesktop.login1" "/org/freedesktop/login1" "org.freedesktop.login1.Manager"))
      ;; logind Manager interface available, prefer that instead of UPower:
      (list
       (dbus-register-signal :system "org.freedesktop.login1" "/org/freedesktop/login1"
                             "org.freedesktop.login1.Manager" "PrepareForSleep"
                             (lambda(sleep) (if sleep (upower-sleep-signal-handler) (upower-resume-signal-handler)))))
    ;; else Register directly for UPower signals:
    (list
     (dbus-register-signal
      :system
      "org.freedesktop.UPower" "/org/freedesktop/UPower"
      "org.freedesktop.UPower" "Sleeping"
      'upower-sleep-signal-handler)
     (dbus-register-signal
      :system
      "org.freedesktop.UPower" "/org/freedesktop/UPower"
      "org.freedesktop.UPower" "Resuming"
      'upower-resume-signal-handler))
    ))
  
; List holding registered dbus signals
(setq upower-dbus-registration nil)

; Enable integration
(defun upower-enable()
  "Enable integration with UPower. Does nothing if already enabled."
  (interactive)
  (when (not upower-dbus-registration)
    (setq upower-dbus-registration
          (upower-register))
    (message "Enabled integration with UPower daemon.")))

; Disable integration
(defun upower-disable()
  "Disable integration with UPower daemon. Does nothing if already disabled."
  (interactive)
  (while upower-dbus-registration
    (dbus-unregister-object (car upower-dbus-registration))
    (setq upower-dbus-registration (cdr upower-dbus-registration)))
  (message "Disabled integration with UPower daemon."))

(provide 'upower)

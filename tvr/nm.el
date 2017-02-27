;;; nm.el --- Simple NetworkManager integration through D-Bus.
;; -*- coding: utf-8 -*-

;; Author: Øyvind Stegard <oyvind.stegard@ifi.uio.no>
;; License: Public domain, use at own risk, no warranties of any kind.

;; Requires Emacs23 with D-Bus bindings and preferably a running NetworkManager
;; instance.
;;
;; You will need to call the function `nm-enable' for things to start happening.
;; Functions you would like to run when network is connected:
;; (add-hook 'nm-connected-hook 'ping-skynet)
;;
;; Functions you would like to run when network is disconnected:
;; (add-hook 'nm-disconnected-hook (lambda() (message "Darnit, we are down.")))

(require 'dbus)

(defvar nm-connected-hook nil
  "A hook variable which runs when network is connected.")

(defvar nm-disconnected-hook nil
  "A hook variable which runs when network is disconnected.")

(defun nm-is-connected()
  "Returns t if NetworkManager is connected, nil otherwise."
  (equal 3 (dbus-get-property
            :system
            "org.freedesktop.NetworkManager" "/org/freedesktop/NetworkManager"
            "org.freedesktop.NetworkManager" "State")))

(setq nm-dbus-registration nil)
(defun nm-enable()
  "Enable integration with NetworkManager. Does nothing if already enabled."
  (interactive)
  (when (not nm-dbus-registration)
    (setq nm-dbus-registration (dbus-register-signal
                                :system
                                "org.freedesktop.NetworkManager" "/org/freedesktop/NetworkManager"
                                "org.freedesktop.NetworkManager" "StateChanged"
                                'nm-state-dbus-signal-handler))
    (message "Enabled integration with NetworkManager.")))

(defun nm-disable()
  "Disable integration with NetworkManager. Does nothing if already disabled."
  (interactive)
  (when nm-dbus-registration
    (dbus-unregister-object nm-dbus-registration)
    (setq nm-dbus-registration nil)
    (message "Disabled integration with NetworkManager.")))

(defun nm-state-dbus-signal-handler (nmstate)
  "Handles NetworkManager signals and runs appropriate hooks."
  (cond
   ((or (= 4 nmstate) (= 1 nmstate))
    (run-hooks 'nm-disconnected-hook))
   ((= 3 nmstate)
    (run-hooks 'nm-connected-hook))))

(provide 'nm)

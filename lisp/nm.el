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

(defcustom  nm-connected-hook nil
  "A hook variable which runs when network is connected."
  :type 'hook
  :group 'emacspeak-dbus)

(defcustom nm-disconnected-hook nil
  "A hook variable which runs when network is disconnected."
  :type 'hook
  :group 'emacspeak-dbus)

(defvar nm-service "org.freedesktop.NetworkManager"
  "Name of NetworkManager service.")

(defvar nm-path "/org/freedesktop/NetworkManager"
  "NetworkManager Path in DBus.")

(defvar nm-interface "org.freedesktop.NetworkManager"
  "NMetworkManager interface in DBus.")

(defun nm-is-connected()
  "Returns t if NetworkManager is connected, nil otherwise."
  (declare (special nm-service nm-path nm-interface))
  (equal 3
         (dbus-get-property :system
                            nm-service nm-path nm-interface
                            "State")))

(defvar  nm-dbus-registration nil
  "Records if nm-dbus is initialized.")

;;;###autoload
(defun nm-enable()
  "Enable integration with NetworkManager. Does nothing if already enabled."
  (interactive)
  (declare (special nm-service nm-path nm-interface))
  (declare (special nm-dbus-registration))
  (unless nm-dbus-registration
    (setq nm-dbus-registration
          (dbus-register-signal
           :system
           nm-service nm-path nm-interface
           "StateChanged"
           'nm-state-dbus-signal-handler))
    (message "Enabled integration with NetworkManager.")))

;;;###autoload
(defun nm-disable()
  "Disable integration with NetworkManager. Does nothing if already disabled."
  (interactive)
  (declare (special nm-dbus-registration))
  (when nm-dbus-registration
    (dbus-unregister-object nm-dbus-registration)
    (setq nm-dbus-registration nil)
    (message "Disabled integration with NetworkManager.")))

(defun nm-state-dbus-signal-handler (nm-state)
  "Handles NetworkManager signals and runs appropriate hooks."
  (cond
   ((or (= 4 nm-state) (= 1 nm-state))
    (run-hooks 'nm-disconnected-hook))
   ((= 3 nm-state)
    (run-hooks 'nm-connected-hook))))

(provide 'nm)

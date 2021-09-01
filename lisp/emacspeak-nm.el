;;; emacspeak-nm.el --- Simple NetworkManager integration through D-Bus.  -*- lexical-binding: t; -*-
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

(defun nm-service-p()
  "Checks whether NetworkManager service is available on system bus."
  (member "org.freedesktop.NetworkManager" (dbus-list-known-names :system)))

(defun nm-connected-p()
  "Returns nil if NetworkManager service indicates that network is down, t otherwise. "
  (and
   (nm-service-p)
   (equal 70
          (dbus-get-property :system
                             "org.freedesktop.NetworkManager"
                             "/org/freedesktop/NetworkManager"
                             "org.freedesktop.NetworkManager" "State"))))

(defvar nm-dbus-registration nil
  "Record if registered with NM.")

(defun nm-enable()
  "Enable integration with NetworkManager. Does nothing if
already enabled or service is not available."
  (interactive)
  (cl-declare (special nm-dbus-registration))
  (cond
   ((not (nm-service-p))
    (message "NetworkManager service not available.") nil)
   ((not nm-dbus-registration)
    (setq nm-dbus-registration
          (dbus-register-signal
           :system
           "org.freedesktop.NetworkManager" "/org/freedesktop/NetworkManager"
           "org.freedesktop.NetworkManager" "StateChanged"
           'nm-state-dbus-signal-handler))
    (message "Enabled integration with NetworkManager."))
   ((message "Integration with NetworkManager already enabled."))))

(defun nm-disable()
  "Disable integration with NetworkManager. Does nothing if already disabled."
  (interactive)
  (cl-declare (special nm-dbus-registration))
  (when nm-dbus-registration
    (dbus-unregister-object nm-dbus-registration)
    (setq nm-dbus-registration nil)
    (message "Disabled integration with NetworkManager.")))

;; NM_STATE numbers and meanings:
;; NM_STATE_UNKNOWN = 0
;;     Networking state is unknown. 
;; NM_STATE_ASLEEP = 10
;;     Networking is inactive and all devices are disabled. 
;; NM_STATE_DISCONNECTED = 20
;;     There is no active network connection. 
;; NM_STATE_DISCONNECTING = 30
;;     Network connections are being cleaned up. 
;; NM_STATE_CONNECTING = 40
;;     A network device is connecting to a network and there is no other available network connection. 
;; NM_STATE_CONNECTED_LOCAL = 50
;;     A network device is connected, but there is only link-local connectivity. 
;; NM_STATE_CONNECTED_SITE = 60
;;     A network device is connected, but there is only site-local connectivity. 
;; NM_STATE_CONNECTED_GLOBAL = 70
;;     A network device is connected, with global network connectivity. 
;;
;; See:
;; https://developer.gnome.org/NetworkManager/unstable/spec.html#type-NM_STATE

(defun nm-state-dbus-signal-handler (nmstate)
  "Handles NetworkManager signals and runs appropriate hooks."
  (cond
   ((or (= 10 nmstate) (= 20 nmstate) (= 30 nmstate) (= 50 nmstate) (= 60 nmstate))
    (run-hooks 'nm-disconnected-hook))
   ((= 70 nmstate)
    (run-hooks 'nm-connected-hook))))

(provide 'emacspeak-nm)

;;; emacspeak-dbus.el --- DBus On Emacspeak Desktop -*- lexical-binding: t; -*-
;;; $Id: emacspeak-dbus.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  DBus Tools For The Emacspeak Desktop
;;; Keywords: Emacspeak,  Audio Desktop dbus
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2018, T. V. Raman
;;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
;;; All Rights Reserved.
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNDBUS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; Loading this module sets  up Emacspeak to respond to DBus notifications.
;;; This module needs to be loaded explicitly from the user's init file
;;; after emacspeak has been started.

;;; @subsection Overview
;;;
;;; This module provides integration via DBus  for the following:
;;; @itemize @bullet
;;; @item Respond to network coming up or going down
;;; --- @code{(nm-enable)}.
;;; @item Respond to screen getting locked/unlocked by gnome-screen-saver
;;; --- @code{(emacspeak-dbus-watch-screen-lock)}.
;;; @item Respond to laptop  going to sleep or waking up
;;; ---  @code{(emacspeak-dbus-sleep-enable)}.
;;; @item Respond to insertion/ejection of removable storage
;;; --- @code{(emacspeak-dbus-udisks-enable)}.
;;; @item Watch for power devices
;;; --- @code{(emacspeak-dbus-upower-enable)}.
;;; @item An interactive command  @command{emacspeak-dbus-lock-screen}
;;; bound to @kbd{C-, C-d} to lock the screen using DBus.
;;; Note: this key-binding is available only if this module is loaded.
;;; @end itemize
;;; Add calls to the desired functions from the above list
;;; to the emacs startup file after  this module has been loaded.
;;; See relevant hooks for customizing behavior.
;;; Note that each of the  sleep/wake-up, UDisks2   and network/up-down
;;; can be separately enabled/disabled, and the actions customized
;;; via appropriately named hook functions.
;;;

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'amixer)
(require 'derived)
(require 'dbus)
(require 'nm "nm" 'no-error)

;;}}}
;;{{{ Forward Declarations:

(declare-function soundscape-restart "soundscape" (&optional device))
(declare-function soundscape-tickle "soundscape" nil)
(declare-function soundscape-listener-shutdown "soundscape" nil)
(declare-function jabber-connect-all "jabber-core" (&optional arg))
(declare-function jabber-disconnect "jabber-core" (&optional arg))
(declare-function twittering-start "ext:twittering-mode" nil)
(declare-function twittering-stop "twittering-mode" nil)

;;}}}
;;{{{ ScreenSaver Mode:

(define-derived-mode emacspeak-screen-saver-mode special-mode
  "Screen Saver Mode"
  "A light-weight mode for the `*Emacspeak Screen Saver *' buffer.
This is a hidden buffer that is made current so we automatically
switch to a screen-saver soundscape."
  (setq header-line-format "")
  t)

(defvar emacspeak-screen-saver-saved-configuration  nil
  "Record window configuration when screen-saver was launched.")

(defun emacspeak-screen-saver ()
  "Launch Emacspeak screen-saver.
Initialize screen-saver buffer  if needed, and switch to  it."
  (cl-declare (special emacspeak-screen-saver-saved-configuration))
  (setq emacspeak-screen-saver-saved-configuration (current-window-configuration))
  (let ((buffer (get-buffer-create "*Emacspeak Screen Saver*")))
    (with-current-buffer buffer (emacspeak-screen-saver-mode))
    (funcall-interactively #'switch-to-buffer buffer)
    (delete-other-windows)))

;;}}}
;;{{{ NM Handlers
(declare-function ems-get-active-network-interfaces "emacspeak-wizards" nil)

(defun emacspeak-dbus-nm-connected ()
  "Announce  network manager connection.
Startup  apps that need the network."
  (cl-declare (special emacspeak-speak-network-interfaces-list))
  (setq emacspeak-speak-network-interfaces-list
        (ems-get-active-network-interfaces))
  (dtk-notify-say "Network up")
  (emacspeak-play-auditory-icon 'network-up))

(defun emacspeak-dbus-nm-disconnected ()
  "Announce  network manager disconnection.
Stop apps that use the network."
  (cl-declare (special emacspeak-speak-network-interfaces-list))
  (setq emacspeak-speak-network-interfaces-list
        (mapcar #'car (network-interface-list)))
  (emacspeak-auditory-icon 'network-down)
  (dtk-notify-say "Network down")
  (message (mapconcat #'identity emacspeak-speak-network-interfaces-list "")))

(add-hook 'nm-connected-hook 'emacspeak-dbus-nm-connected)
(add-hook 'nm-disconnected-hook 'emacspeak-dbus-nm-disconnected)

;;}}}
;;{{{ Sleep/Resume:

(defun emacspeak-dbus-login1-sleep-p ()
  "Test if login1 service  sleep signal is available."
  (member
   "PrepareForSleep"
   (dbus-introspect-get-signal-names
    :system
    "org.freedesktop.login1" "/org/freedesktop/login1"
    "org.freedesktop.login1.Manager")))

(defvar emacspeak-dbus-sleep-hook nil
  "Functions called when machine is about to sleep (suspend or hibernate). ")

(defvar emacspeak-dbus-resume-hook nil
  "Functions called when machine is resumed (from suspend or hibernate).")

(defun emacspeak-dbus-sleep-signal-handler()
  (run-hooks 'emacspeak-dbus-sleep-hook))

(defun emacspeak-dbus-resume-signal-handler()
  "Resume handler"
  (tts-restart)
  (run-hooks 'emacspeak-dbus-resume-hook))

(defun emacspeak-dbus-screensaver-check ()
  "Check  and fix Emacs DBus Binding to gnome-screensaver"
  (ems-with-messages-silenced
   (condition-case nil
       (dbus-call-method
        :session
        "org.gnome.ScreenSaver" "/org/gnome/ScreenSaver"
        "org.gnome.ScreenSaver" "GetActive")
     (error
      (progn
        (shell-command
         "pidof gnome-screensaver \
 && kill -9 `pidof gnome-screensaver` 2>&1 > /dev/null")
        (start-process "screen-saver" nil "gnome-screensaver"))))
   t))

(defvar emacspeak-dbus-sleep-registration nil
  "List holding sleep registration.")

(defun emacspeak-dbus-sleep-register()
  "Register signal handlers for sleep/resume. Return list of
signal registration objects."
  (cond
   ((emacspeak-dbus-login1-sleep-p)
    (emacspeak-dbus-screensaver-check)
    (list
     (dbus-register-signal
      :system "org.freedesktop.login1" "/org/freedesktop/login1"
      "org.freedesktop.login1.Manager" "PrepareForSleep"
      #'(lambda(sleep)
          (if sleep
              (emacspeak-dbus-sleep-signal-handler)
            (emacspeak-dbus-resume-signal-handler))))))
   (t (error "org.freedesktop.login1 has no PrepareForSleep signal."))))

;;; Enable integration
(defun emacspeak-dbus-sleep-enable()
  "Enable integration with Login1. Does nothing if already enabled."
  (interactive)
  (cl-declare (special emacspeak-dbus-sleep-registration))
  (unless emacspeak-dbus-sleep-registration
    (setq emacspeak-dbus-sleep-registration (emacspeak-dbus-sleep-register))))

;;; Disable integration
(defun emacspeak-dbus-sleep-disable()
  "Disable integration with login1 daemon. Does nothing if
already disabled."
  (interactive)
  (cl-declare (special emacspeak-dbus-sleep-registration))
  (while emacspeak-dbus-sleep-registration
    (dbus-unregister-object (car emacspeak-dbus-sleep-registration))
    (setq emacspeak-dbus-sleep-registration
          (cdr emacspeak-dbus-sleep-registration))))

(defun emacspeak-dbus-sleep ()
  "Emacspeak  hook for -sleep signal from Login1."
  (soundscape-listener-shutdown)
    (save-some-buffers t))

(add-hook  'emacspeak-dbus-sleep-hook#'emacspeak-dbus-sleep)

(defun emacspeak-dbus-resume ()
  "Emacspeak hook for Login1-resume."
  (cl-declare (special amixer-alsactl-config-file))
  (emacspeak-prompt "waking-up")
  (when (featurep 'xbacklight) (xbacklight-black))
  (amixer-restore amixer-alsactl-config-file)
  (when (featurep 'soundscape) (soundscape-restart))
  (when
      (dbus-call-method
       :session
       "org.gnome.ScreenSaver" "/org/gnome/ScreenSaver"
       "org.gnome.ScreenSaver" "GetActive")
    (emacspeak-prompt "pwd")
    (emacspeak-auditory-icon 'help)))

(add-hook 'emacspeak-dbus-resume-hook #'emacspeak-dbus-resume)

;;}}}
;;{{{ UDisks2:

(defvar emacspeak-dbus-udisks-registration nil
  "List holding storage (UDisks2) registration.")

(defun emacspeak-dbus-udisks-register()
  "Register signal handlers for UDisks2  InterfacesAdded signal."
  (list
   (dbus-register-signal
    :system
    "org.freedesktop.UDisks2" "/org/freedesktop/UDisks2"
    "org.freedesktop.DBus.ObjectManager" "InterfacesAdded"
    #'(lambda(path _props)
        (emacspeak-play-auditory-icon 'open-object)
        (message "Added storage %s" path)))
   (dbus-register-signal
    :system
    "org.freedesktop.UDisks2" "/org/freedesktop/UDisks2"
    "org.freedesktop.DBus.ObjectManager" "InterfacesRemoved"
    #'(lambda(path _props)
        (message "Removed storage %s" path)
        (emacspeak-play-auditory-icon 'close-object)))))

(defun emacspeak-dbus-udisks-enable()
  "Enable integration with UDisks2. Does nothing if already enabled."
  (interactive)
  (cl-declare (special emacspeak-dbus-udisks-registration))
  (unless emacspeak-dbus-udisks-registration
    (setq emacspeak-dbus-udisks-registration (emacspeak-dbus-udisks-register))))

;;; Disable integration
(defun emacspeak-dbus-udisks-disable()
  "Disable integration with UDisks2 daemon. Does nothing if
already disabled."
  (interactive)
  (cl-declare (special emacspeak-dbus-udisks-registration))
  (while emacspeak-dbus-udisks-registration
    (dbus-unregister-object (car emacspeak-dbus-udisks-registration))
    (setq emacspeak-dbus-udisks-registration
          (cdr emacspeak-dbus-udisks-registration))))

;;}}}
;;{{{ UPower:

(defvar emacspeak-dbus-upower-registration nil
  "List holding storage (UPower) registration.")

(defun emacspeak-dbus-upower-register()
  "Register signal handlers for UPower  InterfacesAdded signal."
  (list
   (dbus-register-signal
    :system
    "org.freedesktop.UPower" "/org/freedesktop/UPower"
    "org.freedesktop.UPower" "DeviceAdded"
    #'(lambda(device)
        (emacspeak-play-auditory-icon 'on)
        (message "Added device %s" device)))
   (dbus-register-signal
    :system
    "org.freedesktop.UPower" "/org/freedesktop/UPower"
    "org.freedesktop.UPower" "DeviceRemoved"
    #'(lambda(device)
        (message "Removed device  %s" device)
        (emacspeak-play-auditory-icon 'off)))))

(defun emacspeak-dbus-upower-enable()
  "Enable integration with UPower. Does nothing if already enabled."
  (interactive)
  (cl-declare (special emacspeak-dbus-upower-registration))
  (unless emacspeak-dbus-upower-registration
    (setq emacspeak-dbus-upower-registration (emacspeak-dbus-upower-register))))

;;; Disable integration
(defun emacspeak-dbus-upower-disable()
  "Disable integration with UPower daemon. Does nothing if
already disabled."
  (interactive)
  (cl-declare (special emacspeak-dbus-upower-registration))
  (while emacspeak-dbus-upower-registration
    (dbus-unregister-object (car emacspeak-dbus-upower-registration))
    (setq emacspeak-dbus-upower-registration
          (cdr emacspeak-dbus-upower-registration))))

;;}}}
;;{{{ Interactive Command: Lock Screen
(defun emacspeak-dbus-lock-screen ()
  "Lock screen using DBus."
  (interactive)
  (emacspeak-dbus-screensaver-check)
  (emacspeak-auditory-icon 'close-object)
  (emacspeak-prompt "locking-up")
  (when (featurep 'xbacklight) (xbacklight-black))
  (dbus-call-method
   :session
   "org.gnome.ScreenSaver"
   "/"
   "org.gnome.ScreenSaver"
   "Lock"))

(global-set-key (ems-kbd "C-, C-d") 'emacspeak-dbus-lock-screen)
;;}}}
;;{{{ Watch Screensaver:

(defvar emacspeak-dbus-screen-lock-handle nil
  "Handle to DBus signal registration for watching screenlock.")

(defun emacspeak-dbus-watch-screen-lock ()
  "Register a handler to watch screen lock/unlock."
  (cl-declare (special emacspeak-dbus-screen-lock-handle
                       emacspeak-screen-saver-saved-configuration))
  (setq
   emacspeak-dbus-screen-lock-handle
   (dbus-register-signal
    :session
    "org.gnome.ScreenSaver" "/org/gnome/ScreenSaver"
    "org.gnome.ScreenSaver" "ActiveChanged"
    #'(lambda (lock)
        (if lock
            (progn (emacspeak-screen-saver))
          (progn
            (emacspeak-prompt "success")
            (when (eq major-mode 'emacspeak-screen-saver-mode)(quit-window))
            (when
                (window-configuration-p emacspeak-screen-saver-saved-configuration)
              (set-window-configuration emacspeak-screen-saver-saved-configuration))
            (emacspeak-speak-mode-line)))))))

(defun emacspeak-dbus-unwatch-screen-lock ()
  "De-Register a handler to watch screen lock/unlock."
  (cl-declare (special emacspeak-dbus-screen-lock-handle))
  (dbus-unregister-object emacspeak-dbus-screen-lock-handle)
  (setq emacspeak-dbus-screen-lock-handle nil))

;;}}}
;;{{{Setup:
;;;###autoload
(defun emacspeak-dbus-setup ()
  "Turn on all defined handlers."
  (nm-enable)
    (emacspeak-dbus-sleep-enable)
    (emacspeak-dbus-udisks-enable)
    (emacspeak-dbus-upower-enable)
    (emacspeak-dbus-watch-screen-lock))

;;}}}
(provide 'emacspeak-dbus)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

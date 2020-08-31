(setq pre-redisplay-function nil
      x-wait-for-event-timeout 0
      mail-host-address "google.com"
      user-mail-address "raman@google.com")
;;; To disable touchpad in all apps:


(defvar touchpad-device "12"
  "Device ID of touchpad.
Set by locating it via xinput --list.")

(defun turn-off-touchpad (&optional frame)
  (interactive)
  (declare (special touchpad-device))
  (start-process "xinput" nil 
    "xinput" "set-prop" touchpad-device "Device Enabled" "0")
  (message "Disabled touchpad"))

(defun turn-on-touchpad (&optional frame)
  (interactive)
  (declare (special touchpad-device))
  (start-process "xinput" nil 
    "xinput" "set-prop" touchpad-device "Device Enabled" "1")
  (message "Enabled touchpad"))

(when (fboundp 'global-disable-mouse-mode)
  (global-disable-mouse-mode)
  (setq disable-mouse-command 'dtk-stop))
(turn-off-touchpad)


(provide 'laptop-local)

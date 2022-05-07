(setq pre-redisplay-function nil
      x-wait-for-event-timeout 0
      mail-host-address "google.com"
      user-mail-address "raman@google.com")
(xbacklight-black)
(defvar touchpad-device "10"
  "Device ID of synaptics.
Set by locating it via xinput list | grep -i touchpad ")

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

;(turn-off-touchpad)
(provide 'laptop-local)

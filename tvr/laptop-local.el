(setq pre-redisplay-function nil
      x-wait-for-event-timeout 0
      mail-host-address "google.com"
      user-mail-address "raman@google.com")
(light-black)
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

(defun tvr-calendar ()
  "Open Google Calendar in Chrome"
  (interactive)
  (browse-url-chrome "calendar/"))
(define-key emacspeak-personal-y-keymap "c" 'tvr-calendar)



(defun tvr-chat ()
  "Open Google Chat in Chrome"
  (interactive)
  (browse-url-chrome "chat/"))

(define-key emacspeak-personal-y-keymap " " 'tvr-chat)

(defun tvr-mail ()
  "Open Google Mail in Chrome"
  (interactive)
  (browse-url-chrome "chat/"))


(define-key emacspeak-personal-y-keymap "m" 'tvr-mail)
(provide 'laptop-local)

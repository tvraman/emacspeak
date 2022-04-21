;;; Disable mouse buttons and touchpad.
;; Instead have all mouse-buttons run dtk-stop 
;; Also, Avoids accidental touches  on touch-pad
(require 'cl-lib)
;(load-library "disable-mouse-autoloads")

;; To disable touchpad in all apps:
;; but if you want to completely disable it, you can do the following
;; In a terminal type:
;; xinput list | grep -i touchpad
;; Thinkpad X250: ⎜   ↳ SynPS/2 Synaptics TouchPad              	id=11	[slave  pointer  (2)]
;; to determine the device ID (in my case, 11). Then disable by typing:
;; xinput set-prop 11 "Device Enabled" 0
;; To enable it, type:
;; xinput set-prop 11 "Device Enabled" 1
(defgroup touchpad-unprepare nil
  "disable mouse/touchpad in emacs"
  :group 'applications)
(defcustom touchpad-device "12"
  "Device ID of touchpad.
Set by locating it via xinput --list."
  :type 'string
  :group 'touchpad-unprepare)

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

;(add-hook 'focus-in-hook #'turn-off-touchpad)
;(add-hook 'focus-out-hook #'turn-on-touchpad)
;(add-hook 'delete-frame-functions #'turn-on-touchpad)
(when (fboundp 'global-disable-mouse-mode)
  (global-disable-mouse-mode)
  (setq disable-mouse-command 'dtk-stop))
(turn-off-touchpad)

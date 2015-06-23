;;; Disable mouse buttons and touchpad.
;;; Avoids accidental touches 

(loop
 for  k in 
 '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]    
   [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]  
   [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]  
   [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]  
   [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5])
 do
 (global-unset-key k))

;; disable on modeline etc:
(setq mode-line-coding-system-map nil    
      mode-line-column-line-number-mode-map nil
      mode-line-input-method-map nil)


;;; To disable touchpad in all apps:
;; but if you want to completely disable it, you can do the following
;; In a terminal type:
;; xinput list | grep -i touchpad
;;; Thinkpad X250: ⎜   ↳ SynPS/2 Synaptics TouchPad              	id=11	[slave  pointer  (2)]
;; to determine the device ID (in my case, 11). Then disable by typing:
;; xinput set-prop 11 "Device Enabled" 0
;; To enable it, type:
;; xinput set-prop 11 "Device Enabled" 1
(defgroup touchpad-unprepare nil
  "disable mouse/touchpad in emacs")
(defcustom touchpad-device "11"
  "Device ID of touchpad.
Set by locating it via xinput --list."
  :type 'string
  :group 'touchpad-unprepare)

(defun turn-off-mouse (&optional frame)
  (interactive)
  (declare (special touchpad-device))
  (shell-command
   (format
    "xinput set-prop %s \"Device Enabled\" 0"
    touchpad-device))
  (message "Disabled touchpad"))

(defun turn-on-mouse (&optional frame)
  (interactive)
  (declare (special touchpad-device))
  (shell-command
   (format
    "xinput set-prop %s \"Device Enabled\" 1"
    touchpad-device))
  (message "Enabled touchpad"))

;(add-hook 'focus-in-hook #'turn-off-mouse)
;(add-hook 'focus-out-hook #'turn-on-mouse)
;(add-hook 'delete-frame-functions #'turn-on-mouse)

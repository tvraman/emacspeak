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

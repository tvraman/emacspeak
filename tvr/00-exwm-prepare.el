;;{{{Load and start exwm:

(require 'exwm)
(require 'exwm-config)
(exwm-config-example)
(exwm-config-ido)
;;}}}
;;{{{Look and feel:

(setq exwm-workspace-number 2)

;;; Name exwm buffers meaningfully:

(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))
;;; hide echo-area when not used:
(setq exwm-workspace-minibuffer-position 'bottom)

;;}}}
;;{{{Global Keys:

;;}}}

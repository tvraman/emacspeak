;;{{{Load and start exwm:

(require 'exwm)
(require 'exwm-config)
(exwm-config-example)
(exwm-config-ido)
;;}}}
;;{{{Look and feel:

(setq exwm-workspace-number 2)




;;; hide echo-area when not used:
(setq exwm-workspace-minibuffer-position 'bottom)

;;}}}
;;{{{Global Keys:

;;}}}
;;{{{Control Orca:
(load-library "orca")
;;}}}

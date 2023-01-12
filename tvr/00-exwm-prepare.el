;;{{{Load and start exwm:

(require 'exwm)
(require 'exwm-config)
(exwm-config-example)
;;}}}
;;{{{Look and feel:

(setq exwm-workspace-number 2)




;;; hide echo-area when not used:
(setq exwm-workspace-minibuffer-position 'bottom)

;;}}}
;;{{{Global Keys:
;; I set s-. to exwm-reset via custom
;; recover emacspeak prefix in exwm buffers: 
(define-key exwm-mode-map emacspeak-prefix 'emacspeak-keymap)
;; doesn't appear to be a need to restore this: switch to char-mode if needed.
;;}}}
;;{{{Control Orca:
(load-library "orca")
;;}}}

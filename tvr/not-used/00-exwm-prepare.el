;;; No-Op if Stumpwm already running:
(when
    (zerop  (length  (string-trim (shell-command-to-string "pidof stumpwm"))))
    
  ;;{{{Load and start exwm:

  (require 'exwm)
  (require 'exwm-config)
  (exwm-config-example)
  ;;}}}
  ;;{{{Look and feel:

  (setq exwm-workspace-number 3)

  (add-hook 'exwm-update-title-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-title)))

  ;;}}}


  )

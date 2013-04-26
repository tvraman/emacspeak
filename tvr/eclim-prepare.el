;;; Install emacs-eclim via elpa.
(require 'emacs-eclim-autoloads)
(require 'eclim)
(global-eclim-mode)

;; Variables
(setq eclimd-wait-for-process nil
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 2
      )

;; Call the help framework with the settings above & activate
;; eclim-mode
(help-at-pt-set-timer)

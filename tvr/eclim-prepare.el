;;; Install emacs-eclim via elpa.
(require 'emacs-eclim-autoloads)
(require 'eclim)
(require 'eclimd)
(global-eclim-mode)

;; Variables
(setq eclimd-wait-for-process nil
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 2
      )

;; Call the help framework with the settings above & activate
;; eclim-mode
(help-at-pt-set-timer)

(setq semantic-default-submodes
      '(global-semantic-idle-scheduler-mode
        global-semanticdb-minor-mode
        global-semantic-idle-summary-mode
        global-semantic-mru-bookmark-mode))
(semantic-mode 1)

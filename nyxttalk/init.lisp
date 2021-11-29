(ql:quickload :tts)
(tts:init) ;;; (tts:init :engine "espeak" for espeak
 (tts:speak "Welcome To The Next In Browsers!")
(define-configuration buffer
    ((default-modes (append '(emacs-mode) %slot-default))))
(swank:create-server :port 4006 :dont-close t)
(tts:speak "Swank server started at port 4006")

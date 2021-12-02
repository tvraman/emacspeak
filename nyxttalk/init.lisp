;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init
        (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init) (load quicklisp-init)))

(ql:quickload :tts)
(tts:init) ;;; (tts:init :engine "espeak" for espeak
(tts:speak "Welcome To The Next In Browsers!")

(ql:quickload :aweb)

(swank:create-server :port 4006 :dont-close t)
(tts:speak "Swank server started at port 4006")

(define-configuration buffer
  ((default-modes (append '(emacs-mode) %slot-default%))))

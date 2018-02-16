;; -*- lexical-binding: nil; -*-
(setq pre-redisplay-function nil)

(load-library "touchpad-disable")
(load-library "xbacklight")
(when emacspeak-mail-alert (emacspeak-toggle-mail-alert))
(provide 'laptop-local)

;; -*- lexical-binding: nil; -*-
(setq pre-redisplay-function nil
      x-wait-for-event-timeout 0)

(load-library "touchpad-disable")
(load-library "xbacklight")
(when emacspeak-mail-alert (emacspeak-toggle-mail-alert))
(provide 'laptop-local)

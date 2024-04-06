(setq
                                        ;pre-redisplay-function nil
 x-wait-for-event-timeout 0
 mail-host-address "google.com"
 user-mail-address "raman@google.com")
(light-black)

(defun tvr-calendar ()
  "Open Google Calendar in Chrome"
  (interactive)
  (browse-url-chrome "calendar/"))
(define-key emacspeak-v-keymap "c" 'tvr-calendar)

(defun tvr-chat ()
  "Open Google Chat in Chrome"
  (interactive)
  (browse-url-chrome "go/chat/"))

(define-key emacspeak-v-keymap " " 'tvr-chat)

(defun tvr-mail ()
  "Open Google Mail in Chrome"
  (interactive)
  (browse-url-chrome "https://mail//"))

(define-key emacspeak-v-keymap "m" 'tvr-mail)

(defun tvr-snippets ()
  "Open Google Snippets in Chrome"
  (interactive)
  (browse-url-chrome "go/snippets"))

(define-key emacspeak-v-keymap "s" 'tvr-snippets)

(defun tvr-time-off ()
  "Open time-off in Chrome"
  (interactive)
  (browse-url-chrome "go/tom"))

(define-key emacspeak-v-keymap "t" 'tvr-time-off)

(provide 'laptop-local)

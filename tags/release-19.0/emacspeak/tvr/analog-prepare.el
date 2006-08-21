(load-library "analog")
(setq analog-entry-list
      '(
        ;; files in the WWW group
        ("/etc/httpd/logs/access_log"
         (group . "WWW")
         (lines . 10)
         (fields . (33 66)))                  ; show 10 lines
        ("/etc/httpd/logs/error_log"
         (fields . (26 56))
         (group . "WWW")
         (lines . 10))
        ;; files in the Mail group
        ("/var/log/maillog"
         (fields . (16 41))
         (group . "Mail")
         (hide
          . ("queue" "completed")))     ; hide lines matching queue/completed
        ;; commands in the Commands group
        ("df -h"
         (group . "Commands")
         (type . command)               ; commands must be identified
         (lines . all))                 ; keep all lines
        ("free"
         (group . "Commands")
         (type . command)
         (position . head)              ; monitor the head of the output
         (lines . 6))
        ("last"
         (group . "Commands")
         (type . command)
         (position . head)              ; monitor the head of the output
         (lines . 6))
        ("ps aux"
         (group . "Commands")
         (type . command)
         (keep . "tvraman")                ; keep lines matching matt
         (hide . ("bash" "ssh" )) ; hide/keep can be lists
         (lines . all))
        ;; files in the System group
        ("/var/log/messages"
         (group . "System")
         (hide . ("CRON" ))
         (fields . (16 24 32)))))

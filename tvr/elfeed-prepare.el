(augment-load-path "elfeed" "elfeed")
(load-library "elfeed")

    ;; Run once an hour
    (run-at-time t (* 60 60) #'elfeed-update)

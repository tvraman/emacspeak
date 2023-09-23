;; -*- lexical-binding: nil; -*-


(eval-after-load "elfeed"
  `(progn
     ;; Run once an hour
     (defvar my-elfeed-timer 
           (run-at-time t (* 60 60) #'elfeed-update))))

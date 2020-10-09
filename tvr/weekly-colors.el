(require  'cl-lib)
;;{{{Weekday Colors:

(defconst tvr-weekday-color-map
  [("light sky blue" . "#6FBD87")       ; silver tree
   ("royal blue" . "#FFD724")              ;RoyalBlue on pink
   ("#F4C430" . "sea green")            ; saffron
   ("#FFFFDA" . "royal blue")
   ("mint cream" . "royal blue")
   ("PowderBlue" . "gold")
   ("#FFF3FF" . "gold")]                ; lavender blush
  "Alist of color pairs for days of the week")

(defsubst tvr-set-color-for-today ()
  "Set color pair for today."
  (cl-declare (special tvr-weekday-color-map))
  (let ((pair (aref tvr-weekday-color-map (read (format-time-string "%w")))))
    (set-background-color (car pair))
    (set-foreground-color (cdr pair))))

(defun bw ()
  "set foreground to black"
  (set-foreground-color "black"))

;;}}}

(require  'cl-lib)
;;{{{Weekday Colors:

(defconst tvr-weekday-color-map
  [                                    ; 0 is Sunday 
   ("light sky blue" . "#6FBD87")      ; light sky blue on silver tree
   ("royal blue" . "#FFD724")          ;RoyalBlue on pink
   ("#F4C430" . "sea green")           ; saffron on sea green
   ("#FFFFDA" . "royal blue")          ; light yellow on blue
   ("mint cream" . "royal blue")
   ("PowderBlue" . "gold")
   ] 
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

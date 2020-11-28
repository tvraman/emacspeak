;;{{{span: temporary fix
;;; Sites like cricinfo use bad markup and lose inter-word space

(defadvice shr-tag-span (around emacspeak pre act comp)
   "Render span with spaces around its content. "
   (insert " ")
   ad-do-it
   (insert " "))

;;}}}

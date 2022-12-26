;;; Emacs' yes-or-no-p and y-or-no-p have prompts hard-wired.
;; The advice forms below produce more succinct speech.

(defadvice yes-or-no-p (around emacspeak pre act comp)
  "speak."
  (cond
    (use-short-answers
     (let* ((ask (concat (ad-get-arg 0) " y/n "))
            (c (read-char ask)))
       (while (not (member c '(?n ?y)))
              (setq c  (read-char ask)))
       (setq ad-return-value
             (cl-case c
                      (?y t)
                      (?n nil)))))
    (t ad-do-it))
  (emacspeak-auditory-icon
   (if ad-return-value 'y-answer 'n-answer))
  ad-return-value)




(defadvice y-or-n-p (around emacspeak pre act comp)
  "speak."
  (let* ((ask (concat (ad-get-arg 0) " y/n "))
         (c (read-char ask)))
    (while (not (member c '(?n ?y)))
           (setq c  (read-char ask)))
    (setq ad-return-value
          (cl-case c
                   (?y t)
                   (?n nil))))
  (emacspeak-auditory-icon
   (if ad-return-value 'y-answer 'n-answer))
  ad-return-value)

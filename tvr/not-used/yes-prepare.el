;;; Emacs' yes-or-no-p and y-or-no-p have prompts hard-wired.
;; The advice forms below produce more succinct speech.

(defadvice yes-or-no-p (around tvr-fix pre act comp)
  "Simplify Emacs' implementation."
  (cond
    (use-short-answers
     (let* ((ask (concat (ad-get-arg 0) " y/n "))
            (c (read-char ask)))
       (while (not (member c '(?n ?y)))
              (emacspeak-auditory-icon
   (if ad-return-value 'y-answer 'n-answer))
              (setq c  (read-char ask))
              (emacspeak-auditory-icon  'ask-question))
       (setq ad-return-value
             (cl-case c
                      (?y t)
                      (?n nil)))))
    (t ad-do-it))
  ad-return-value)

(defadvice y-or-n-p (around tvr-fix pre act comp)
  "Simplify Emacs' implementation.."
  (let* ((ask (concat (ad-get-arg 0) " y/n "))
         (c (read-char ask)))
    (while (not (member c '(?n ?y)))
           (emacspeak-auditory-icon  'ask-short-question)
           (setq c  (read-char ask)))
    (setq ad-return-value
          (cl-case c
                   (?y t)
                   (?n nil))))
  ad-return-value)

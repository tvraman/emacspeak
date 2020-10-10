(defun new-kbd (string )
  "Simplified and hopefully more robust kbd function.
Always returns a vector i.e. like passing need-vector to edmacro-parse-keys. "
  (let ((res [])
        (special-char-reg "^\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\)$")
        (modifier+angle-reg "^\\(\\([ACHMsS]-\\)*\\)<\\(.+\\)>$"))
    (cl-loop
     for word in (split-string string)
     do
     (let* ((key nil))
       (cond 
        ((and ;;; modifier+-<key> without DEL etc
          (not (string-match special-char-reg word))
          (string-match modifier+angle-reg word))
         (setq key
               (list
                (intern 
                 (concat ;;; strip < and >
                  (substring word (match-beginning 1) (match-end 1))
                  (substring word (match-beginning 3) (match-end 3)))))))
        (t
         (let ((prefix 0)
               (bits 0))
           (while ;;; calculate modifier bits
               (string-match "^[ACHMsS]-." word)
             (cl-incf bits
                      (cdr
                       (assq (aref word 0)
                             '((?A . ?\A-\^@)
                               (?C . ?\C-\^@)
                               (?H . ?\H-\^@)
                               (?M . ?\M-\^@)
                               (?s . ?\s-\^@)
                               (?S . ?\S-\^@)))))
             (cl-incf prefix 2)
             (cl-callf substring word 2))
           (when (string-match "^\\^.$" word)
             (cl-incf bits ?\C-\^@)
             (cl-incf prefix)
             (cl-callf substring word 1))
           (when-let
               (found
                (assoc word
                       '(("NUL" . "\0")
                         ("RET" . "\r")
                         ("LFD" . "\n")
                         ("TAB" . "\t")
                         ("ESC" . "\e")
                         ("SPC" . " ")
                         ("DEL" . "\177"))))
             (setq word (cdr found)))
           (cond ;;; apply modifiers 
            ((= bits 0) (setq key word))
            ((/= (length word) 1)
             (error "%s: Prefix  must precede a single character, not %s"
                    string word))
            ((and
              (/= (logand bits ?\C-\^@) 0)
              (string-match "[@-_a-z]" word))
             (setq key
                   (list (+ bits (- ?\C-\^@)
                            (logand (aref word 0) 31)))))
            (t (setq key (list (+ bits (aref word 0)))))))))
;;; push key on to the result vector 
       (when key (cl-callf vconcat res key))))
    res))

(defun ems-key-tokenize (string)
  "Return vector of tokens."
  (let ((case-fold-search nil)
        (len (length string)) ; We won't alter string in the loop below.
        (pos 0)
        (res []))
    (while ;;; tokenize by white-space into vector res
        (and (< pos len)
             (string-match "[^ \t\n\f]+" string pos)) ;tokenizer  test
      (let* ((word-beg (match-beginning 0))
             (word-end (match-end 0))
             (word (substring string word-beg len))
             key)
        ;; Try to catch events of the form "<as df>".
        (cond
;;; pattern: <xx+>
         ((string-match "\\`<[^ <>\t\n\f][^>\t\n\f]*>" word)
          (setq word (match-string 0 word)
                pos (+ word-beg (match-end 0))))
         (t ;;; plain word, no <>
          (setq word (substring string word-beg word-end)
                pos word-end)))
        ;;; End of tokenizing logic 
        (cond ;;; modifier + keys 
         ((and
           (string-match "^\\(\\([ACHMsS]-\\)*\\)<\\(.+\\)>$" word)
           (progn
             (setq word
                   (concat (substring word (match-beginning 1) (match-end 1))
                           (substring word (match-beginning 3) (match-end 3))))
             (not
              (string-match "\\<\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\)$"
                            word))))
          (setq key (list (intern word))))
         (t
          (let ((orig-word word)
                (prefix 0)
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
            (let ((found
                   (assoc word
                          '(("NUL" . "\0")
                            ("RET" . "\r")
                            ("LFD" . "\n")
                            ("TAB" . "\t")
                            ("ESC" . "\e")
                            ("SPC" . " ")
                            ("DEL" . "\177")))))
              (when found (setq word (cdr found))))
            (when (string-match "^\\\\[0-7]+$" word) ;;; octals
              (cl-loop for ch across word
                       for n = 0 then (+ (* n 8) ch -48)
                       finally do (setq word (vector n))))
            (cond ;;; apply modifiers 
             ((= bits 0) (setq key word))
             ((and (= bits ?\M-\^@)
                   (stringp word)
                   (string-match "^-?[0-9]+$" word))
              (setq key
                    (cl-loop
                     for x across word
                     collect (+ x bits))))
             ((/= (length word) 1)
              (error "%s must prefix a single character, not %s"
                     (substring orig-word 0 prefix) word))
             ((and
               (/= (logand bits ?\C-\^@) 0)
               (stringp word)
               (string-match "[@-_a-z]" word))
              (setq key
                    (list (+ bits (- ?\C-\^@)
                             (logand (aref word 0) 31)))))
             (t (setq key (list (+ bits (aref word 0)))))))))
;;; push key on to the result vector 
        (when key (cl-callf vconcat res key))))
    res))

(defun new-kbd (string )
  "Simplified and hopefully more robust kbd function."
  (let ((res (ems-key-tokenize string)))
;;; events now in vector res, now validate it
    (cond
     ((and
       (cl-loop
        for ch across res
        always
        (and
         (characterp ch)
         (let ((ch2 (logand ch (lognot ?\M-\^@))))
           (and (>= ch2 0) (<= ch2 127))))))
      (concat
       (cl-loop
        for ch across res
        collect
        (cond
         ((= (logand ch ?\M-\^@) 0) ;;; no meta bit
          ch)
         (t (+ ch 128))))))
     (t res))))

;;; Tests:

(let ((tests 
       '(
         "H-DEL"
         "A-SPC"
         "return"
         "DEL"
         "ESC m"
         "ESC C-a"
         "<f1> SPC"
         "<f1> <f2> TAB"
         "<f1> RET"
         "[f1]"
         "<f1>"
         "<F2>"
         "C-x" 
         "C-x C-f" 
         "C-x 4 C-f" 
         "X" 
         "RET" 
         "C-c SPC" 
         "<f1> SPC" 
         "C-M-<down>" 

         "TAB"
         "SPC"
         "RET"
         "C-SPC"
         "C-TAB"
         "C-RET"
         "M-SPC"
         "M-RET"
         "M-TAB"
         "C-c c"
         "S-a"
         "s-a"
         "H-a"
         "C-;"
         "A-a"
         "M-C-a"
         "S-H-a"
         "A-H-a"))
      (result nil))
  (setq result
        (cl-loop
         for test in tests
         unless (equal (new-kbd test) (kbd test))
         collect test))
  (cond
   ((null result) (message "All tests passed."))
   (t (message "%s tests failed." (length result))
      result)))



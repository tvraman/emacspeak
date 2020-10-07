(require 'cl-lib)

(defun ems-key-tokenize (string)
  "Return vector of tokens."
  (let ((res []))
    (cl-loop
     for piece in (split-string string)
     do
       (let* ((word-beg 0)
              (key nil))
         (when
             (string-match "\\`<[^<>[:space:]][^>[:space]]*>" piece)
           (setq piece  (match-string 0 piece)))
         (cond ;;; modifier + keys 
          ((and
            (string-match "^\\(\\([ACHMsS]-\\)*\\)<\\(.+\\)>$" piece)
            (progn
              (setq piece
                    (concat (substring piece (match-beginning 1) (match-end 1))
                            (substring piece (match-beginning 3) (match-end 3))))
              (not
               (string-match "\\<\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\)$"
                             piece))))
           (setq key (list (intern piece))))
          (t
           (let ((orig-word piece)
                 (prefix 0)
                 (bits 0))
             (while ;;; calculate modifier bits
                 (string-match "^[ACHMsS]-." piece)
               (cl-incf bits
                        (cdr
                         (assq (aref piece 0)
                               '((?A . ?\A-\^@)
                                 (?C . ?\C-\^@)
                                 (?H . ?\H-\^@)
                                 (?M . ?\M-\^@)
                                 (?s . ?\s-\^@)
                                 (?S . ?\S-\^@)))))
               (cl-incf prefix 2)
               (cl-callf substring piece 2))
             (when (string-match "^\\^.$" piece)
               (cl-incf bits ?\C-\^@)
               (cl-incf prefix)
               (cl-callf substring piece 1))
             (let ((found
                    (assoc piece
                           '(("NUL" . "\0")
                             ("RET" . "\r")
                             ("LFD" . "\n")
                             ("TAB" . "\t")
                             ("ESC" . "\e")
                             ("SPC" . " ")
                             ("DEL" . "\177")))))
               (when found (setq piece (cdr found))))
             (when (string-match "^\\\\[0-7]+$" piece) ;;; octals
               (cl-loop for ch across piece
                        for n = 0 then (+ (* n 8) ch -48)
                        finally do (setq piece (vector n))))
             (cond ;;; apply modifiers 
              ((= bits 0) (setq key piece))
              ((and (= bits ?\M-\^@)
                    (stringp piece)
                    (string-match "^-?[0-9]+$" piece))
               (setq key
                     (cl-loop
                      for x across piece
                      collect (+ x bits))))
              ((/= (length piece) 1)
               (error "%s must prefix a single character, not %s"
                      (substring orig-word 0 prefix) piece))
              ((and
                (/= (logand bits ?\C-\^@) 0)
                (stringp piece)
                (string-match "[@-_a-z]" piece))
               (setq key
                     (list (+ bits (- ?\C-\^@)
                              (logand (aref piece 0) 31)))))
              (t (setq key (list (+ bits (aref piece 0)))))))))
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



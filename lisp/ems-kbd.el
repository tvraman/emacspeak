(defun ems-kbd (string &optional need-vector)
  (let ((case-fold-search nil)
	(len (length string)) ; We won't alter string in the loop below.
	(pos 0)
	(res []))
    (while (and (< pos len)
		(string-match "[^ \t\n\f]+" string pos))
      (let* ((word-beg (match-beginning 0))
	     (word-end (match-end 0))
	     (word (substring string word-beg len))
	     (times 1)
	     key)
	;; Try to catch events of the form "<as df>".
	(if (string-match "\\`<[^ <>\t\n\f][^>\t\n\f]*>" word)
	    (setq word (match-string 0 word)
		  pos (+ word-beg (match-end 0)))
	  (setq word (substring string word-beg word-end)
		pos word-end))
	(when (string-match "\\([0-9]+\\)\\*." word)
	  (setq times (string-to-number (substring word 0 (match-end 1))))
	  (setq word (substring word (1+ (match-end 1)))))
	(cond ((string-match "^<<.+>>$" word)
	       (setq key (vconcat (if (eq (key-binding [?\M-x])
					  'execute-extended-command)
				      [?\M-x]
				    (or (car (where-is-internal
					      'execute-extended-command))
					[?\M-x]))
				  (substring word 2 -2) "\r")))
	      ((and (string-match "^\\(\\([ACHMsS]-\\)*\\)<\\(.+\\)>$" word)
		    (progn
		      (setq word (concat (substring word (match-beginning 1)
						    (match-end 1))
					 (substring word (match-beginning 3)
						    (match-end 3))))
		      (not (string-match
			    "\\<\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\)$"
			    word))))
	       (setq key (list (intern word))))
	      ((or (equal word "REM") (string-match "^;;" word))
	       (setq pos (string-match "$" string pos)))
	      (t
	       (let ((orig-word word) (prefix 0) (bits 0))
		 (while (string-match "^[ACHMsS]-." word)
		   (cl-incf bits (cdr (assq (aref word 0)
					 '((?A . ?\A-\^@) (?C . ?\C-\^@)
					   (?H . ?\H-\^@) (?M . ?\M-\^@)
					   (?s . ?\s-\^@) (?S . ?\S-\^@)))))
		   (cl-incf prefix 2)
		   (cl-callf substring word 2))
		 (when (string-match "^\\^.$" word)
		   (cl-incf bits ?\C-\^@)
		   (cl-incf prefix)
		   (cl-callf substring word 1))
		 (let ((found (assoc word '(("NUL" . "\0") ("RET" . "\r")
					    ("LFD" . "\n") ("TAB" . "\t")
					    ("ESC" . "\e") ("SPC" . " ")
					    ("DEL" . "\177")))))
		   (when found (setq word (cdr found))))
		 (when (string-match "^\\\\[0-7]+$" word)
		   (cl-loop for ch across word
                            for n = 0 then (+ (* n 8) ch -48)
                            finally do (setq word (vector n))))
		 (cond ((= bits 0)
			(setq key word))
		       ((and (= bits ?\M-\^@) (stringp word)
			     (string-match "^-?[0-9]+$" word))
			(setq key (cl-loop for x across word
                                           collect (+ x bits))))
		       ((/= (length word) 1)
			(error "%s must prefix a single character, not %s"
			       (substring orig-word 0 prefix) word))
		       ((and (/= (logand bits ?\C-\^@) 0) (stringp word)
			     ;; We used to accept . and ? here,
			     ;; but . is simply wrong,
			     ;; and C-? is not used (we use DEL instead).
			     (string-match "[@-_a-z]" word))
			(setq key (list (+ bits (- ?\C-\^@)
					   (logand (aref word 0) 31)))))
		       (t
			(setq key (list (+ bits (aref word 0)))))))))
	(when key
	  (cl-loop repeat times do (cl-callf vconcat res key)))))
    (when (and (>= (length res) 4)
	       (eq (aref res 0) ?\C-x)
	       (eq (aref res 1) ?\()
	       (eq (aref res (- (length res) 2)) ?\C-x)
	       (eq (aref res (- (length res) 1)) ?\)))
      (setq res (cl-subseq res 2 -2)))
    (if (and (not need-vector)
	     (cl-loop for ch across res
                      always (and (characterp ch)
                                  (let ((ch2 (logand ch (lognot ?\M-\^@))))
                                    (and (>= ch2 0) (<= ch2 127))))))
	(concat (cl-loop for ch across res
                         collect (if (= (logand ch ?\M-\^@) 0)
                                     ch (+ ch 128))))
      res)))
(setq ems-kbd-tests
      '(
        "C-c c"
        "M-C-a"
        "A-a"
        ))

(cl-loop
 for test in ems-kbd-tests
 unless (equal (ems-kbd test) (kbd test))
 collect test)

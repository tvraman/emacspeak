(require 'cl-lib)
(eval-when-compile (require 'subr-x))

;;; See[[info:elisp#Key Sequences][info:elisp#Key Sequences]]

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

;;; Tests:

(let ((tests 
       '(
         "<F2>"
         "<f1> <f2> TAB"
         "<f1> RET"
         "<f1> SPC"
         "<f1>"
         "A-H-a"
         "A-SPC"
         "A-TAB"
         "A-a"
         "C-;"
         "C-M-<down>"
         "s-SPC"
         "C-RET"
         "C-SPC"
         "C-TAB"
         "C-c SPC" 
         "C-c TAB"
         "C-c c"
         "C-x 4 C-f" 
         "C-x C-f"
         "C-x" 
         "DEL"
         "ESC C-a"
         "ESC m"
         "H-DEL"
         "H-a"
         "LFD"
         "M-C-a"
         "M-RET"
         "M-SPC"
         "M-TAB"
         "RET"
         "S-H-a"
         "S-a"
         "SPC"
         "X" 
         "[f1]"
         "\177"
         "return"
         "s-a"))
      (result nil))
  (setq result
        (cl-loop
         for test in tests
         unless (equal (new-kbd test) (edmacro-parse-keys test 'need-vector))
         collect test))
  (cond
   ((null result) (message "All tests passed."))
   (t (message "%s tests failed." (length result))
      result)))



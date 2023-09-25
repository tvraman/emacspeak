;;{{{ems-kbd: replacement for function kbd 
;; no longer used. Archived here.
;; simplified kbd function:
;; Uses split-string to  simplify tokenizer.

(defvar ems--kbd-char-table
  '(
    ("NUL" . "\0")
    ("RET" . "\r")
    ("LFD" . "\n")
    ("TAB" . "\t")
    ("ESC" . "\e")
    ("SPC" . " ")
    ("DEL" . "\177"))
  "Map  kbd-char-names to char-values.")

(defvar ems--kbd-mod-table
  '(
    (?A . ?\A-\^@)
    (?C . ?\C-\^@)
    (?H . ?\H-\^@)
    (?M . ?\M-\^@)
    (?s . ?\s-\^@)
    (?S . ?\S-\^@))
  "Map modifier names to modifier bit-values.")

(defun emskbd (string )
  "Like function kbd, but returns a vector."
  (cl-declare (special ems--kbd-mod-table ems--kbd-char-table))
  (let ((res [])
        (mod+char "^[ACHMsS]-.")
        (mod+angle-reg "^\\(\\([ACHMsS]-\\)*\\)<\\(.+\\)>$"))
    (cl-loop
     for word in (split-string string) do
     (let ((key nil))
       (cond
        ((string-match mod+angle-reg word)  ;;; modifier+-<key>
         (setq key
               (list
                (intern
                 (concat ;;; strip < and >
                  (substring word (match-beginning 1) (match-end 1))
                  (substring word (match-beginning 3) (match-end 3)))))))
        (t
         (let ((prefix 0)
               (bits 0))
           (while (string-match mod+char word) ;;; calculate modifier bits
             (cl-incf bits
                      (cdr (assq (aref word 0) ems--kbd-mod-table)))
             (cl-incf prefix 2) ;;; strip modifier
             (cl-callf substring word 2)) ;;; end while modifiers
           (when-let (c (assoc word ems--kbd-char-table)) (setq word (cdr c)))
           (cond ;;; apply modifiers
            ((= bits 0) (setq key word)) ;;; no modifier bits
            ((/= (length word) 1)
             (error "%s: Prefix must precede a character, not %s"
                    string word))
            ((and
              (/= (logand bits ?\C-\^@) 0)
              (string-match "^[@-_a-z]" word)) ;;; ascii control char
             (setq key ;;; C-a is 1 etc.
                   (list (+ bits (- ?\C-\^@)
                            (logand (aref word 0) 31)))))
            (t (setq key (list (+ bits (aref word 0)))))))))
       ;; push key on to the result vector
       (when key (cl-callf vconcat res key))))
    res))

;;}}}

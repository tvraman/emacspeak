;;Author: Qianli Liao
;;I do not know how to put GNU lisence or something like that. Please teach me.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;eval-buffer;;;;;;;;;;;;;;;;;
(defun emacspeak-eval-buffer ()
  "gives speech feedback for eval-buffer, done by  comparing the last several lines of message to see if there is a new error message"
  (interactive) 
  (let ((message-lines-before-eval (emacspeak-get-last-several-lines-of-message)))
    (unwind-protect
	(progn (eval-buffer))
      ;;seems necessary to execute with idle timer, or it will not speak the result 
      (run-with-idle-timer 0.02 nil 'emacspeak-speak-eval-buffer-result message-lines-before-eval)
      )
    )
  )

;;emacspeak-speak-eval-buffer-result: it maybe neat to make this function to lambda , but this is already working fine 
(defun emacspeak-speak-eval-buffer-result (message-lines-before-eval)
  "its argument is the last several lines of message before buffer evaluation, this function check the change of message buffer to determine whether the evaluation is success." 
  (if  (equal message-lines-before-eval (emacspeak-get-last-several-lines-of-message))
      (message "Buffer Evaluated successfully")
    (emacspeak-speak-last-eval-buffer-error)
    
    )
  )

(defun  emacspeak-speak-last-eval-buffer-error()
  "speak the last error message of message buffer"
  (interactive)
  (with-current-buffer  "*Messages*"
    (end-of-buffer)
    (skip-syntax-backward (concat " " (char-to-string   (char-syntax ?\n))))
    (setq speak-end (point))
    (move-beginning-of-line 1)
    (search-forward ":" (point-at-eol) t)
    (emacspeak-speak-region (point) speak-end)
    )
  )

(defun  emacspeak-get-last-message()
  "return the last line in message as a string"
  (interactive)
  (with-current-buffer  "*Messages*"
    (end-of-buffer)
    (skip-syntax-backward (concat " " (char-to-string   (char-syntax ?\n))))
    (setq speak-end (point))
    (move-beginning-of-line 1)
    (setq last-message (buffer-substring (point) speak-end))
    )
  last-message
  )

(defun  emacspeak-get-last-several-lines-of-message()
  "get last several lines of message buffer as a string"
  (with-current-buffer  "*Messages*"
    (end-of-buffer)
    (setq speak-end (point))
    ;;the number of lines depend on this following code
    (move-beginning-of-line -2)
    (buffer-substring-no-properties (point) speak-end)
    )
  )

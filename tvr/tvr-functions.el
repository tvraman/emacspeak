


(defun my-thanks-mail-signature()
  "insert thanks , --Raman at the end of mail message"
  (interactive)
  (goto-char (point-max))
  (insert
   (format "\n Thanks, \n --%s\n" (user-full-name))))

(defun tex-tie-current-word(n)
  (interactive "P")
  "Tie the next n  words."
  (or n (setq n 1))
  (while
      (> n 0)
    (setq n (- n 1))
    (forward-word 1)
    (delete-horizontal-space)
    (insert-char 126 1)
    )
  (forward-word 1))

(defun end-of-word(arg)
  "move to end of word"
  (interactive "P")
  (if arg
      (forward-word arg)
    (forward-word 1)))

(defun comma-at-end-of-word()
  (interactive)
  "Move point to end of word and put a comma."
  (forward-word 1)
  (insert-char
   (string-to-char ",") 1))

(defun lacheck-buffer-file()
  (interactive)
  "Lacheck file visited in current buffer"
  (compile (format "lacheck %s"
                   (buffer-file-name (current-buffer)))))

(defun next-interactive-defun ()
  "Move point to the next interactive defun"
  (interactive)
  (end-of-defun)
  (re-search-forward "^ *(interactive")
  (beginning-of-defun)
  (emacspeak-speak-line))



(provide 'tvr-functions)

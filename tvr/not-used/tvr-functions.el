


(defun my-thanks-mail-signature()
  "insert thanks , --Raman at the end of mail message"
  (interactive)
  (goto-char (point-max))
  (insert
   (format "\n Thanks, \n --%s\n" (user-full-name))))

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

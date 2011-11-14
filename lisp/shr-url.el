(require 'shr)

(defun shr-url-callback (args)
  "Callback for url-retrieve."
  (goto-char (point-min))
  (let* ((buffer (get-buffer-create "Web View"))
         (start (re-search-forward "^$"))
         (dom (libxml-parse-html-region start(point-max))))
    (with-current-buffer buffer
      (erase-buffer)
    (shr-insert-document dom))
    (switch-to-buffer buffer)))    

(defun shr-url (url)
  "Display web page."
  (interactive "sURL: ")
  (url-retrieve url 'shr-url-callback))


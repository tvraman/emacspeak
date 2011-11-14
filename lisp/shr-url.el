(require 'shr)

a(defun shr-url-callback (args)
  "Callback for url-retrieve."
  (goto-char (point-min))
  (let* ((start (re-search-forward "^$"))
         (dom (libxml-parse-html-region start(point-max)))
         (buffer (get-buffer-create (shr-get-title-from-dom dom))))
    (with-current-buffer buffer
      (erase-buffer)
    (shr-insert-document dom)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t))
    (switch-to-buffer buffer)
    (emacspeak-speak-mode-line)))    

(defsubst shr-get-title-from-dom (dom)
  "Return Title."
  (third
   (find-if #'(lambda (e) (and (listp e)(eq 'title (car e))))
            (third dom))))

(defun shr-url (url)
  "Display web page."
  (interactive
   (list
    (read-from-minibuffer "URL: "
                          (get-text-property (point) 'shr-url))))
  (url-retrieve url 'shr-url-callback))


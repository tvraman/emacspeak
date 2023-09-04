(autoload 'org-read-date "org")

(defun magit-org-read-date (prompt &optional _default)
  (org-read-date 'with-time nil nil prompt))

(magit-define-popup-option 'magit-log-popup
  ?s "Since date" "--since=" #'magit-org-read-date)

(magit-define-popup-option 'magit-log-popup
  ?u "Until date" "--until=" #'magit-org-read-date)

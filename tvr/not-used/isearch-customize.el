;; -*- lexical-binding: nil; -*-

(define-key isearch-mode-map [(control return)]
  #'isearch-exit-other-end)

(defun isearch-exit-other-end ()
  "Exit isearch, at the opposite end of the string."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))

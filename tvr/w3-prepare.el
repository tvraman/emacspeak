;;;$Id$  -*- lexical-binding: t; -*-
;;; Compat:
(defalias 'string-to-int 'string-to-number)

(augment-load-path "w3/lisp" "w3")
(load-library "url")
(load-library "w3")
;;{{{  keybindings

(global-set-key "\C-x\C-m" 'w3-fetch)
(global-set-key "\C-x\C-l" 'w3-open-local)
;(global-set-key  "\C-x\C-h" 'w3-use-hotlist )
(define-key w3-mode-map "\M-m" 'back-to-indentation)
(define-key w3-mode-map "*" 'emacspeak-org-bookmark)
(define-key w3-mode-map "8" 'emacspeak-org-bookmark)
(define-key w3-mode-map "\C-d" 'w3-wget)
;;}}}
(emacspeak-w3-toggle-table-borders)
(emacspeak-w3-toggle-table-borders)
(defun w3-add-toolbar-to-buffer (&rest ignore) nil)

;;; style settings:
(setq w3-default-stylesheet "~/.w3/default.css"
w3-honor-stylesheets nil
w3-user-colors-take-precedence t
w3-user-fonts-take-precedence t)

(provide 'w3-prepare)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
(defun url-basepath (file &optional x)
  "Return the base pathname of FILE, or the actual filename if X is true."
  (cond
   ((null file) "")
   ((string-match (eval-when-compile (regexp-quote "?")) file)
    (if x
	(file-name-nondirectory (substring file 0 (match-beginning 0)))
      (file-name-directory (substring file 0 (match-beginning 0)))))
   (x (file-name-nondirectory file))
   (t (file-name-directory file))))
(setq w3-explicit-coding-system 'utf-8)
;;; try this for a while:
(defadvice w3-http-equiv-headers (around skip pre act comp) t)

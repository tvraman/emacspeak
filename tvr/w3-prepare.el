;;;$Id:$

(augment-load-path "w3/lisp" "w3")
(load-library "url")
(setq url-mime-encoding-string nil)
(load-library "w3")
(defvar font-running-xemacs nil)
;;{{{  keybindings

(global-set-key "\C-x\C-m" 'w3-fetch)
(global-set-key "\C-x\C-l" 'w3-open-local)
(global-set-key  "\C-x\C-h" 'w3-use-hotlist )
(define-key w3-mode-map "\M-m" 'back-to-indentation)
(define-key w3-mode-map "\C-d" 'w3-wget)
;;}}}
;;{{{ ssl
;;; customized via custom
;; (setq ssl-program-name "essl"
;;       ssl-program-arguments '("s_client" "-quiet" "-host" host "-port"
;;                               service
;;                               "-verify" (int-to-string ssl-certificate-verification-policy)
;;                               "-CApath" ssl-certificate-directory))
 ;;}}}
(emacspeak-w3-toggle-table-borders)
(emacspeak-w3-toggle-table-borders)
(defun w3-add-toolbar-to-buffer (&rest ignore) nil)
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

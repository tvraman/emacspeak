;;;$Id$

(augment-load-path "w3/lisp" "w3")
(load-library "w3")
(defvar font-running-xemacs nil)
;;{{{  keybindings

(global-set-key "\C-x\C-m" 'w3-fetch)
(global-set-key "\C-x\C-l" 'w3-open-local)
(global-set-key  "\C-x\C-h" 'w3-use-hotlist )
(define-key w3-mode-map "\M-m" 'back-to-indentation)
;;}}}
;;{{{ ssl
;;; customized via custom
;; (setq ssl-program-name "essl"
;;       ssl-program-arguments '("s_client" "-quiet" "-host" host "-port"
;;                               service
;;                               "-verify" (int-to-string ssl-certificate-verification-policy)
;;                               "-CApath" ssl-certificate-directory))
 ;;}}}

(provide 'w3-prepare)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}

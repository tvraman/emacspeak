(with-eval-after-load "slime"
  (cl-declaim (special slime-lisp-implementations
                       slime-use-autodoc-mode slime-prefix-map slime-doc-map
                       common-lisp-hyperspec-root inferior-lisp-program))
  (add-hook 'slime-repl-mode-hook 'lispy-mode)
  (define-key slime-prefix-map "d" slime-doc-map)
  (setq inferior-lisp-program (executable-find "sbcl"))
  (setq common-lisp-hyperspec-root
        (if (file-exists-p "/usr/share/doc/hyperspec/")
            "file:///usr/share/doc/hyperspec/"
          "http://www.lispworks.com/reference/HyperSpec/"))
  (global-set-key (kbd "C-c s") 'slime-selector)
  (setq slime-contribs '(slime-fancy slime-hyperdoc slime-quicklisp slime-asdf))
  (slime-setup)
  (slime-autodoc--disable)
  (setq slime-use-autodoc-mode nil)
  (setq
   slime-lisp-implementations
   `((sbcl ("sbcl" "--core"
            ,(expand-file-name "sbcl.core-for-slime" user-emacs-directory))))))

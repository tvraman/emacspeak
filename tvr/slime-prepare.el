(with-eval-after-load "slime"
  (setq inferior-lisp-program (executable-find "sbcl"))
  (setq common-lisp-hyperspec-root
        (if (file-exists-p "/usr/share/doc/hyperspec/")
            "file:///usr/share/doc/hyperspec/"
          "http://www.lispworks.com/reference/HyperSpec/"))
  (global-set-key (ems-kbd "C-c s") 'slime-selector)
  (setq slime-contribs '(slime-fancy slime-hyperdoc slime-quicklisp))
  (slime-setup)
  (slime-autodoc--disable)
  (setq slime-use-autodoc-mode nil)
  (setq
   slime-lisp-implementations
   `((sbcl ("sbcl" "--core"
            ,(expand-file-name "sbcl-core-for-slime" user-emacs-directory))))))

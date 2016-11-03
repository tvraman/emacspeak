;; -*- lexical-binding: t; -*-
(augment-load-path "slime" "slime")
(load-library "slime-autoloads")
(require 'slime)
(setq inferior-lisp-program (executable-find "clisp"))
(setq common-lisp-hyperspec-root
      (if (file-exists-p "/usr/share/doc/hyperspec/")
          "file:///usr/share/doc/hyperspec/"
          "http://www.lispworks.com/reference/HyperSpec/"))
(global-set-key (kbd "C-c s") 'slime-selector)
(setq slime-contribs '(slime-fancy slime-hyperdoc slime-quicklisp))
(slime-setup)
(slime-autodoc--disable)

(augment-load-path "slime" "slime")
(augment-load-path "slime/contrib" "slime-repl")
(require 'slime)
(require 'slime-scratch)
(require 'slime-repl)
(setq inferior-lisp-program (executable-find "clisp"))

(setq common-lisp-hyperspec-root
      (if (file-exists-p "/usr/share/doc/hyperspec/")
          "file:///usr/share/doc/hyperspec/"
          "http://www.lispworks.com/reference/HyperSpec/"))
(global-set-key "\C-cs" 'slime-selector)

;;; Do this at the end:
;(setq slime-contribs '(slime-fancy))
(slime-setup)

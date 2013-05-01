(augment-load-path  "semantic" "semantic")
(augment-load-path "cedet" "cedet")

(setq imenu-create-index-function
      'imenu-default-create-index-function)
(semantic-mode 1)

(add-hook 'speedbar-load-hook (lambda () (require 'semantic-sb)))

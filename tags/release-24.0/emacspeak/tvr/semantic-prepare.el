(augment-load-path  "semantic" "semantic")

(load-library "semantic-load")
(setq imenu-create-index-function
      'imenu-default-create-index-function)
(add-hook 'semantic-init-hooks
          (lambda ()
            (senator-minor-mode 1)))

(global-semanticdb-minor-mode 1)

(add-hook 'speedbar-load-hook (lambda () (require 'semantic-sb)))

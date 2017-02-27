(augment-load-path "yasnippet" "yasnippet")
(load-library "yasnippet")
(setq yas/root-directory
      (expand-file-name "yasnippet/snippets"
                        emacs-personal-library))
(yas/initialize)
(yas/load-directory yas/root-directory)

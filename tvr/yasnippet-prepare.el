(setq yas-dir (augment-load-path "yasnippet" "yasnippet"))
(load-library "yasnippet")
(when yas-dir 
(setq yas/root-directory
      (expand-file-name "snippets"
                        (file-name-directory yas-dir))))
(yas/initialize)
(yas/load-directory yas/root-directory)
(yas/initialize)
(yas/reload-all)

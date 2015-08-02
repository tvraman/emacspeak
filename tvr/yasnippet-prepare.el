(setq yas-dir (augment-load-path "yasnippet" "yasnippet"))
(load-library "yasnippet")
(when yas-dir 
(setq yas-snippet-dirs
      (expand-file-name "snippets"
                        (file-name-directory yas-dir))))
(yas/initialize)
(yas/load-directory yas/root-directory)
(yas/initialize)
(yas/reload-all)
(yas-global-mode)

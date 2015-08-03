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
(add-hook
 'prog-mode-hook
 #'(lambda nil
     (yas-minor-mode 1)))
;;; Silence yasnippet:
(setq yas-verbosity 0)

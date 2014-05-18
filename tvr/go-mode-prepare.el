(augment-load-path "go-mode.el" "go-mode")
(when (featurep 'yasnippet)
  (add-to-list
   'yas-snippet-dirs
   (expand-file-name "yasnippet-go" emacs-personal-library)))
                                 



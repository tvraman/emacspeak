(require 'my-functions)
(augment-load-path "dmacro")
(autoload 'insert-dmacro "dmacro" 
  "Dynamic macros" t)
(add-hook 'after-load-alist
          '("dmacro"
            (dmacro-load (expand-file-name "dmacro/raman.dm" emacs-personal-library-directory))))

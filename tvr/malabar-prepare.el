(augment-load-path "malabar-mode/lisp" "malabar-mode")

(setq semantic-default-submodes
      '(global-semantic-idle-scheduler-mode
        global-semanticdb-minor-mode
        global-semantic-idle-summary-mode
        global-semantic-mru-bookmark-mode))
(semantic-mode 1)
(require 'malabar-mode)
(setq malabar-groovy-lib-dir
      (expand-file-name "malabar-mode/lib" (file-name-directory load-file-name)))
        (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))





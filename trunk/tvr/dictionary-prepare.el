(augment-load-path "dictionary" "dictionary")

(global-set-key "\C-c/" 'dictionary-prefix-command)
(load-library "dictionary")
(define-key dictionary-mode-map "\C-m" 'link-selected)

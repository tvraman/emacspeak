;; -*- lexical-binding: nil; -*-
(augment-load-path "haskell-mode" "haskell-site-file")

    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; -*- lexical-binding: t; -*-
(augment-load-path "scala-mode" "scala-mode")
(load-library "scala-mode")
(augment-auto-mode-alist
 ".scala$"
 'scala-mode)

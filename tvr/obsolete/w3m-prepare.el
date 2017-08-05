;; -*- lexical-binding: t; -*-
(augment-load-path "w3m" "w3m")
(load-library "w3m")
(defvar coding-category-utf-16-be nil)
(defvar coding-category-utf-16-le nil)
(defvar coding-category-utf-8 nil)
(require 'w3m)
(setq w3m-coding-system 'utf-8
          w3m-file-coding-system 'utf-8
          w3m-file-name-coding-system 'utf-8
          w3m-input-coding-system 'utf-8
          w3m-output-coding-system 'utf-8
          w3m-terminal-coding-system 'utf-8)

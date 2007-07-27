(load-library "javascript")
(load-library "moz")
(augment-auto-mode-alist  ".js$" 'javascript-mode)
(add-hook 'js-mode-hook 'moz-minor-mode)
(fold-add-to-marks-list 'js-mode
                        "// <" "// >" "")
(augment-load-path "selenemacs" "selenium")
(load-library "selenemacs")
(defalias 'javascript-generic-mode 'javascript-mode)
(add-hook 'javascript-mode-hook
          'moz-minor-mode)

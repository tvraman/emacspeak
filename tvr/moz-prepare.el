(load-library "js-mode")
(load-library "moz")
(augment-auto-mode-alist  ".js$" 'js-mode)
(add-hook 'js-mode-hook 'moz-minor-mode)
(fold-add-to-marks-list 'js-mode
                        "// <" "// >" "")


(defalias 'javascript-generic-mode 'js-mode)
(add-hook 'javascript-mode-hook
          'moz-minor-mode)

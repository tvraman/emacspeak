;;;$Id$
(augment-load-path "speedbar" "speedbar")
(load-library "speedbar")
(add-hook 'speedbar-load-hook
          (function
           (lambda nil
             (speedbar-add-supported-extension ".html$")
             (speedbar-add-supported-extension ".py$"))))
(global-set-key '[insert] 'speedbar-get-focus)

(load-library "rpm")

;;;$Id:$  -*- lexical-binding: t; -*-
(augment-load-path "speedbar" )
(load-library "speedbar")
(add-hook 'speedbar-load-hook
          (function
           (lambda nil
             (speedbar-add-supported-extension ".html$")
             (speedbar-add-supported-extension ".py$"))))
;(load-library "rpm")

(augment-load-path "xslt-process/lisp")
(autoload 'xslt-process-install-docbook "xslt-process"
       "Register the DocBook package with XSLT-process" t)
(autoload 'xslt-process-mode "xslt-process"
  "Run XSLT processor on buffer" t)

(add-hook 'xml-mode-hook 'xslt-process-mode)

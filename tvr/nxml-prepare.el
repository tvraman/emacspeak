(augment-load-path "nxml-mode/" "nxml-mode")
(load-library "rng-auto")
(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|html\\)\\'" . nxml-mode)
            auto-mode-alist))

(defalias 'xml-mode 'nxml-mode)
(defalias 'xhtml-mode 'nxml-mode)
(defalias 'html-mode 'nxml-mode)
(defalias 'sgml-mode 'nxml-mode)

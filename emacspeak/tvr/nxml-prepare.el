;;;$Id$
(augment-load-path "nxml-mode" "nxml-mode")
(load-library "rng-auto")
(setq auto-mode-alist
        (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|html\\)\\'" . nxml-mode)
	      auto-mode-alist))

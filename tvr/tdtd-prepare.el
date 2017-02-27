;; $Id$

;; DTD mode
(augment-load-path "tdtd" "tdtd")
(autoload 'dtd-mode "tdtd" "Major mode for SGML and XML DTDs." t)
(autoload 'dtd-etags "tdtd"
  "Execute etags on FILESPEC and match on DTD-specific regular expressions."
  t)
(autoload 'dtd-grep "tdtd" "Grep for PATTERN in files matching FILESPEC." t)

;; Turn on font lock when in DTD mode
(add-hook 'dtd-mode-hooks
	  'turn-on-font-lock)
(setq auto-mode-alist
      (append
       (list
	'("\\.dcl$" . dtd-mode)
	'("\\.dec$" . dtd-mode)
	'("\\.dtd$" . dtd-mode)
	'("\\.ele$" . dtd-mode)
	'("\\.ent$" . dtd-mode)
	'("\\.mod$" . dtd-mode))
       auto-mode-alist))
(setq-default dtd-xml-flag t)
(setq-default dtd-prompt-descriptive-name nil)

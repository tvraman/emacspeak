;;; Setup iedit keybinding without sacrificing hyper == C-;

(defvar tvr-iedit-prefix (ems-kbd "C-z")
  "Personal iedit prefix key.")

(define-key global-map tvr-iedit-prefix 'iedit-mode)
(define-key isearch-mode-map tvr-iedit-prefix 'iedit-mode-from-isearch)
      (define-key esc-map tvr-iedit-prefix 'iedit-execute-last-modification)
      (define-key help-map tvr-iedit-prefix 'iedit-mode-toggle-on-function)
      (message "Iedit default key binding is %s" (key-description tvr-iedit-prefix))

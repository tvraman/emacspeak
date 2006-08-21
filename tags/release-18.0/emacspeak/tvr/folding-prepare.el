
(load-library "folding")
(declare (special fold-fold-on-startup
                  fold-keys-already-setup
                  fold-keys-already-setup
                  fold-dont-bind-keys
                  fold-inhibit-startup-message
                  ))
    
(setq fold-fold-on-startup nil)
(setq fold-keys-already-setup nil)
(add-hook 'folding-mode-hook
          (function (lambda()
                      (unless fold-keys-already-setup
                        (setq fold-keys-already-setup t)
                        (define-prefix-command 'ctl-f-folding-mode-prefix)
                        (define-key 'ctl-f-folding-mode-prefix "f" 'fold-fold-region)
                        (define-key  'ctl-f-folding-mode-prefix "e" 'fold-enter)
                        (define-key 'ctl-f-folding-mode-prefix "x" 'fold-exit)
                        (define-key  'ctl-f-folding-mode-prefix "b" 'fold-whole-buffer)
                        (define-key 'ctl-f-folding-mode-prefix "o" 'fold-open-buffer)
                        (define-key 'ctl-f-folding-mode-prefix "h" 'fold-hide)
                        (define-key 'ctl-f-folding-mode-prefix "s" 'fold-show)
                        (define-key 'ctl-f-folding-mode-prefix "t" 'fold-top-level)
                        )
                      (local-set-key "\C-f" 'ctl-f-folding-mode-prefix))))

(setq fold-dont-bind-keys t)
(setq fold-inhibit-startup-message t)
(folding-mode-add-find-file-hook)
(fold-add-to-marks-list 'apache-generic-mode
                        "# {{{" 
                        "# }}}" "")
(fold-add-to-marks-list 'xml-mode
                        "<!-- {" 
                        "<!-- } -->" " --> ")
(fold-add-to-marks-list 'sgml-mode
                        "<!-- {" 
                        "<!-- } -->" " --> ")
(fold-add-to-marks-list 'dtd-mode
                        "<!-- {" 
                        "<!-- } -->" " --> ")
(fold-add-to-marks-list 'c-mode "/* <" "/* > */" "*/")
(fold-add-to-marks-list 'c++-mode
                        "//<" "//>" "")
(fold-add-to-marks-list 'LaTeX-mode "%%% {{{" "%%% }}}" " ")
(fold-add-to-marks-list 'latex2e-mode "%%% {{{" "%%% }}}" " ")
(fold-add-to-marks-list 'latex-mode "%%% {{{" "%%% }}}" " ")
(fold-add-to-marks-list 'BibTeX-mode "%%% {{{" "%%% }}}" " ")
(fold-add-to-marks-list 'lisp-mode ";;; {" ";;; }" "")
(fold-add-to-marks-list 'lex-mode" /* {{{ " " /* }}} */ " "*/")
(fold-add-to-marks-list 'html-helper-mode "<!-- { " "<!-- } -->" "-->")
(fold-add-to-marks-list 'tcl-mode "# {{{" "# }}}" " ")
(fold-add-to-marks-list 'sh-mode "# {{{" "# }}}" " ")

(eval-after-load
    "lispy"
  `(progn
     (cl-declare (special lispy-mode-map lispy-mode-map-lispy))
     (define-key lispy-mode-map (kbd "M-m") nil)
     (define-key lispy-mode-map (kbd ";") 'self-insert-command)
     (define-key lispy-mode-map (kbd "C-d") 'delete-char)
     (define-key lispy-mode-map (kbd "M-C-d") 'lispy-delete)
     
     (define-key lispy-mode-map (kbd "M-e") 'lispy-move-end-of-line)
     (define-key lispy-mode-map "a" 'special-lispy-beginning-of-defun)
     (define-key lispy-mode-map "A" 'special-lispy-ace-symbol)
     (define-key lispy-mode-map (kbd "C-,") nil)
     (define-key lispy-mode-map [(control left)] 'lispy-barf)
     (define-key lispy-mode-map [(control right)] 'lispy-slurp)
     (define-key lispy-mode-map-lispy (kbd "C-,") nil)
     (add-hook
      'lispy-mode-hook
      #'(lambda nil
          (setq
           header-line-format
           '((:eval
              (format "%s %s %s"
                      (buffer-name)
                      " Lispy"
                      (if vc-mode
                          (concat
                           vc-mode
                           (format "%s" (vc-state (buffer-file-name))))
                        "")))))))

;;;###autoload
;;;  Lispy for eval-expression:

     (defun conditionally-enable-lispy ()
       (when (memq this-command '(eval-expression emacspeak-wizards-show-eval-result))
         (lispy-mode 1)))
     (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)

     ))

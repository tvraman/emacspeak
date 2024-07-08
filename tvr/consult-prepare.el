(defvar  emacspeak-consult-keymap nil "Emacspeak consult keymap")

(define-prefix-command 'emacspeak-consult-keymap)

(global-set-key (kbd "C-/") 'emacspeak-consult-keymap)

(cl-loop
 for b in
 '(
   ("#" consult-register-load)
   ("'" consult-register-store)
   ("4b" consult-buffer-other-window)
   ("5b" consult-buffer-other-frame)
   (":" consult-complex-command)
   ("B" consult-bookmark)
   ("G" consult-goto-line)
   ("I" consult-info)
   ("K" consult-keep-lines)
   ("L" consult-line-multi)
   ("M" consult-mark)
   ("M-e" consult-isearch-history)
   ("M-x" consult-mode-command)
   ("M-y" consult-yank-pop)
   ("b" consult-buffer)
   ("c" consult-locate)
   ("d" consult-find)
   ("e" consult-compile-error)
   ("h" consult-org-heading)
   ("i" consult-imenu)
   ("k" consult-global-mark)
   ("l" consult-line)
   ("m" consult-man)
   ("o" consult-outline)
   ("p" consult-project-buffer)
   ("r" consult-ripgrep)
   ("u" consult-focus-lines)
   )
 do
 (define-key  emacspeak-consult-keymap (kbd (car b))  (cadr b)))

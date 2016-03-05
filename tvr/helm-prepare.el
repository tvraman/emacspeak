;;; Taken from http://tuhdo.github.io/helm-intro.html

(require 'helm)
(require 'helm-config)

;;; Use hyper-; as the helm prefix 
(global-set-key (kbd "C-x @h;") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)
(global-set-key (kbd "C-x @h;;") 'helm-mode)
(global-set-key (kbd "C-x @h; M-:") 'helm-eval-expression-with-eldoc)
(define-key helm-map (kbd "C-s") 'helm-toggle-suspend-update)

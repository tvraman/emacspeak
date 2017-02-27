(when (locate-library "python-mode-autoloads")
  (load-library "python-mode-autoloads"))

(when (locate-library "ipython-autoloads")
  (load-library "ipython-autoloads"))
(require 'pysmell)
(add-hook 'python-mode-hook (lambda () (pysmell-mode 1)))

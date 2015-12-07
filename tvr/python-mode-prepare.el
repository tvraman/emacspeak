(when (locate-library "python-mode-autoloads")
  (load-library "python-mode-autoloads"))

(when (locate-library "ipython-autoloads")
  (load-library "ipython-autoloads"))

(add-hook
 'python-mode-hook
 #'(lambda ()
                              (when (locate-library "elpy")
                                (elpy-enable))))

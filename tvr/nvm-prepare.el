;;{{{Node/NVM Setup:
(defun tvr-nvm-setup ()
  "Set up NVM/NPM."
  (with-eval-after-load "nvm"
    (let ((v
           (car (sort (mapcar #'car (nvm--installed-versions)) #'string>))))
      (nvm-use v)
      (executable-find "node"))))

(defvar tvr-npm-node
  (tvr-nvm-setup)
  "Find the right Node executable.")

;;}}}

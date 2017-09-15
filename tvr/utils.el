(defun augment-load-path (path &optional library whence at-end)
  "add directory to load path. Path is resolved relative to `whence'
which defaults to emacs-personal-library."
  (interactive "Denter directory name: ")
  (cl-declare (special emacs-personal-library))
  (unless (and library (locate-library library))
    (add-to-list
     'load-path
     (expand-file-name
      path
      (or
       whence
       (and (boundp 'emacs-personal-library) emacs-personal-library)))
     at-end))
  (when library (locate-library library)))

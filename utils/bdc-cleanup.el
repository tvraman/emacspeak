;;; bdc-cleanup.el --- Delete byte-compile-dynamic

(require 'cl-lib)

(defun bdc-cleanup ()
  "Remove byte-compile-dynamic lines."
  (let ((file-list (directory-files default-directory nil "\\.el$")))
    (cl-loop
     for f in file-list do
     (let ((indent-tabs-mode nil))
       (find-file f)
       (flush-lines "byte-compile-dynamic: " (point-min) (point-max))
       (shell-command-on-region (point-min) (point-max)
                                "cat -s" (current-buffer) 'replace)
       (save-buffer)))))

(bdc-cleanup)

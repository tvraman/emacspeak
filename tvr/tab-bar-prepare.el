(defun tab-create (name)
  "Create the NAME tab if it doesn't exist already."
  (condition-case nil
      (unless (equal (alist-get 'name (tab-bar--current-tab)) name)
        (tab-bar-rename-tab-by-name name name))
    (error (tab-new)
           (tab-bar-rename-tab name))))

(defun tvr-tabs ()
  "Set up my tab-bar."
  (tab-create "Books")
  (tab-create "EXWM")
  (tab-create "XTerm"))

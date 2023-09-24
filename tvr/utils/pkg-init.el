(make-thread
     #'(lambda ()
         (let ((file-name-handler-alist nil)
               (load-source-file-function nil))
           (package-initialize))))

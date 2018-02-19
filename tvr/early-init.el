
     
         (let ((file-name-handler-alist nil)
               (gc-cons-threshold 64000000)
               (load-source-file-function nil))
           (package-initialize))

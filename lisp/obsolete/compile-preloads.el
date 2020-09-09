(setq file-name-handler-alist nil
      gc-cons-threshold 64000000
      load-source-file-function  nil)
(let ((message-log-max nil)
      (inhibit-message  nil))
  (mapcar
   #'load-file
   '(
     "../lisp/emacspeak-load-path.el"
     "../lisp/emacspeak-loaddefs.el"
     "../lisp/emacspeak-cus-load.el"
     "../lisp/g-client/g-load-path.el"
     "../lisp/g-client/g-cus-load.el"
     "../lisp/g-client/g-loaddefs.el")))

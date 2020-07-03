(setq file-name-handler-alist nil
      gc-cons-threshold 64000000
      load-source-file-function  nil)
(load-file "../lisp/emacspeak-load-path.el")
(load-file "../lisp/emacspeak-loaddefs.el")
(load-file "../lisp/emacspeak-cus-load.el")
(load-file "../lisp/g-client/g-load-path.el")
(load-file "../lisp/g-client/g-cus-load.el")
(load-file "../lisp/g-client/g-loaddefs.el")

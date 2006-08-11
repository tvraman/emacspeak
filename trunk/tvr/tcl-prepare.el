
(load-library "tcl")(declare (special inferior-tcl-mode-map 
                      tcl-prompt-regexp
                      tcl-help-directory-list))

    

    
    
                                        

    ;; If you plan to use the interface to the TclX help files, you must
    ;; set the variable tcl-help-directory-list to point to the topmost
    ;; directories containing the TclX help files.  Eg:
    ;;

    (autoload 'tcl-help-on-word "tcl" "Help on Tcl commands" t)


    (add-hook 'tcl-mode-hook 'tcl-auto-fill-mode)
    (add-hook 'tcl-mode-hook 'tcl-guess-application)
    (setq tcl-prompt-regexp "[-_a-zA-Z]+> \| [=>]+")
    (add-hook 'inferior-tcl-mode-hook
              (function (lambda  ()
                          (define-key  inferior-tcl-mode-map "\C-c\C-p"
                            'comint-previous-prompt ))))
        

    (setq tcl-help-directory-list
              (list "/usr/lib/tclX8.2/help/tcl"
                    "/usr/lib/tkX8.2/help/tk")
            )
          
    (setq tcl-default-application "tcl")

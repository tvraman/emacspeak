;;;$Id$
;;; Generate Documentation.
;;; this is legacy code  written in 1995
;;; preserved for posterity.
(require 'cl)
;;{{{ Design:

;;; prompt for a file of lisp code.
;;; insert its contents into a temporary buffer, 
;;; Nuke all comments.
;;; Read forms one at a time,
;;; If form is an interactive command, output texinfo documentation.

;;}}}
;;{{{ main function


(defsubst write-documentation (form output)
  "Write documentation for form into buffer"
  (let ((doc nil)
        (key nil)
        (name (second form)))
    (when (and (eq 'defun (first form))
               (commandp name))
      (setq doc (fourth form))
      (setq key (where-is-internal name))
      (if key 
          (condition-case nil
                    (setq key 
                          (format "Key Sequence:%s"
                                (mapconcat
                                 'key-description
                                 key " ")))
                  (error nil)))
      (save-excursion
        (set-buffer output)
        (goto-char (point-max))
        (insert
         (format "@findex  %s\n @kindex %s\n %s\n\n"
                 name 
                 (or key (format "M-x %s" name))
                 doc ))))))

(defun generate-documentation (filename)
  "Generate documentation from file"
  (interactive
   (list (read-file-name "Enter name of Emacs Lisp file to
document: ")))
  (let ((form nil)
        (input (get-buffer-create
                (format "*doc-%s" filename)))
        (output (find-file-noselect
                 (format "%s.texi"
                         (file-name-sans-extension
                          (file-name-nondirectory filename))))))
    (save-excursion
      (set-buffer input)
      (erase-buffer)
      (insert-file filename)
      (flush-lines "^;")
      (flush-lines "^ *$")
      (goto-char (point-min))
      (while(and
             (not (eobp))
             (condition-case nil
                 (setq form (read input))
               (error nil)))
        (write-documentation form output)))
    (save-buffer output)))


(defun write-key-documentation (k)
  "Insert documentation for key K."
  (interactive
   (list
    (read-key-sequence "Key to document:")))
  (write-command-documentation (key-binding k)))
     
(defun write-command-documentation (name)
  "Write documentation for form into buffer"
  (interactive
   (list (read-command "Command to document:")))
  (let ((doc nil)
        (key nil))
    (when (commandp name)
      (setq doc  (documentation name))
      (setq key (where-is-internal name))
      (if key 
          (condition-case nil
              (setq key 
                    (format "%s"
                            (mapconcat
                             'key-description
                             key " ")))
            (error nil)))
      (set-mark (point))
      (insert
       (format "\n@findex  %s\n @kindex %s\n \n"
               name 
               (or key (format "M-x %s" name))))
       (insert (format "@item @kbd{%s}\n%s\n %s\n"
key name doc )))))

(defun write-variable-documentation (name)
  "Write documentation for form into buffer"
  (interactive
   (list (read-variable "User variable  to document:")))
      (insert
       (format "\n@vindex  %s\n%s\n\n"
               name 
               (describe-variable name))))
        
;;}}}
(provide 'gendoc)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}


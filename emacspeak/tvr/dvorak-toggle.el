(defcustom keytables-directory
  "/lib/kbd/keymaps/i386/"
  "Keytables directory"
  :type 'directory)
  

(defvar  dvorak-on nil
  "Records state of keyboard")


(defun dvorak-toggle ()
  "Toggle use of dvorak keymap while I learn it."
  (interactive)
  (declare (special dvorak-on
		    keytables-directory))
  (let ((command
         (format "sudo loadkeys %s"
                 (cond
                  (dvorak-on
                   (expand-file-name
                    "qwerty/emacs2.kmap.gz"
                    keytables-directory))
                  (t
                   (expand-file-name
                    "dvorak/dvorak.kmap.gz"
                    keytables-directory))))))
    (shell-command command)
    (message "Switched to %s"
             (if dvorak-on
                 "dvorak"
               "qwerty")))    
  (setq dvorak-on (not dvorak-on)))

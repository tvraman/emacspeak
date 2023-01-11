;;; Easily start/stop orca for use with Chrome:

(defvar orca-process nil
  "Orca process handle")

(defun orca-toggle ()
  "Toggle state of orca."
  (interactive)
  (cond
    (orca-process (delete-process orca-process)
                  (setq orca-process  nil))
    (t (setq orca-process (start-process "Orca"nil "orca")))))

(global-set-key (kbd "s-o") 'orca-toggle)

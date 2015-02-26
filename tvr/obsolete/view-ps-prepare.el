(require 'cl)
  (augment-load-path "view-process-mode" "view-process-mode")
(autoload 'view-processes "view-process-mode"
     "View Processes"
   t)
(unless (fboundp 'face-height)
  (defun face-width (&rest args ) 1)
  (defun face-height (&rest args) 1))
(global-set-key '[f6] 'view-processes)
;;; on bsd systems use "-jaxww" for job control type listings
(setq View-process-status-command-switches-bsd
      "-auxww")
(setq View-process-status-command-switches-system-v "-leaf")

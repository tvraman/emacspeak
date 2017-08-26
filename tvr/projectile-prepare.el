;;; -*- lexical-binding: nil; -*-
(load-library "projectile-autoloads")
;(projectile-global-mode)
(eval-after-load 'projectile
  `(progn 
(global-set-key (kbd "C-c '") 'projectile-commander)
(global-set-key (kbd "C-c ;") 'projectile-command-map)
(setq 
 projectile-mode-line
 '(:eval 
   (if(not (string= "-"  (projectile-project-name)))
       (format "[%s]" (projectile-project-name))
     "")))))

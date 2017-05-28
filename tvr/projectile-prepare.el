;; -*- lexical-binding: t; -*-
(load-library "projectile")
(projectile-global-mode)
(global-set-key (kbd "C-c ;") 'projectile-command-map)
(csetq 
 projectile-mode-line
 '(:eval 
   (if(not (string= "-"  (projectile-project-name)))
       (format "[%s]" (projectile-project-name))
     "")))

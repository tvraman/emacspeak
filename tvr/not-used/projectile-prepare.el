;;; -*- lexical-binding: nil; -*-

(when (locate-library "projectile")
  (add-hook 'prog-mode-hook 'projectile-mode))
;(projectile-global-mode)
(eval-after-load 'projectile
  `(progn 
(global-set-key (ems-kbd "C-c '") 'projectile-commander)
(global-set-key (ems-kbd "C-c ;") 'projectile-command-map)
(setq 
 projectile-mode-line
 '(:eval 
   (if(not (string= "-"  (projectile-project-name)))
       (format "[%s]" (projectile-project-name))
     "")))))

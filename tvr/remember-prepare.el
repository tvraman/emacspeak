;;; See http://johnwiegley.com/org.mode.day.planner.html
(load-library "remember")
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(setq org-remember-templates
           '(
             (?w "~/org/web.org" "* %?           %U \n   %a"
                 "~/.org/web.org")))

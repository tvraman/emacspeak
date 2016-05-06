(when (locate-library "elscreen")
       (elscreen-start)
       (global-set-key (kbd "C-/") elscreen-map)
       (define-key elscreen-map  (kbd "C-/") 'elscreen-toggle))

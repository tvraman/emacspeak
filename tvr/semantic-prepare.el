(augment-load-path  "semantic" "semantic")
(augment-load-path "cedet" "cedet")
;;; enable  desired semantic minor modes via custom.
(semantic-mode 1)

(add-hook 'speedbar-load-hook (lambda () (require 'semantic-sb)))

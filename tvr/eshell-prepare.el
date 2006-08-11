(augment-load-path "pcomplete" "pcomplete")
(augment-load-path "eshell" "eshell")
;reload comint to fix eshell bug
(load-library "eshell")
(load-library "comint")

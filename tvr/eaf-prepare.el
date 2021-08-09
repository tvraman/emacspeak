(push (expand-file-name "eaf/" tvr-site-lib) load-path)
(load-library "eaf")
(eaf-setq eaf-browser-enable-adblocker "true")
;;; We'll handle pdf, epub etc via Emacspeak as usual --- no EAF
(setq eaf-pdf-extension-list nil)
(global-set-key (ems-kbd "C-; C-f") 'eaf-open-browser)

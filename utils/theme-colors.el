;;; colors-theme.el -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'name-this-color)

(defun tvr-theme-colors (palette)
  "Display colors in  palette"
  (interactive "SPalette:")
  (with-output-to-temp-buffer   "*Colors*"
    (print palette)
    (cl-loop
     for p in  (symbol-value palette) 
     when (stringp (cl-second p)) do
     (prin1 (car p))
     (prin1    (ntc-name-this-color (cl-second p)))
     (terpri))))

(provide 'theme-colors)

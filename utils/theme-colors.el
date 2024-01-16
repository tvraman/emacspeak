;;; colors-theme.el -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'name-this-color)

(defun tvr-theme-colors (palette)
  "Display colors in  palette"
  (interactive
   (list
    (intern
     (completing-read
      "Palette: "
      (cl-loop
       for s being the symbols
       when (string-match ".*palette$" (symbol-name s)) collect s)))))
  (with-output-to-temp-buffer   "*Colors*"
    (print palette)
    (cl-loop
     for p in  (symbol-value palette) 
     when (stringp (cl-second p)) do
     (prin1 (cl-first p))
     (prin1    (ntc-name-this-color (cl-second p)))
     (terpri))))

(provide 'theme-colors)

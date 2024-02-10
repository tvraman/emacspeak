;;; colors-theme.el -*- lexical-binding: t -*-
(require 'cl-lib)


(defun tvr-theme-colors (palette)
  "Display colors in  palette"
  (interactive
   (list
    (intern
     (completing-read
      "Palette: "
      (cl-loop
       for s being the symbols
       when (and (boundp s)
                 (not (functionp s))
                 (string-match ".*palette$" (symbol-name s))) collect
                 s))))) ; done reading input
  (with-current-buffer (get-buffer-create    "*Colors*") ; produce output
    (let ((inhibit-read-only  t))
      (erase-buffer)
      (insert (format "%s\n" palette))
      (cl-loop
       for p in  (symbol-value palette) 
       when (stringp (cl-second p)) do
       (let ((c (ems--color-name (cl-second p))))
         (insert (format "%s:\t%s\t%s\n" (cl-first p) c (cl-second p))))))
    (setq buffer-read-only t)
    (special-mode))
  (emacspeak-icon 'open-object)
  (funcall-interactively #'switch-to-buffer "*Colors*")
  (goto-char (point-min))
  (emacspeak-speak-line))

(declare-function emacspeak-icon "emacspeak-sounds" (icon))
(declare-function ems--color-name "emacspeak-wizards" (color))
(declare-function emacspeak-speak-line "emacspeak-speak" (&optional arg))


(provide 'theme-colors)

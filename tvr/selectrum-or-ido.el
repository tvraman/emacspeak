;;; Switch between selectrum and ido/ido-ubiquitous
(require 'cl-lib)

(defvar selectrum-or-ido 'ido
  "What we are using: ido for ido, selectrum for selectrum")

(defun toggle-selectrum-or-ido ()
  "Toggle between using ido and selectrum."
  (cl-declare (special selectrum-or-ido))
  (cond
   ((eq selectrum-or-ido 'ido)
    (setq selectrum-or-ido 'selectrum))
   ((eq selectrum-or-ido 'selectrum)
    (setq selectrum-or-ido 'ido))
   (t (error "Strange state. selectrum-or-ido is %s" selectrum-or-ido)))
  (selectrum-or-ido-configure selectrum-or-ido))


(defun selectrum-or-ido-configure (backend)
  "Switched to specified backend ido or selectrum."
  (cond
   ((eq backend 'ido)
    (selectrum-mode -1)
    (selectrum-prescient-mode -1)
    (ido-everywhere 1)
    (ido-ubiquitous-mode 1)
    (flx-ido-mode 1))
   ((eq backend 'selectrum)
    (ido-everywhere -1)
    (ido-ubiquitous-mode -1)
    (flx-ido-mode -1)
    (selectrum-mode 1)
    (selectrum-prescient-mode 1))
   (t (error "Unknown backend %s" backend))))

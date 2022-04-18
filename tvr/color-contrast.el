;;; From the author of the modus themes:


;; Code is courtesy of Omar Antolín Camarena:
;; https://github.com/oantolin/emacs-config

(defun wcag (hex)
  (apply #'+
         (cl-mapcar
          (lambda (k x)
            (* k (if (<= x 0.03928)
                     (/ x 12.92)
                   (expt (/ (+ x 0.055) 1.055) 2.4))))
          '(0.2126 0.7152 0.0722)
          (color-name-to-rgb hex))))

(defun clr (c1 c2)
  (let ((ct (/ (+ (wcag c1) 0.05)
               (+ (wcag c2) 0.05))))
    (max ct (/ ct))))

;; You can then combine that with Org's tables. Such as what I have on
;; this blog post:
;; https://protesilaos.com/codelog/2020-07-04-modus-themes-faint-colours/
;; (I am not particularly good with Org tables, but those work).



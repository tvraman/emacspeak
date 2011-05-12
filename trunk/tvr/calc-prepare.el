
;;; Commands added by calc-private-autoloads on Fri Mar 25 22:44:12 1994.
(autoload 'calc-dispatch	   "calc" "Calculator Options" t)
(autoload 'full-calc		   "calc" "Full-screen Calculator" t)
(autoload 'full-calc-keypad	   "calc" "Full-screen X Calculator" t)
(autoload 'calc-eval		   "calc" "Use Calculator from Lisp")
(autoload 'defmath		   "calc" nil t t)
(autoload 'calc			   "calc" "Calculator Mode" t)
(autoload 'quick-calc		   "calc" "Quick Calculator" t)
(autoload 'calc-keypad		   "calc" "X windows Calculator" t)
(autoload 'calc-embedded	   "calc" "Use Calc inside any buffer" t)
(autoload 'calc-embedded-activate  "calc" "Activate =>'s in buffer" t)
(autoload 'calc-grab-region	   "calc" "Grab region of Calc data" t)
(autoload 'calc-grab-rectangle	   "calc" "Grab rectangle of data" t)

(augment-load-path "calc" "calc")
(global-set-key "\e#" 'calc-dispatch)
;;; End of Calc autoloads.
;;; Calc mode settings:
;;; Mode settings stored by Calc on Fri Mar 27 11:24:23 1992
    (setq calc-group-digits t)
    (setq calc-complex-format 'i)
    (setq calc-angle-mode 'rad)
    (setq calc-language 'tex)
    (setq calc-display-trail nil)
    (setq calc-auto-why 1)
;;; End of mode settings


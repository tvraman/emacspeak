;;{{{ Interactive Check Implementation:

;;; Notes:

;; The implementation from 2014 worked for emacspeak.  it has been
;; moved to obsolete/old-emacspeak-preamble.el to avoid the fragility
;; from using backtrace-frame.  See
;; http://tvraman.github.io/emacspeak/blog/ems-interactive-p.html for
;; the version that depended on calling backtrace-frame.

;; This updated implementation avoids that call and was contributed
;; by Stefan Monnier in April 2022.

;;  Note that `ems-interactive-p', unlike `called-interactively-p',
;;  will return non-nil when the original command calls itself recursively.
;;  More specifically `called-interactively-p' tries to returns non-nil
;;  if and only if the current call to the surrounding function (let's call it
;;  F) was made interactively, whereas `ems-interactive-p' returns non-nil if
;;  F happens to be the same function as the one that was called interactively
;;  (either because it's the original (interactive) call, or because of
;;  a nested/recursive call).

;;; Design:
;; Advice on funcall-interactively stores the name of the
;; interactive command being run.
;; The defadvice macro is itself adviced to generate a locally bound
;; predicate that ensures that ems-interactive-p is only called from
;; within emacspeak advice forms.
;; Thus, ems-interactive-p is reserved for use within Emacspeak advice.

(defvar ems--interactive-funcname nil
  "Holds name of function being called interactively.")

(defadvice funcall-interactively (around emacspeak  pre act comp)
  "Record name of interactive function being called."
  (let ((ems--interactive-funcname (ad-get-arg 0)))
    ad-do-it))

;; Beware: Advice on defadvice 
(advice-add 'defadvice :around #'ems--generate-interactive-check)
(defun ems--generate-interactive-check (orig-macro funname args &rest body)
  "Lexically redefine ems-interactive-p  to test  ems--interactive-funcname.
The local definition expands to a call to `eq' that compares
FUNNAME to our stored value of ems--interactive-funcname."
  (apply orig-macro funname args
         (macroexp-unprogn
          (macroexpand-all
           (macroexp-progn body)
           `((ems-interactive-p         ; new definition
              . ,(lambda () `(eq ems--interactive-funcname ',funname)))
             . ,macroexpand-all-environment)))))

(defun ems-interactive-p ()
  "Dynamically defined at runtime to provide Emacspeak's
  interactive check.  This definition never be called, so produce debug
  info if the unexpected happens."
  (cl-declare (special ems--interactive-funcname))
  (error
   (format "From %s: Unexpected call!" ems--interactive-funcname)))

;;}}}

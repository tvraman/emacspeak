;;; Load Next into a running Lisp.
;;; This is what I use with Slime to run Next from Source:

(require :asdf)

(asdf:load-asd
 (format nil "~a/sourceforge/next/next.asd"
         (uiop:getenv "HOME")))
(ql:quickload :next)
(next:start)

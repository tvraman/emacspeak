;;; Load Nyxt into a running Lisp.
;;; This is what I use with Slime to run Nyxt from Source:

(require :asdf)

(asdf:load-asd
 (format nil "~a/sourceforge/nyxt/nyxt.asd"
         (uiop:getenv "HOME")))

(load
 (format nil "~a/sourceforge/nyxt/quicklisp-client/setup.lisp"
         (uiop:getenv "HOME")))

(ql:quickload "nyxt")
(nyxt:start)

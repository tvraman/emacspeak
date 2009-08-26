Index: lisp/emacspeak-moz.el
===================================================================
--- lisp/emacspeak-moz.el	(revision 6157)
+++ lisp/emacspeak-moz.el	(working copy)
@@ -149,7 +149,7 @@
                   (eq browse-url-browser-function 'browse-url-w3)
                   (eq browse-url-browser-function 'w3m-browse-url))
         (emacspeak-webutils-autospeak))
-      (browse-url-of-buffer ))))
+      (browse-url-of-buffer emacspeak-moz-output-buffer))))
 ;;;###autoload
 (defun emacspeak-moz-close-tab-or-browser ()
   "Close tab, or browser when one tab left."
@@ -166,7 +166,7 @@
                            (browse-url-url-at-point)
                            "http://"))))
   (emacspeak-moz-eval-expression
-   (format "window.location.href='%s'\n"
+   (format "content.location.href='%s'\n"
            url)))
 
 ;;;###autoload
@@ -180,7 +180,7 @@
     (cond
      (url
       (emacspeak-moz-eval-expression
-       (format "window.location.href=\"%s\";\n"
+       (format "content.location.href=\"%s\";\n"
                url))
       (message "Sent url at point to firefox."))
      (t (error "No url under point.")))))
@@ -364,10 +364,11 @@
 (add-hook 'inferior-moz-mode-hook
           'emacspeak-moz-init)
 
-(defadvice inferior-moz-start-process (after emacspeak pre act
-                                             comp)
-  "Init emacspeak ADom bits."
-  (emacspeak-moz-init))
+;(defadvice inferior-moz-start-process (after emacspeak pre act
+;                                             comp)
+;  "Init emacspeak ADom bits."
+;  (emacspeak-moz-init))
+
 (add-hook 'javascript-mode-hook
           'emacspeak-setup-programming-mode)
 ;;}}}

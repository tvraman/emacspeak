
(autoload 'html-helper-mode "html-helper-mode")
(augment-auto-mode-alist "\\.html$" 'html-helper-mode)
(declare (special html-helper-do-write-file-hooks
                  html-helper-build-new-buffer
                  html-helper-address-string
                  html-helper-new-buffer-template))
;;; Config:
(setq  html-helper-do-write-file-hooks t)

(setq  html-helper-build-new-buffer t )
                                        
                                        
;;; personal settings:
(setq html-helper-who-am-i "T. V. Raman")
(setq html-helper-my-net-address "tvraman@us.ibm.com")

(setq  html-helper-address-string
       (format "<A href=\"mailto:%s\">Email: %s</a>"
               html-helper-my-net-address html-helper-who-am-i))
                                        
(setq  html-helper-new-buffer-template
       '("<html>\n"
         "<!--$Id$ -->\n"
         "  <head>\n"
         "<LINK REV=MADE HREF=\"mailto:tvraman@us.ibm.com\">\n"
         "<title>" p "</title>\n"
         "</head>\n\n"
         "<body>\n"
         "<h1>" p "</h1>\n\n"
         p
         "\n\n<hr>\n"
         "<address>" html-helper-address-string "</address>\n"
         html-helper-timestamp-start
         html-helper-timestamp-end
         "\n</body>\n </html>\n"))

(add-hook 'html-helper-mode-hook
          (function (lambda ()
                      (auto-fill-mode 1)
                      (require 'html-addons))))
(require 'html-outline)

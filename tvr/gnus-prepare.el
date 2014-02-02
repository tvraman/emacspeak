;;;  Gnus Setup For GMail imap:
;;{{{  News Source etc 
;;; Example: http://www.google.com/url?q=http://blogs.openaether.org/data/gnus.example.el&sa=U&ei=R1DdUuLMCYiDogTV0YHYDg&ved=0CCkQFjAC&usg=AFQjCNF4T3kHZQ8CDmpFbzJeJcXbdTYOXw
(require 'nnimap)
(require 'nnir)
(require 'gnus-demon)
(setq gnus-auto-subscribed-groups nil)
(setq gnus-auto-subscribed-categories nil)





;;; Set all nnimap options through the select method.

(setq
 gnus-ssselect-method
 '(nnimap
   "gmail"
   (nnimap-address "imap.gmail.com")
   (nnimap-server-port 993)
   (nnimap-fetch-partial-articles "text/")
   (nnimap-inbox  "[Gmail]/All")
   (nnimap-split-methods
    (|
     ("List-ID" ".*<\\(.*\\)>.*" "\\1")
     ("Unclassified" ))) 
   (nnimap-expunge-on-close always)
   (nnimap-stream ssl)))

;;; Fetch news when emacs is idle.
(gnus-demon-add-handler 'gnus-demon-scan-news 2 t) 

(setq
 message-send-mail-function 'smtpmail-send-it
 smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
 smtpmail-auth-credentials '(("smtp.gmail.com" 587 user-mail-address nil))
 smtpmail-default-smtp-server "smtp.gmail.com"
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587)
(setq
 gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
(setq gnus-agent nil)

;;}}}
;;{{{  gnus mode hooks 

(add-hook
 'gnus-summary-mode-hook
 #'(lambda ()
     (define-key gnus-summary-mode-map "\C-t" 'gnus-summary-toggle-header)
     (define-key gnus-summary-mode-map "\M-h" 'gnus-summary-hide-all-threads)
     (define-key gnus-summary-mode-map "\M-t" 'gnus-summary-hide-all-headers )
     (define-key gnus-summary-mode-map "t" 'gnus-summary-show-some-headers)
     (define-key gnus-summary-mode-map '[left] 'gnus-summary-catchup-and-exit)
     (define-key gnus-summary-mode-map '[right] 'gnus-summary-show-article)))

(setq  gnus-sort-gathered-threads-function 'gnus-thread-sort-by-date)

(add-hook
 'gnus-group-mode-hook
 #'(lambda ()
     (define-key gnus-group-mode-map '[right] 'gnus-group-read-group)))

;;}}}
;(setq gnus-summary-line-format "%t%U%R%-20,20a %s \n")
;(setq gnus-group-line-format "%M%S%p%P%5y: %(%G%)%l \n")

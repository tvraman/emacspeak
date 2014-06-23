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
 gnus-select-method
 `(nnimap
   "gmail"
   (nnimap-address "imap.gmail.com")
   (nnimap-server-port 993)
   (nnimap-fetch-partial-articles "text/")
   (nnimap-record-commands t)
   (nnimap-expunge-on-close always)
   (nnimap-stream ssl)))

;;; See http://www.cataclysmicmutation.com/2010/11/multiple-gmail-accounts-in-gnus/ for mult-account setup
;;; Use the user as the hostname  in your .authinfo file

(defun gm-user-to-nnimap (user)
  "Return nnimap select method for sspecified user."
  `(nnimap
    ,user
    (nnimap-user ,(format "%s@gmail.com" user))
    (nnimap-address "imap.gmail.com")
    (nnimap-server-port 993)
    (nnimap-fetch-partial-articles "text/")
    ;(nnimap-record-commands t)
    (nnimap-expunge-on-close always)
    (nnimap-stream ssl)
    (nnimap-authinfo-file "~/.authinfo.gpg")))

(setq gnus-secondary-select-methods 
      (mapcar #'gm-user-to-nnimap
              '( "tv.raman.tv" "emacspeak")))

;;; Fetch news when emacs is idle.
;(gnus-demon-add-handler 'gnus-demon-scan-news 2 t) 
(setq
 ;;; use sendmail-send-it if you can send email out directly
 send-mail-function 'smtpmail-send-it
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587
 )

(setq
 gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
(setq gnus-agent nil)

;;}}}
;;{{{  gnus mode hooks 



(setq  gnus-sort-gathered-threads-function 'gnus-thread-sort-by-date)

;;}}}
(setq gnus-summary-line-format "%t%U%R%-20,20a %s \n")
(setq gnus-group-line-format "%M%S%p%P%5y: %(%G%)%l \n")
(provide 'gnus-prepare)

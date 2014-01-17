;;;  Gnus Setup For GMail imap:
;;{{{  News Source etc 

(setq
 gnus-gmail-plain-method 
 '(nnimap "gmail"
          (nnimap-address "imap.gmail.com")
          (nnimap-server-port 993)
          (nnimap-stream ssl)))
;;; dovecot and offlineimap:
(setq
 offline-method
 '(nnimap "google"
          (nnimap-shell-program "/usr/lib/dovecot/imap")
          (nnimap-expunge-on-close always)
          (nnimap-stream shell)))

(setq gnus-select-method gnus-gmail-plain-method)

;;; split out mailing lists:
(setq nnimap-split-fancy
  `(|
   ;; All lists
   ("List-Id" ".*<\\(.*\\).google.com>.*" "\\1")))

(setq nnimap-split-methods '(nnimap-split-fancy) )


(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "raman@google.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
(setq gnus-agent nil)

;;}}}
;;{{{  gnus mode hooks 

(add-hook
 'gnus-summary-mode-hook
 #'(lambda ()
     (define-key gnus-summary-mode-map "\C-t" 'gnus-summary-toggle-header)
     (define-key gnus-summary-mode-map "T" 'gnus-summary-hide-all-headers )
     (define-key gnus-summary-mode-map "t" 'gnus-summary-show-some-headers)
     (define-key gnus-summary-mode-map '[left] 'gnus-summary-catchup-and-exit)
     (define-key gnus-summary-mode-map '[right] 'gnus-summary-show-article)))

(add-hook
 'gnus-select-group-hook
 #'(lambda ()
             (gnus-summary-hide-all-threads)))

;;; author names may be irrelevant:





(add-hook
 'gnus-group-mode-hook
 #'(lambda ()
     (define-key gnus-group-mode-map '[right] 'gnus-group-read-group)))

;;}}}
;;{{{ threading and hiding headers

;;}}}



;(setq gnus-summary-line-format "%t%U%R%-20,20a %s \n")
;(setq gnus-group-line-format "%M%S%p%P%5y: %(%G%)%l \n")









;;;  Gnus Setup For GMail imap:  -*- lexical-binding: nil; -*-
;;{{{  News Source etc
;;; Example: http://www.google.com/url?q=http://blogs.openaether.org/data/gnus.example.el&sa=U&ei=R1DdUuLMCYiDogTV0YHYDg&ved=0CCkQFjAC&usg=AFQjCNF4T3kHZQ8CDmpFbzJeJcXbdTYOXw

(eval-after-load "gnus"
  `(progn
;;; Configure gnus select  via file-xoauth2.el after
;;; customizations have been loaded.
     (setq gnus-auto-subscribed-groups nil
           gnus-auto-subscribed-categories nil)

;;; Fetch news when emacs is idle.
                                        ;(gnus-demon-add-handler 'gnus-demon-scan-news 2 t)

     (setq
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
     (setq gnus-agent nil)

     ;;}}}
     ;;{{{  gnus mode hooks

     (setq  gnus-sort-gathered-threads-function 'gnus-thread-sort-by-date)

     ;;}}}
     (setq gnus-summary-line-format "%t%U%R%-20,20a %s \n")
     (setq gnus-group-line-format "%M%S%p%P%5y: %(%g%)%l \n")
     (defun gmail-report-spam ()
       "Report the current or marked mails as spam.
This moves them into the Spam folder."
       (interactive)
       (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/Spam")
       (emacspeak-auditory-icon 'task-done))

     (define-key gnus-summary-mode-map "$" 'gmail-report-spam)
     (load-library "file-xoauth2")
     (provide 'gnus-prepare)
     ))

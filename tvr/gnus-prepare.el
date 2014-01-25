;;;  Gnus Setup For GMail imap:
;;{{{  News Source etc 
;;; Example: http://www.google.com/url?q=http://blogs.openaether.org/data/gnus.example.el&sa=U&ei=R1DdUuLMCYiDogTV0YHYDg&ved=0CCkQFjAC&usg=AFQjCNF4T3kHZQ8CDmpFbzJeJcXbdTYOXw
(setq
 gnus-gmail-plain-method 
 '(nnimap "gmail"
          (nnimap-address "imap.gmail.com")
          (nnimap-server-port 993)
          (nnimap-stream ssl)))

(setq gnus-select-method gnus-gmail-plain-method)
;;; Fetch news when emacs is idle.
(gnus-demon-add-handler 'gnus-demon-scan-news 2 t) 
    (gnus-demon-init)  ; this is redundant in No Gnus (e.g. Emacs23) since gnus-demon-add-handler does it for you

 You don't need to set any other variables nor load more files.

 The parameters to gnus-demon-add-handler are:

* FUNC the function to call
* TIME how many time units ( specified in gnus-demon-timestep) pass between attempts to call the function
* IDLE whether Emacs has to be idle in order to call the function. t: it must be idle. A number: it must have been idle for this many minutes
(well, gnus-demon-timestep). nil: call it anyway even if it isn't idle

 Gnus demon is pretty smart, but it's behavior is not properly documented. Here is how it works. Assume you set gnus-demon-timestep to 1 second,
the TIME to 60 and IDLE to 5, then gnus will try to call your function every 60 seconds, but it will first check if you have been idle for 5
seconds. If so, it executes the function, if not, it sets the idle timer which is triggered first time you are idle for 5 sec. Once executed the
one minute count starts again and gnus takes it all over again. Thus, you are pretty sure that the intervals between calls are not smaller than
(60 sec) + (continuous work time) + (5 idle sec).
                                                                                                                                                  

 If you want to be notified of new and interesting mail, take a look at GnusNotify
                                                                                                                                                  

 Scanning all the news can be slow. If you want a fast scan only of the important lists (mail for example). Set the level of important groups to
lower or equal to 3 and add this:

 (defun gnus-demon-scan-news-3 ()
  (let ((win (current-window-configuration))
        (gnus-read-active-file 'some)
        (gnus-check-new-newsgroups nil)
        (gnus-verbose 2)
        (gnus-verbose-backends 5)
        (level 3)
        )
    ;; (message "check-mail: %s" (format-time-string "%H:%M:%S"))
    (while-no-input
      (unwind-protect
          (save-window-excursion
            (when (gnus-alive-p)
              (with-current-buffer gnus-group-buffer
                (gnus-group-get-new-news level))))
        (set-window-configuration win)))))

(setq gnus-demon-timestep 10)
(gnus-demon-add-handler 'gnus-demon-scan-news-3 12 1)

 This checks your mail every 2 minutes on condition that you have been idle for 10 seconds. No check message is displayed and gnus is not
stalling emacs (that is, on user input the process is aborted because of the while-no-input wraper).

 (this doesn't work with virtual groups (gnus bug), remove level in gnus-group-get-new-news call)
                                                                                                                                                  

 I'm thinking of adding something like:

     (gnus-demon-add-handler 'gnus-group-save-newsrc 10 t)

 Or could it be harmful?
                                                                                                                                                  

 On my laptop if I forgot to unplug Gnus before I went offline the demon would crash Emacs. This solves the problem.

 (defadvice gnus-demon-scan-news (around gnus-demon-timeout activate)
  "Timeout for Gnus."
  (with-timeout
      (120 (message "Gnus timed out."))
    ad-do-it))

 Of course the length of the timeout should be set generously.
                                                                                                                                                  

 CategoryGnus
 
                                                                                                                                                   

 SiteMap Search ElispArea HowTo Glossary RecentChanges News Problems Suggestions Add Translation Talk Edit this page View other revisions
Administration Last edited 2012-09-05 10:08 UTC by spinuvit (diff)

 Search:                      Language:            [Go!]
   CC-GNU GPL 
 This work is licensed to you under version 2 of the GNU General Public License. Alternatively, you may choose to receive this work under any
other license that grants the right to use, copy, modify, and/or distribute the work, as long as that license imposes the restriction that
derivative works have to grant the same rights and impose the same restriction. For example, you may choose to receive this work under the GNU
Free Documentation License, the CreativeCommons ShareAlike License, the XEmacs manual license, or similar licenses.

;;; split out mailing lists:
;;; Mail split per Info manual:

(setq nnimap-inbox "[Gmail]/Important")

(setq nnimap-split-fancy
      '(|
    ("List-Id" ".*<\\(.*\\).com>.*" "\\1")
    ("X-Mailing-List" ".*<\\(.*\\).org>.*" "\\1")))

(setq nnimap-split-methods 'nnimap-split-fancy)


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

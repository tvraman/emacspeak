(require 'smtpmail)
(require 'nnir)

(setq smtpmail-debug-info t)

(setq nnmail-expiry-wait 2)
(setq nnmail-resplit-incoming t)
(setq user-mail-address "user1@example.com")
(setq mail-host-address "example.com")
(setq gnus-message-archive-group "nnimap+user1:INBOX.Sent")
(setq gnus-outgoing-message-group "nnimap+user1:INBOX.Sent")

(setq gnus-select-method '(nntp "news" 
				(nntp-address "news.example.com")
				(nnir-search-engine nntp))
      )

(setq gnus-secondary-select-methods 
      '((nnimap "user1"
		(remove-prefix "INBOX.")
		(nnimap-address "mail.example.com")
		(nnimap-stream ssl)
		(nnimap-nov-is-evil t)
		(nnir-search-engine imap)
		(nnimap-authinfo-file "~/.imap-user1")
		)
	(nnimap "user2"
		(remove-prefix "INBOX.")
		(nnimap-address "mail.example.com")
		(nnimap-stream ssl)
		(nnimap-nov-is-evil t)
		(nnir-search-engine imap)
		(nnimap-authinfo-file "~/.imap-user2")
		)
	(nnimap "user3"
		(remove-prefix "INBOX.")
		(nnimap-address "imap.example2.com")
		(nnimap-stream ssl)
		(nnimap-nov-is-evil t)
		(nnir-search-engine imap)
		(nnimap-authinfo-file "~/.imap-user3")
		)
	(nnimap "user4"
		(remove-prefix "INBOX.")
		(nnimap-address "mail.example3.com")
		(nnimap-nov-is-evil t)
		(nnir-search-engine imap)
		(nnimap-authinfo-file "~/.imap-user4")
		)
	(nnimap "user5"
		(remove-prefix "INBOX.")
		(nnimap-address "mail.example3.com")
		(nnimap-nov-is-evil t)
		(nnir-search-engine imap)
		(nnimap-authinfo-file "~/.imap-user5")
		)
	(nnml "user6")
	)
      )

(setq mail-sources '((pop :server "pop.example4.com"
			  :user "user6"
			  :password "passwd")))
(remove-hook 'gnus-mark-article-hook
             'gnus-summary-mark-read-and-unread-as-read)
(add-hook 'gnus-mark-article-hook 'gnus-summary-mark-unread-as-read)

(setq mm-text-html-renderer 'w3m)
(eval-after-load "mm-decode"
 '(progn 
      (add-to-list 'mm-discouraged-alternatives "text/html")
      (add-to-list 'mm-discouraged-alternatives "text/richtext")))

;; filter crud
(setq nnimap-split-inbox '("INBOX" ))
(setq nnimap-split-predicate "UNDELETED")
(setq nnimap-split-crosspost nil)
(setq nnimap-split-rule '(("user1" ("INBOX" nnimap-split-fancy))
			  ("user2" ("INBOX" nnimap-split-fancy))
			  ("user3" ("INBOX" nnimap-split-fancy))
			  ("user4" ("INBOX" nnimap-split-fancy))
			  ("user5" ("INBOX" nnimap-split-fancy)))
      nnimap-split-fancy
      '(| 
	("X-Spam-Flag" "YES" "INBOX.Junk")
	("List-Id" ".*news.jabber.org.*" "INBOX.jabber.news")
	("^\\(Delivered-\\)?To" "\\(.*\\)-\\(.*\\)@example.com" "INBOX.alias.\\2")
	"INBOX.unsorted"))

(setq nnmail-split-methods
      '(("mail.misc" ".*")))


;; setting up posting styles
(setq gnus-posting-styles
      '((".*"
	 (address "user1@example.com")
	 (name "User One"))
	("one-list@example.com
	 (address "user1-one@example.com"))
	("two-list@test.com"
	 (address "user1-two@example.com"))
	("three-list@test2.com"
	 (address "user1-three@example.com"))
))

	  

;;gnus-group-line-format's default value was
;;"%M%S%p%P%5y:%B%(%g%)%l %O\n"

;;(setq gnus-group-line-format "%M%S%5y/%-5t: %uG %D\n")
(setq gnus-group-line-format "%M%S%p%P%5y:%B%(%uG%)%l %O\n")
(defun gnus-user-format-function-G (arg) 
  (concat (car (cdr gnus-tmp-method)) ":"
          (or (gnus-group-find-parameter gnus-tmp-group 'display-name)
              (let ((prefix (assq 'remove-prefix (cddr gnus-tmp-method))))
                (if (and prefix
                         (string-match (concat "^\\("
                                               (regexp-quote (cadr prefix))
                                               "\\)")
                                       gnus-tmp-qualified-group))
                    (substring gnus-tmp-qualified-group (match-end 1))
                  gnus-tmp-qualified-group)))))

(setq gnus-thread-hide-subtree t)

(setq gnus-summary-same-subject "")
(setq gnus-sum-thread-tree-root "")
(setq gnus-sum-thread-tree-single-indent "")
(setq gnus-sum-thread-tree-leaf-with-other "+-> ")
(setq gnus-sum-thread-tree-vertical "|")
(setq gnus-sum-thread-tree-single-leaf "`-> ")


(require 'gnus-demon)
(setq gnus-use-demon t)
(gnus-demon-add-handler 'gnus-group-get-new-news 10 2)
(gnus-demon-init)


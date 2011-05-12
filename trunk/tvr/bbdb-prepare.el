; Configure BBDB
(augment-load-path "bbdb/lisp" "bbdb")

;; Autoload for bbdb

(autoload 'bbdb         "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-name    "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-company "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-net     "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-notes   "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-insinuate-mh       "bbdb-mhe"   "Hook BBDB into MH-E")
(autoload 'bbdb-insinuate-gnus     "bbdb-gnus"  "Hook BBDB into GNUS")
(autoload 'bbdb-insinuate-vm     "bbdb-vm"  "Hook BBDB into vm")
(autoload 'bbdb-insinuate-sendmail "bbdb"       "Hook BBDB into sendmail")
(autoload 'bbdb/gnus-lines-and-from "bbdb-gnus")

;; Configuration for bbdb
(setq bbdb-ignore-some-messages-alist
      '(
      		("From" . "delivery system")
	("From" . "delivery")
	("From" . "listmaster")
	("From" . "mailer-daemon")
	("From" . "operator")
	("From" . "post office")
        ("From" . "postmaster")
        ("From" . "root")
        ("From" . "admin")
("From" . "online")
("From" . "alert")
        ("From" . "webmaster")
("From" . "administ")
("From" . "daemon")))

(setq bbdb-ignore-most-messages-alist
      '(	
	("To"   . "tvraman")		; record mail to me))
(setq bbdb-completion-type 'primary-or-name)



; Always save BBDB without asking
(setq bbdb-offer-save 'always)

;; For bbdb-print

(setq bbdb-use-pop-up nil)
(setq bbdb-canonicalize-redundant-nets-p t)



(when (featurep 'vm)
  (add-hook 'vm-quit-hook 'bbdb-save-db))

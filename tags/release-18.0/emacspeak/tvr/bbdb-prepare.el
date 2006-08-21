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
      '(("From" . "daemon")
	("From" . "mailer-daemon")
        ("From" . "postmaster")
        ("From" . "webmaster")
        ("From" . "root")
	("From" . "delivery system")
	("From" . "postmaster")
	("From" . "listmaster")
	("From" . "post office")
	("From" . "root")
	("From" . "operator")
	("From" . "delivery")
	("From" . "administ")
	))
(setq bbdb/mail-auto-create-p 'bbdb-ignore-most-messages-hook)
(setq bbdb-ignore-most-messages-alist
      '(	
	("To"   . "raman")		; record mail to me
	))
(setq bbdb-completion-type 'primary-or-name)



; Always save BBDB without asking
(setq bbdb-offer-save 'always)

;; For bbdb-print

(setq bbdb-use-pop-up nil)
(setq bbdb-canonicalize-redundant-nets-p t)


(setq bbdb-quiet-about-name-mismatches t)
(when (featurep 'vm)
  (add-hook 'vm-quit-hook 'bbdb-save-db))

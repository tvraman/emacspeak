
(require 'eudc)

(global-set-key "\C-c\C-q" 'eudc-query-form)
(defadvice ldap-search-internal (around fix-directory-bug pre act
                                        comp)
  "chdir to /tmp so we find the answers."
  (let ((default-directory "/tmp/"))
ad-do-it
ad-return-value))

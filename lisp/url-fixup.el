;;; pattern: http://www.google.com/url?q=http://emacspeak.sourceforge.net/&sa=U&ei=GceWT42_EY_ViALW84nlCQ&ved=0CBIQFjAA&usg=AFQjCNGz91Z7Yz9dPVoKPP6HVGZ0UqFhRA
;;; prefix: http://www.google.com/url?q=
;;; Suffix: &sa=...

(defsubst emacspeak-w3-canonicalize-google-result-url (url)
  (let ((prefix-length (if emacspeak-websearch-google-use-https 29 28))
        (suffix-start (string-match "&sa=" url)))
    (substring url prefix-length suffix-start)))

(defsubst emacspeak-w3-google-result-url-prefix ()
  "Return prefix of result urls."
  (declare (special emacspeak-websearch-google-use-https))
  (format "%s://www.google.com/url?q="
          (if emacspeak-websearch-google-use-https "https" "http")))

             
(defadvice url-retrieve-internal (before fix-bug pre act comp)
  "Fix bug in handling of google result urls."
  (let ((u (ad-get-arg 0)))
    (when (string-prefix-p (emacspeak-google-result-url-prefix) u)
      (ad-set-arg 0 (emacspeak-canonicalize-google-result-url u)))))

(defun foo (x y)
  "show x"
  (message (concat x y)))

(defadvice foo (before try pre act comp)
  (ad-set-argument 0 (concat (ad-get-arg 0) " world")))

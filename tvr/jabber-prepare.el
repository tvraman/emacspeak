(augment-load-path "emacs-jabber")
(load-library "jabber")
(load-library "ssl")
(load-library "nm")
(add-hook 'nm-connected-hook 'jabber-connect-all)
(add-hook 'nm-disconnected-hook 'jabber-disconnect)
(nm-enable)

;;; fixing namespaces so we work with Googletalk (thanks Magnus)

(defun jabber-xml-resolve-namespace-prefixes (xml-data &optional default-ns prefixes)
  (let ((node-name (jabber-xml-node-name xml-data))
	(attrs (jabber-xml-node-attributes xml-data)))
    ;; First find any foo:xmlns attributes..
    (dolist (attr attrs)
      (let ((attr-name (symbol-name (car attr))))
	(when (string-match "xmlns:" attr-name)
	  (let ((prefix (substring attr-name (match-end 0)))
		(ns-uri (cdr attr)))
	    ;; A slightly complicated dance to never change the
	    ;; original value of prefixes (since the caller depends on
	    ;; it), but also to avoid excessive copying (which remove
	    ;; always does).  Might need to profile and tweak this for
	    ;; performance.
	    (setq prefixes
		  (cons (cons prefix ns-uri)
			(if (assoc prefix prefixes)
			    (remove (assoc prefix prefixes) prefixes)
			  prefixes)))))))
    ;; If there is an xmlns attribute, it is the new default
    ;; namespace.
    (let ((xmlns (jabber-xml-get-xmlns xml-data)))
      (when xmlns
	(setq default-ns xmlns)))
    ;; Now, if the node name has a prefix, replace it and add an
    ;; "xmlns" attribute.  Slightly ugly, but avoids the need to
    ;; change all the rest of jabber.el at once.
    (let ((node-name-string (symbol-name node-name)))
      (when (string-match "\\(.*\\):\\(.*\\)" node-name-string)
	(let* ((prefix (match-string 1 node-name-string))
	       (unprefixed (match-string 2 node-name-string))
	       (ns (assoc prefix prefixes)))
	  (if (null ns)
	      ;; This is not supposed to happen...
	      (message "jabber-xml-resolve-namespace-prefixes: Unknown prefix in %s" node-name-string)
	    (setf (car xml-data) (intern unprefixed))
	    (setf (cadr xml-data) (cons (cons 'xmlns (cdr ns)) (delq 'xmlns attrs)))))))
    ;; And iterate through all child elements.
    (mapc (lambda (x) 
	    (when (listp x)
	      (jabber-xml-resolve-namespace-prefixes x default-ns prefixes)))
	  (jabber-xml-node-children xml-data))
    xml-data))

(defun jabber-filter (process fsm)
  "the filter function for the jabber process"
  (with-current-buffer (process-buffer process)
    ;; Start from the beginning
    (goto-char (point-min))
    (let (xml-data)
      (loop 
       do
       ;; Skip whitespace
       (unless (zerop (skip-chars-forward " \t\r\n"))
	 (delete-region (point-min) (point)))
       ;; Skip processing directive
       (when (looking-at "<\\?xml[^?]*\\?>")
	 (delete-region (match-beginning 0) (match-end 0)))

       ;; Stream end?
       (when (looking-at "</stream:stream>")
	 (return (fsm-send fsm :stream-end)))

       ;; Stream header?
       (when (looking-at "<stream:stream[^>]*>")
	 (let ((stream-header (match-string 0))
	       (ending-at (match-end 0))
	       session-id stream-version)
	   ;; These regexps extract attribute values from the stream
	   ;; header, taking into account that the quotes may be either
	   ;; single or double quotes.
	   (setq session-id
		 (and (or (string-match "id='\\([^']+\\)'" stream-header)
			  (string-match "id=\"\\([^\"]+\\)\"" stream-header))
		      (jabber-unescape-xml (match-string 1 stream-header))))
	   (setq stream-version
		 (and (or
		       (string-match "version='\\([0-9.]+\\)'" stream-header)
		       (string-match "version=\"\\([0-9.]+\\)\"" stream-header))
		      (match-string 1 stream-header)))
	   (jabber-log-xml fsm "receive" stream-header)
	   
	   ;; If the server is XMPP compliant, i.e. there is a version attribute
	   ;; and it's >= 1.0, there will be a stream:features tag shortly,
	   ;; so just wait for that.

	   (fsm-send fsm (list :stream-start session-id stream-version))
	 
	   (delete-region (point-min) ending-at)))
       
       ;; Normal tag

       ;; XXX: do these checks make sense?  If so, reinstate them.
       ;;(if (active-minibuffer-window)
       ;;    (run-with-idle-timer 0.01 nil #'jabber-filter process string)

       ;; This check is needed for xml.el of Emacs 21, as it chokes on
       ;; empty attribute values.
       (save-excursion
	 (while (search-forward-regexp " \\w+=''" nil t)
           (replace-match "")))
       
       (setq xml-data (and (catch 'unfinished
			     (jabber-xml-skip-tag-forward)
			     (> (point) (point-min)))
			   (xml-parse-region (point-min) (point))))
       (if xml-data
	   (jabber-reset-choked))

       while xml-data
       do
       ;; If there's a problem with writing the XML log,
       ;; make sure the stanza is delivered, at least.
       (condition-case e
	   (jabber-log-xml fsm "receive" (car xml-data))
	 (error
	  (ding)
	  (message "Couldn't write XML log: %s" (error-message-string e))
	  (sit-for 2)))
       (delete-region (point-min) (point))

       (fsm-send fsm (list :stanza (jabber-xml-resolve-namespace-prefixes (car xml-data))))
       ;; XXX: move this logic elsewhere
       ;; We explicitly don't catch errors in jabber-process-input,
       ;; to facilitate debugging.
       ;; (jabber-process-input (car xml-data))
       ))))

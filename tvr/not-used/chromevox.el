(defcustom browse-url-chromevox-program "google-chrome"
  "Chrome executable with ChromeVox loaded."
  :type 'string
  :group 'chromevox)
(defvar browse-url-chromevox-arguments nil)


(defun browse-url-chromevox (url &optional new-window)
  "Ask the Chrome browser with ChromeVox loaded  WWW browser to load URL.
Default to the URL around or before point.  The strings in
variable `browse-url-chromevox-arguments' are also passed to
Chromium."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
	   (concat browse-url-chromevox-program  url) nil
	   browse-url-chromevox-program
	   (append
	    browse-url-chromium-arguments
	    (list url)))))

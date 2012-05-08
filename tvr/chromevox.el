(defcustom chromevox-chrome "google-chrome"
  "Chrome executable with ChromeVox loaded."
  :type 'string
  :group 'chromevox)

(defun browse-url-chromium (url &optional new-window)
  "Browse using our talking Chromium build."
  (interactive (browse-url-interactive-arg "URL: "))
  (shell-command
(format "%s '%s'" chromevox-chrome url)))

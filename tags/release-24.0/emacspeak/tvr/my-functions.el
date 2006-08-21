;;;$Id$(require 'cl)
(defun my-thanks-mail-signature()
  "insert thanks , --Raman at the end of mail message"
  (interactive)
  (goto-char (point-max))
  (insert
   (format "\n Thanks, \n --Raman\n")))

(defalias 'my-debug 'toggle-debug-on-error)

(defun tex-tie-current-word(n)
  (interactive "P")
  "Tie the next n  words."
  (or n (setq n 1))
  (while
      (> n 0)
    (setq n (- n 1))
    (forward-word 1)
    (delete-horizontal-space)
    (insert-char 126 1)
    )
  (forward-word 1))

(defun  byte-compile-current-buffer()
  "byte compile current buffer"
  (interactive)
  (byte-compile-file  (buffer-file-name )))

(defun load-current-file ()
  (interactive)
  "load file into emacs"
  (load-file (buffer-file-name)))

(defun end-of-word(arg)
  "move to end of word"
  (interactive "P")
  (if arg
      (forward-word arg)
    (forward-word 1)))

(defun comma-at-end-of-word()
  (interactive)
  "Move point to end of word and put a comma."
  (forward-word 1)
  (insert-char
   (string-to-char ",") 1))

(defun lacheck-buffer-file()
  (interactive)
  "Lacheck file visited in current buffer"
  (compile (format "lacheck %s"
                   (buffer-file-name (current-buffer)))))

(defun dired-play-sound ()
  "Play the file on the current line. "
  (interactive)
  (start-process "play" nil "play"
                 (dired-get-filename t t ) ))(require 'dired)

(defun next-interactive-defun ()
  "Move point to the next interactive defun"
  (interactive)
  (end-of-defun)
  (re-search-forward "^ *(interactive")
  (beginning-of-defun)
  (emacspeak-speak-line))

(defun display-pod-as-manpage (filename)
  "Create a virtual manpage in Emacs from the Perl Online Documentation."
  (interactive
   (list
    (expand-file-name
     (read-file-name "Enter name of POD file: "))))
  (require 'man)
  (let* ((pod2man-args (concat filename " | nroff -man "))
	 (bufname (concat "Man " filename))
	 (buffer (generate-new-buffer bufname)))
    (save-excursion
      (set-buffer buffer)
      (let ((process-environment (copy-sequence process-environment)))
        ;; Prevent any attempt to use display terminal fanciness.
        (setenv "TERM" "dumb")
        (set-process-sentinel
         (start-process pod2man-program buffer "sh" "-c"
                        (format (cperl-pod2man-build-command) pod2man-args))
         'Man-bgproc-sentinel)))))

(provide 'my-functions)
(defun complete-isearch (regexp)
  "Search in the completions.  If a prefix is given, use REGEXP isearch."
  (interactive "P")
  (unless (and (memq last-command '(minibuffer-complete
        minibuffer-completion-help))
        (window-live-p minibuffer-scroll-window))
    (minibuffer-completion-help))
  (with-current-buffer (window-buffer minibuffer-scroll-window)
    (save-window-excursion
      (select-window minibuffer-scroll-window)
      (if (isearch-forward regexp nil)
   (choose-completion)))))
(define-key minibuffer-local-must-match-map "\C-s" 'complete-isearch)
(define-key minibuffer-local-map "\C-s" 'complete-isearch)
(define-key minibuffer-local-completion-map "\C-s" 'complete-isearch)
(defun voice-mail ()
  "Open voice mail in VM."
  (interactive)
  (vm-visit-folder
   (file-name-sans-extension emacspeak-voicemail-spool-file))
  (emacspeak-vm-mode-line))

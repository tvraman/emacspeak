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

;;{{{ amphetadesk
(defgroup amphetadesk nil
  "AmphetaDesk"
  :group 'aplications)

(defcustom amphetadesk-program "amp"
  "Script that launches amphetadesk."
  :type 'file
  :group 'amphetadesk)

(defcustom amphetadesk-port 8888
  "Port where AmphetaDesk listens."
  :type 'integer
  :group 'amphetadesk)

(defsubst amphetadesk-ensure-live ()
  "Ensure AmphetaDesk is alive, and start it if necessary."
  (declare (special amphetadesk-program
                    amphetadesk-port))
  (if (=  1
          (shell-command
           (format "netstat -nat | grep %s"
amphetadesk-port)))
      (shell-command
       (format "%s &"
               amphetadesk-program)
       "*AmphetaDesk*")))

(defun amphetadesk ()
  "Open amphetadesk"
  (interactive)
  (amphetadesk-ensure-live)
  (w3-fetch "http://127.0.0.1:8888/"))

;;}}}

(provide 'my-functions)

(with-eval-after-load
    "sdcv"

  (defun tvr-sdcv-update-dictionary-list ()
    "Update sdcv dictionary lists if necessary by examining
/usr/share/sdcv/dict"
    (cl-declare (special sdcv-dictionary-complete-list
                         sdcv-dictionary-simple-list))
    (let ((installed (split-string (shell-command-to-string "sdcv -l") "\n"))
          (dictionaires nil))
      (pop installed)                   ; nuke header line
      (setq dictionaires
            (cl-loop
             for d in installed collect
             (split-string d " " 'omit-nulls " ")))
      (setq sdcv-dictionary-simple-list
            (cl-loop
             for d in dictionaires collect
             (mapconcat #'identity (butlast d) " ")))))
  (tvr-sdcv-update-dictionary-list)
  )


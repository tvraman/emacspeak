;;; cus-load.el --- automatically extracted custom dependencies
;;
;;; Code:

(put 'dtk 'custom-loads '("dtk-tcl"))
(put 'emacspeak-tnt 'custom-loads '("emacspeak-tnt"))
(put 'emacspeak-forms 'custom-loads '("emacspeak-forms"))
(put 'emacspeak-rss 'custom-loads '("emacspeak-rss"))
(put 'speedbar 'custom-loads '("emacspeak-speedbar"))
(put 'emacspeak-xslt 'custom-loads '("emacspeak-xslt"))
(put 'emacspeak-eshell 'custom-loads '("emacspeak-eshell"))
(put 'emacspeak-rpm 'custom-loads '("emacspeak-rpm-spec"))
(put 'emacspeak-speak 'custom-loads '("emacspeak-advice" "emacspeak-speak" "emacspeak-wizards"))
(put 'flyspell 'custom-loads '("emacspeak-flyspell"))
(put 'emacspeak-widget 'custom-loads '("emacspeak-widget"))
(put 'eshell 'custom-loads '("emacspeak-eshell"))
(put 'emacspeak-realaudio 'custom-loads '("emacspeak-realaudio"))
(put 'emacspeak-imcom 'custom-loads '("emacspeak-imcom"))
(put 'voice-lock 'custom-loads '("voice-lock"))
(put 'emacspeak-ocr 'custom-loads '("emacspeak-ocr"))
(put 'emacspeak-websearch 'custom-loads '("emacspeak-websearch"))
(put 'forms 'custom-loads '("emacspeak-forms"))
(put 'tts 'custom-loads '("dtk-speak" "dtk-tcl" "emacspeak-setup" "voice-setup"))
(put 'emacspeak-pronounce 'custom-loads '("emacspeak-pronounce"))
(put 'emacspeak 'custom-loads '("dtk-speak" "emacspeak-advice" "emacspeak-loaddefs" "emacspeak-dismal" "emacspeak-ediff" "emacspeak-erc" "emacspeak-eshell" "emacspeak-eudc" "emacspeak-flyspell" "emacspeak-forms" "emacspeak-imcom" "emacspeak-info" "emacspeak-ispell" "emacspeak-ocr" "emacspeak-rss" "emacspeak-websearch" "emacspeak-midge" "emacspeak-outline" "emacspeak-personality" "emacspeak-pronounce" "emacspeak-replace" "emacspeak-speak" "emacspeak-speedbar" "emacspeak-table-ui" "emacspeak-view-process" "emacspeak-vm" "emacspeak-w3" "emacspeak-widget" "emacspeak-wizards" "emacspeak" "voice-lock"))
(put 'emacspeak-xml-shell 'custom-loads '("emacspeak-xml-shell"))
(put 'amphetadesk 'custom-loads '("amphetadesk"))
(put 'comint 'custom-loads '("emacspeak-advice"))
(put 'isearch 'custom-loads '("emacspeak-replace"))
(put 'emacspeak-eudc 'custom-loads '("emacspeak-eudc"))
(put 'vm 'custom-loads '("emacspeak-vm"))
(put 'w3 'custom-loads '("emacspeak-w3"))
(put 'emacspeak-table 'custom-loads '("emacspeak-table-ui"))
(put 'emacspeak-wizards 'custom-loads '("emacspeak-wizards" "emacspeak-xslt"))
(put 'emacspeak-speedbar 'custom-loads '("emacspeak-speedbar"))
(put 'emacspeak-sounds 'custom-loads '("emacspeak-sounds"))
(put 'emacspeak-flyspell 'custom-loads '("emacspeak-flyspell"))
(put 'emacspeak-eterm 'custom-loads '("emacspeak-eterm"))
(put 'widgets 'custom-loads '("emacspeak-widget"))
(put 'emacspeak-ispell 'custom-loads '("emacspeak-ispell"))
(put 'eamcspeak-erc 'custom-loads '("emacspeak-erc"))
(put 'emacspeak-erc 'custom-loads '("emacspeak-erc"))
(put 'emacspeak-vm 'custom-loads '("emacspeak-vm"))
(put 'emacspeak-custom 'custom-loads '("emacspeak-custom"))
(put 'emacspeak-w3 'custom-loads '("emacspeak-w3"))
(put 'eudc 'custom-loads '("emacspeak-eudc"))
(put 'emacspeak-midge 'custom-loads '("emacspeak-midge"))
(put 'emacspeak-aumix 'custom-loads '("emacspeak-aumix"))
(put 'emacspeak-m-player 'custom-loads '("emacspeak-m-player"))
(put 'emacspeak-view-process 'custom-loads '("emacspeak-view-process"))
(put 'emacspeak-dismal 'custom-loads '("emacspeak-dismal"))
(put 'aplications 'custom-loads '("amphetadesk"))
(put 'emacspeak-ediff 'custom-loads '("emacspeak-ediff"))
(put 'emacspeak-remote 'custom-loads '("emacspeak-remote"))
;;; These are for handling :version.  We need to have a minimum of
;;; information so `custom-changed-variables' could do its job.  
;;; For both groups and variables we have to set `custom-version'.
;;; For variables we also set the `standard-value' and for groups
;;; `group-documentation' (which is shown in the customize buffer), so
;;; we don't have to load the file containing the group.

;;; `custom-versions-load-alist' is an alist that has as car a version
;;; number and as elts the files that have variables that contain that
;;; version. These files should be loaded before showing the
;;; customization buffer that `customize-changed-options' generates.


;;; This macro is used so we don't modify the information about
;;; variables and groups if it's already set. (We don't know when
;;; cus-load.el is going to be loaded and at that time some of the
;;; files might be loaded and some others might not).
(defmacro custom-put-if-not (symbol propname value)
  `(unless (get ,symbol ,propname)
     (put ,symbol ,propname ,value)))


(defvar custom-versions-load-alist nil
 "For internal use by custom.")

(provide 'cus-load)

;;; Local Variables:
;;; version-control: never
;;; no-byte-compile: t
;;; no-update-autoloads: t
;;; End:
;;; cus-load.el ends here

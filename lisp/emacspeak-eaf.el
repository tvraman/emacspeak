;;; emacspeak-eaf.el --- Speech-enable EAF  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable EAF An Emacs Interface to eaf
;; Keywords: Emacspeak,  Audio Desktop eaf
;;;   LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;; A speech interface to Emacs |
;; 
;;  $Revision: 4532 $ |
;; Location undetermined
;; 

;;;   Copyright:
;; Copyright (C) 1995 -- 2007, 2019, T. V. Raman
;; All Rights Reserved.
;; 
;; This file is not part of GNU Emacs, but the same permissions apply.
;; 
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNEAF FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Commentary:
;; EAF ==  Emacs Application Framework
;; https://github.com/manateelazycat/emacs-application-framework.git 
;; This module speech-enables the elisp side of EAF.

;;; Code:

;;;   Required modules

(eval-when-compile (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;;  Interactive Commands:

'(
  eaf--kill-python-process
  eaf-browser-restore-buffers
  eaf-browser-send-esc-or-exit-fullscreen
  eaf-create-mindmap
  eaf-describe-bindings
  eaf-edit-buffer-cancel
  eaf-edit-buffer-confirm
  eaf-edit-buffer-switch-to-org-mode
  eaf-edit-mode
  eaf-elfeed-open-url
  eaf-file-browser-qrcode
  eaf-file-sender-qrcode
  eaf-file-sender-qrcode-in-dired
  eaf-generate-keymap-doc
  eaf-get-path-or-url
  eaf-goto-left-tab
  eaf-goto-right-tab
  eaf-import-chrome-bookmarks
  eaf-import-firefox-bookmarks
  eaf-install-dependencies
  eaf-interleave-add-file-url
  eaf-interleave-add-note
  eaf-interleave-app-mode
  eaf-interleave-mode
  eaf-interleave-open-notes-file
  eaf-interleave-quit
  eaf-interleave-sync-current-note
  eaf-interleave-sync-next-note
  eaf-interleave-sync-pdf-page-current
  eaf-interleave-sync-previous-note
  eaf-keyboard-quit
  eaf-kill-process
  eaf-mode
  eaf-open
  eaf-open-airshare
  eaf-open-bookmark
  eaf-open-browser
  eaf-open-browser-with-history
  eaf-open-camera
  eaf-open-demo
  eaf-open-external
  eaf-open-ipython
  eaf-open-jupyter
  eaf-open-mail-as-html
  eaf-open-mindmap
  eaf-open-music
  eaf-open-office
  eaf-open-terminal
  eaf-open-this-buffer
  eaf-open-url-at-point
  eaf-open-vue-demo
  eaf-pdf-outline
  eaf-pdf-outline-jump
  eaf-pdf-outline-mode
  eaf-proxy-atomic_edit
  eaf-proxy-caret_toggle_browsing
  eaf-proxy-clear_cookies
  eaf-proxy-clear_focus
  eaf-proxy-clear_history
  eaf-proxy-copy_code
  eaf-proxy-copy_link
  eaf-proxy-copy_text
  eaf-proxy-duplicate_page
  eaf-proxy-eval_js
  eaf-proxy-eval_js_file
  eaf-proxy-history_backward
  eaf-proxy-history_forward
  eaf-proxy-import_chrome_history
  eaf-proxy-insert_or_caret_at_line
  eaf-proxy-insert_or_close_buffer
  eaf-proxy-insert_or_download_youtube_audio
  eaf-proxy-insert_or_download_youtube_video
  eaf-proxy-insert_or_duplicate_page
  eaf-proxy-insert_or_edit_url
  eaf-proxy-insert_or_focus_input
  eaf-proxy-insert_or_history_backward
  eaf-proxy-insert_or_history_forward
  eaf-proxy-insert_or_new_blank_page
  eaf-proxy-insert_or_open_browser
  eaf-proxy-insert_or_open_downloads_setting
  eaf-proxy-insert_or_open_link
  eaf-proxy-insert_or_open_link_background_buffer
  eaf-proxy-insert_or_open_link_new_buffer
  eaf-proxy-insert_or_recover_prev_close_page
  eaf-proxy-insert_or_refresh_page
  eaf-proxy-insert_or_save_as_bookmark
  eaf-proxy-insert_or_save_as_pdf
  eaf-proxy-insert_or_save_as_single_file
  eaf-proxy-insert_or_scroll_down
  eaf-proxy-insert_or_scroll_down_page
  eaf-proxy-insert_or_scroll_left
  eaf-proxy-insert_or_scroll_right
  eaf-proxy-insert_or_scroll_to_begin
  eaf-proxy-insert_or_scroll_to_bottom
  eaf-proxy-insert_or_scroll_up
  eaf-proxy-insert_or_scroll_up_page
  eaf-proxy-insert_or_select_left_tab
  eaf-proxy-insert_or_select_right_tab
  eaf-proxy-insert_or_switch_to_reader_mode
  eaf-proxy-insert_or_toggle_device
  eaf-proxy-insert_or_translate_text
  eaf-proxy-insert_or_view_source
  eaf-proxy-insert_or_zoom_in
  eaf-proxy-insert_or_zoom_out
  eaf-proxy-insert_or_zoom_reset
  eaf-proxy-kill_text
  eaf-proxy-new_blank_page
  eaf-proxy-open_devtools
  eaf-proxy-open_downloads_setting
  eaf-proxy-open_link
  eaf-proxy-open_link_background_buffer
  eaf-proxy-open_link_new_buffer
  eaf-proxy-redo_action
  eaf-proxy-refresh_page
  eaf-proxy-save_page_password
  eaf-proxy-scroll_down
  eaf-proxy-scroll_down_page
  eaf-proxy-scroll_left
  eaf-proxy-scroll_right
  eaf-proxy-scroll_to_begin
  eaf-proxy-scroll_to_bottom
  eaf-proxy-scroll_up
  eaf-proxy-scroll_up_page
  eaf-proxy-search_text_backward
  eaf-proxy-search_text_forward
  eaf-proxy-select_all_or_input_text
  eaf-proxy-select_text
  eaf-proxy-toggle_adblocker
  eaf-proxy-toggle_dark_mode
  eaf-proxy-toggle_password_autofill
  eaf-proxy-undo_action
  eaf-proxy-yank_text
  eaf-proxy-zoom_in
  eaf-proxy-zoom_out
  eaf-proxy-zoom_reset
  eaf-restart-process
  eaf-search-it
  eaf-send-alt-backspace-sequence
  eaf-send-ctrl-return-sequence
  eaf-send-down-key
  eaf-send-key
  eaf-send-key-sequence
  eaf-send-left-key
  eaf-send-return-key
  eaf-send-right-key
  eaf-send-second-key-sequence
  eaf-send-shift-return-sequence
  eaf-send-up-key
  eaf-share-path-or-url
  eaf-stop-process
  eaf-toggle-fullscreen
  )

(defadvice eaf-py-proxy-insert_or_export_text (after emacspeak pre act comp)
  "Speak resulting buffer."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Exporting web page as text.")))

(provide 'emacspeak-eaf)
;;;  end of file


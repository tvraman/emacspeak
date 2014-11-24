;;; emacspeak-eww.el --- Speech-enable EWW
;;; $Id: emacspeak-eww.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description: Speech-enable EWW An Emacs Interface to eww
;;; Keywords: Emacspeak, Audio Desktop eww
;;{{{ LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;; $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{ Copyright:
;;;Copyright (C) 1995 -- 2007, 2011, T. V. Raman
;;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
;;; All Rights Reserved.
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNEWW FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING. If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ introduction

;;; Commentary:
;;; EWW == Emacs Web Browser
;;; EWW is a light-weight Web browser built into Emacs 24.4.
;;; This module speech-enables EWW

;;; Code:
;;}}}
;;{{{ Required modules

(require 'cl)

(declaim (optimize (safety 0) (speed 3)))

(eval-when-compile (require 'eww "eww" 'no-error))

(eval-when-compile (require 'emacspeak-feeds "emacspeak-feeds" 'no-error))

(require 'emacspeak-preamble)

(require 'emacspeak-we)

(require 'emacspeak-webutils)

(require 'emacspeak-google)

(require 'xml)
;;}}}
;;{{{ Compatibility Helpers:

;;; For compatibility between Emacs 24 and Emacs 25
;;; eww in emacs-24 used eww-current-title etc as variables.
;;; eww in emacs 25 groups these as properties on eww-data.
;;; Emacspeak-eww defines wrapper functions to hide this difference.

(loop
 for name in
 '(title url source dom)
 do
 (cond
  ((boundp 'eww-data)
   (eval
    `(defsubst
       ,(intern (format "emacspeak-eww-current-%s" name)) ()
       , (format "Return eww-current-%s." name)
         (declare (special eww-data))
         (plist-get eww-data
                    ,(intern (format ":%s" name))))))
  (t
   (eval
    `(defsubst
       ,(intern (format "emacspeak-eww-current-%s" name))
       ()
       , (format "Return eww-current-%s." name)
         ,(intern (format "eww-current-%s" name)))))))

;;}}}
;;{{{ Inline Helpers:

(defsubst emacspeak-eww-prepare-eww ()
  "Ensure that we are in an EWW buffer that is well set up."
  (declare (special major-mode  emacspeak-eww-cache-updated))
  (unless (eq major-mode 'eww-mode) (error "Not in EWW buffer."))
  (unless (emacspeak-eww-current-dom) (error "No DOM!"))
  (unless emacspeak-eww-cache-updated (eww-update-cache (emacspeak-eww-current-dom))))

(defsubst emacspeak-eww-post-render-actions ()
  "Post-render actions for setting up emacspeak."
  (emacspeak-eww-prepare-eww)
  (emacspeak-pronounce-toggle-use-of-dictionaries t))

;;}}}
;;{{{ Setup EWW Initialization:

;;; Inform emacspeak-webutils about EWW:

(add-hook
 'eww-mode-hook
 #'(lambda ()
     (setq
      emacspeak-webutils-document-title (emacspeak-eww-current-title)
      emacspeak-webutils-url-at-point
      #'(lambda ()
          (let ((url (get-text-property (point) 'help-echo)))
            (cond
             ((and url
                   (stringp url)
                   (string-prefix-p
                    (emacspeak-google-result-url-prefix) url))
              (emacspeak-google-canonicalize-result-url url))
             ((and url (stringp url))url)
             (t (error "No URL under point.")))))
      emacspeak-webutils-current-url (emacspeak-eww-current-url))))

(defvar emacspeak-eww-masquerade t
  "Says if we masquerade as a mainstream browser.")

(defun emacspeak-eww-masquerade ()
  "Toggle masquerade state."
  (interactive)
  (declare (special emacspeak-eww-masquerade))
  (setq emacspeak-eww-masquerade (not emacspeak-eww-masquerade))
  (message "Turned %s masquerade"
           (if emacspeak-eww-masquerade "on" "off"))
  (emacspeak-auditory-icon
   (if emacspeak-eww-masquerade 'on 'off)))

(defcustom  emacspeak-eww-masquerade-as
  (format "User-Agent: %s %s %s\r\n"
          "Mozilla/5.0 (X11; Linux x86_64)"
          "AppleWebKit/537.36 (KHTML, like Gecko)"
          "Chrome/36.0.1964.2 Safari/537.36")
  "User Agent string that is  sent when masquerading is on."
  :type 'string
  :group 'emacspeak-eww)

;;; Advice note: Setting ad-return-value in one arm of the cond appears to perculate to both arms.

(defadvice url-http-user-agent-string (around emacspeak pre act comp)
  "Respond to user  asking us to masquerade."
  (cond
   ((and emacspeak-eww-masquerade
         (eq browse-url-browser-function 'eww-browse-url))
    (setq ad-return-value emacspeak-eww-masquerade-as))
   (t (setq ad-return-value "User-Agent: URL/Emacs \r\n"))))

(defun emacspeak-eww-setup ()
  "Setup keymaps etc."
  (declare (special eww-mode-map eww-link-keymap
                    shr-inhibit-images emacspeak-pronounce-common-xml-namespace-uri-pronunciations
                    emacspeak-eww-masquerade emacspeak-pronounce-load-pronunciations-on-startup))
  (when emacspeak-pronounce-load-pronunciations-on-startup
    (emacspeak-pronounce-augment-pronunciations
     'eww-mode emacspeak-pronounce-common-xml-namespace-uri-pronunciations)
    (emacspeak-pronounce-add-dictionary-entry
     'eww-mode
     emacspeak-speak-rfc-3339-datetime-pattern
     (cons 're-search-forward 'emacspeak-speak-decode-rfc-3339-datetime)))
;;; turn off images
  (setq shr-inhibit-images t)
                                        ; remove "I" "o" from
                                        ; eww-link-keymap
  (loop
   for c in
   '(?I ?o)
   do
   (when (assoc  c eww-link-keymap)
     (delete (assoc  c eww-link-keymap) eww-link-keymap)))

  (define-key eww-link-keymap  "k" 'shr-copy-url)
  (loop
   for binding  in
   '(
     ( "\C-t" emacspeak-google-command)
     ("'" emacspeak-speak-rest-of-buffer)
     ("*" eww-add-bookmark)
     ("," emacspeak-eww-previous-h)
     ("." emacspeak-eww-next-h)
     ("/" search-forward)
     ("1" emacspeak-eww-next-h1)
     ("2" emacspeak-eww-next-h2)
     ("3" emacspeak-eww-next-h3)
     ("=" dtk-toggle-punctuation-mode)
     ("?" emacspeak-webutils-google-similar-to-this-page)
     ("A" eww-view-dom-having-attribute)
     ("C" eww-view-dom-having-class)
     ("E" eww-view-dom-having-elements)
     ("G" emacspeak-google-command)
     ("I" eww-view-dom-having-id)
     ("J" emacspeak-eww-next-element-like-this)
     ("K" emacspeak-eww-previous-element-like-this)
     ("N" emacspeak-eww-next-element-from-history)
     ("O" emacspeak-eww-previous-li)
     ("P" emacspeak-eww-previous-element-from-history)
     ("Q" emacspeak-kill-buffer-quietly)
     ("R" eww-view-dom-having-role)
     ("T" emacspeak-eww-previous-table)
     ("[" emacspeak-eww-previous-p)
     ("\;" emacspeak-webutils-play-media-at-point)
     ("\C-e" emacspeak-prefix-command)
     ("\M- " emacspeak-eww-speak-this-element)
     ("\M-1" emacspeak-eww-previous-h1)
     ("\M-2" emacspeak-eww-previous-h2)
     ("\M-3" emacspeak-eww-previous-h3)
     ("\M-a" eww-view-dom-not-having-attribute)
     ("\M-c" eww-view-dom-not-having-class)
     ("\M-e" eww-view-dom-not-having-element-list)
     ("\M-i" eww-view-dom-not-having-id)
     ("\M-r" eww-view-dom-not-having-role)
     ("\d" emacspeak-eww-restore)
     ("]" emacspeak-eww-next-p)
     ("b" shr-previous-link)
     ("e" emacspeak-we-xsl-map)
     ("f" shr-next-link)
     ("k" eww-copy-page-url)
     ("n" emacspeak-eww-next-element)
     ("o" emacspeak-eww-next-li)
     ("p" emacspeak-eww-previous-element)
     ("t" emacspeak-eww-next-table)
     )
   do
   (emacspeak-keymap-update eww-mode-map binding)))

(when (boundp 'eww-mode-map) (emacspeak-eww-setup))

;;}}}
;;{{{ Map Faces To Voices:

(voice-setup-add-map
 '(
   (eww-form-submit voice-animate)
   (eww-form-checkbox voice-monotone)
   (eww-form-select voice-annotate)
   (eww-form-text voice-lighten)))

;;}}}
;;{{{ Advice Interactive Commands:

(loop
 for f in
 '(eww-up-url eww-top-url
              eww-next-url eww-previous-url
              eww-back-url eww-forward-url)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback"
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)
       (dtk-speak (emacspeak-eww-current-title))))))

(defvar emacspeak-eww-style nil
  "Record if we applied an  xsl style in this buffer.")

(make-variable-buffer-local 'emacspeak-eww-style)

(defvar emacspeak-eww-feed nil
  "Record if this eww buffer is displaying a feed.")

(make-variable-buffer-local 'emacspeak-eww-feed)

(defvar emacspeak-eww-url-template nil
  "Record if this eww buffer is displaying a url-template.")

(make-variable-buffer-local 'emacspeak-eww-url-template)

(defvar emacspeak-eww-buffer-hash (make-hash-table  :test #'equal )
  "Table storing eww buffer handles hashed by URL.")

;;;Check cache if URL already open, otherwise cache.

(defadvice eww (around emacspeak pre act comp)
  "Check cache, if already open, switch to existing buffer.
Otherwise proceed  and cache the buffer at the end of eww-render. "
  (let* ((this-url (ad-get-arg 1))
         (handle  (gethash  this-url emacspeak-eww-buffer-hash)))
    (cond
     ((and handle (buffer-live-p handle))
      (switch-to-buffer handle))
     (t                                ; proceed
      (emacspeak-webutils-autospeak)
      ad-do-it))
    ad-return-value))

(defadvice eww-reload (around emacspeak pre act comp)
  "Check buffer local settings for feed buffers.
If buffer was result of displaying a feed, reload feed.
If we came from a url-template, reload that template.
Retain previously set punctuations  mode."
  (add-hook 'emacspeak-web-post-process-hook 'emacspeak-eww-post-render-actions)
  (cond
   ((and (emacspeak-eww-current-url)
         emacspeak-eww-feed
         emacspeak-eww-style)
                                        ; this is a displayed feed
    (lexical-let
        ((p dtk-punctuation-mode)
         (r dtk-speech-rate)
         (u (emacspeak-eww-current-url) )
         (s emacspeak-eww-style))
      (kill-buffer)
      (add-hook
       'emacspeak-web-post-process-hook
       #'(lambda ()
           (dtk-set-punctuations p)
           (dtk-set-rate r)
           (emacspeak-dtk-sync))
       'at-end)
      (emacspeak-feeds-feed-display u s 'speak)))
   ((and (emacspeak-eww-current-url) emacspeak-eww-url-template)
                                        ; this is a url template
    (lexical-let
        ((n emacspeak-eww-url-template)
         (p dtk-punctuation-mode)
         (r dtk-speech-rate))
      (add-hook
       'emacspeak-web-post-process-hook
       #'(lambda nil
           (dtk-set-punctuations p)
           (dtk-set-rate r)
           (emacspeak-dtk-sync))
       'at-end)
      (kill-buffer)
      (emacspeak-url-template-open (emacspeak-url-template-get  n))))
   (t ad-do-it)))

(loop
 for f in
 '(eww eww-reload eww-open-file)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback"
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)))))

(defvar emacspeak-eww-rename-result-buffer t
  "Result buffer is renamed to document title.")

(defun emacspeak-eww-after-render-hook ()
  "Setup Emacspeak for rendered buffer. "
  (declare (special emacspeak-eww-cache-updated emacspeak-eww-buffer-hash))
  (let ((title (emacspeak-eww-current-title)))
    (when emacspeak-eww-rename-result-buffer
      (when (= 0 (length title)) (setq title "EWW: Untitled"))
      (rename-buffer title 'unique)))
  (puthash  (emacspeak-eww-current-url) (current-buffer)emacspeak-eww-buffer-hash)
  (unless emacspeak-web-post-process-hook (emacspeak-speak-mode-line))
  (emacspeak-webutils-run-post-process-hook)
  (when (eq major-mode 'eww-mode)
    (eww-update-header-line-format)))

(cond
 ((boundp  'eww-after-render-hook) ; emacs 25
  ; temporary solution since eww-after-render-hook is called too early by EWW
  (defadvice shr-insert-document (after emacspeak pre act comp)
    (emacspeak-eww-after-render-hook))
  )
 (t
  (defadvice eww-render (after emacspeak pre act comp)
    "Setup Emacspeak for rendered buffer."
    (emacspeak-eww-after-render-hook))))
    
;;(when (symbolp 'eww-after-render-hook)
  ;(add-hook 'eww-after-render-hook
            ;'emacspeak-eww-after-render-hook)))

(defadvice eww-add-bookmark (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'mark-object)))

(defadvice eww-beginning-of-text (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'large-movement)))

(defadvice eww-end-of-text(after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'mark-object)))

(defadvice eww-bookmark-browse (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'open-object)))

(defadvice eww-bookmark-kill (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'delete-object)))

(defadvice eww-bookmark-quit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'close-object)))

(defadvice eww-bookmark-yank(after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'yank-object)))

(defadvice eww-list-bookmarks(after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'open-object)))

(loop
 for f in
 '(eww-next-bookmark eww-previous-bookmark)
 do
 (eval
  `(defadvice ,f(after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p) (emacspeak-auditory-icon 'select-object))
     (emacspeak-speak-line))))

(defadvice eww-quit(after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'close-object)))

(loop
 for f in
 '(eww-change-select
   eww-toggle-checkbox
   eww-submit)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'button)))))

(loop
 for f in
 '(shr-next-link shr-previous-link)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "Provide auditory feedback."
     (let ((emacspeak-speak-messages nil))
       ad-do-it
       (when (ems-interactive-p)
         (emacspeak-auditory-icon 'large-movement)
         (emacspeak-speak-region
          (point)
          (next-single-property-change (point) 'help-echo  nil (point-max))))))))

;;; Handle emacspeak-we-url-executor

(defadvice eww-follow-link (around emacspeak pre act comp)
  "Respect emacspeak-we-url-executor if set."
  (emacspeak-auditory-icon 'button)
  (cond
   ((and (ems-interactive-p)
         (boundp 'emacspeak-we-url-executor)
         (fboundp emacspeak-we-url-executor))
    (let ((url (get-text-property (point) 'shr-url)))
      (unless url (error "No URL  under point"))
      (funcall emacspeak-we-url-executor url)))
   (t ad-do-it)))

;;}}}
;;{{{ xslt transform on request:

(defadvice eww-display-html (before emacspeak pre act comp)
  "Apply XSLT transform if requested."
  (let ((orig (point)))
    (when (and emacspeak-we-xsl-p emacspeak-we-xsl-transform)
      (emacspeak-xslt-region
       emacspeak-we-xsl-transform (point) (point-max)
       emacspeak-we-xsl-params))
    (goto-char orig)))

;;}}}
;;{{{ DOM Structure In Rendered Buffer:

(loop
 for  tag in
 '(h1 h2 h3 div                    ; sectioning
      ul ol dl                     ; Lists
      li dt dd p                   ; block-level: bullets, paras
      form blockquote              ; block-level
      a b it em span               ; in-line
      br hr                        ; separators
      th tr table )
 do
 (eval
  `
  (defadvice  ,(intern (format "shr-tag-%s" tag)) (around emacspeak pre act comp)
    (let ((start (point)))
      ad-do-it
      (let ((start (if (char-equal (following-char) ?\n)
                       (min (point-max) (1+ start) )start))
            (end (if (> (point) start) (1- (point)) (point))))
        (put-text-property start end
                           (quote ,tag) 'eww-tag)
        (when (memq (quote ,tag) '(h1 h2 h3))
          (put-text-property start end 'h 'eww-tag)))))))

;;}}}
;;{{{  Customize image loading:

(defcustom emacspeak-eww-silence-images t
  "Set to nil if you want EWW to load images."
  :type 'boolean
  :group 'emacspeak-eww)

(defadvice eww-display-image (around emacspeak pre act comp)
  "Dont load images if asked to silence them."
  (unless emacspeak-eww-silence-images ad-do-it))

;;}}}
;;{{{ element, class, role, id caches:

(defvar emacspeak-eww-cache-updated nil
  "Records if caches are updated.")

(make-variable-buffer-local 'emacspeak-eww-cache-updated)

;;; Mark cache to be dirty if we restore history:

(defadvice eww-restore-history (after emacspeak pre act comp)
  "mark cache dirty."
  (setq emacspeak-eww-cache-updated nil)
  (emacspeak-eww-prepare-eww))

(defvar eww-id-cache nil
  "Cache of id values. Is buffer-local.")

(make-variable-buffer-local 'eww-id-cache)

(defvar eww-class-cache nil
  "Cache of class values. Is buffer-local.")

(make-variable-buffer-local 'eww-class-cache)

(defvar eww-role-cache nil
  "Cache of role values. Is buffer-local.")

(make-variable-buffer-local 'eww-role-cache)
;;; Holds element names as strings.

(defvar eww-element-cache nil
  "Cache of element names. Is buffer-local.")

(make-variable-buffer-local 'eww-element-cache)

(defun eww-update-cache (dom)
  "Update element, role, class and id cache."
  (declare (special eww-element-cache eww-id-cache
                    eww-role-cache eww-class-cache emacspeak-eww-cache-updated))
  (when (listp dom) ; build cache
    (let ((id (xml-get-attribute-or-nil dom 'id))
          (class (xml-get-attribute-or-nil dom 'class))
          (role (xml-get-attribute-or-nil dom 'role))
          (el (symbol-name (xml-node-name dom)))
          (children (xml-node-children dom)))
      (when id (pushnew id eww-id-cache))
      (when class (pushnew class eww-class-cache))
      (when role (pushnew role eww-role-cache))
      (when el (pushnew el eww-element-cache))
      (when children (mapc #'eww-update-cache children)))
    (setq emacspeak-eww-cache-updated t)))

;;}}}
;;{{{ Filter DOM:

(defvar eww-shr-render-functions
  '((title . eww-tag-title)
    (form . eww-tag-form)
    (input . eww-tag-input)
    (textarea . eww-tag-textarea)
    (body . eww-tag-body)
    (select . eww-tag-select)
    (link . eww-tag-link)
    (a . eww-tag-a))
  "Customize shr rendering for EWW.")

(defun eww-dom-keep-if (dom predicate)
  "Return filtered DOM  keeping nodes that match  predicate.
 Predicate receives the node to test."
  (cond
   ((not (listp dom)) nil)
   ((funcall predicate dom) dom)
   (t
    (let ((filtered
           (delq nil
                 (mapcar
                  #'(lambda (node) (eww-dom-keep-if node predicate))
                  (xml-node-children dom)))))
      (when filtered
        (push (xml-node-attributes dom) filtered)
        (push (xml-node-name dom) filtered))))))

(defun eww-dom-remove-if (dom predicate)
  "Return filtered DOM  dropping  nodes that match  predicate.
 Predicate receives the node to test."
  (cond
   ((not (listp dom)) dom)
   ((funcall predicate dom) nil)
   (t
    (let
        ((filtered
          (delq nil
                (mapcar #'(lambda (node) (eww-dom-remove-if  node predicate))
                        (xml-node-children dom)))))
      (when filtered
        (push (xml-node-attributes dom) filtered)
        (push (xml-node-name dom) filtered))))))

(defun eww-attribute-list-tester (attr-list)
  "Return predicate that tests for attr=value from members of
attr-value list for use as a DOM filter."
  (eval
   `#'(lambda (node)
        (let (attr  value found)
          (loop
           for pair in (quote ,attr-list)
           until found
           do
           (setq attr (first pair)
                 value (second pair))
           (setq found (string= (xml-get-attribute node attr) value)))
          (when found node)))))

(defun eww-attribute-tester (attr value)
  "Return predicate that tests for attr=value for use as a DOM filter."
  (eval
   `#'(lambda (node)
        (when
            (string= (xml-get-attribute node (quote ,attr)) ,value) node))))

(defun eww-elements-tester (element-list)
  "Return predicate that tests for presence of element in element-list
for use as a DOM filter."
  (eval
   `#'(lambda (node)
        (when (memq (xml-node-name node) (quote ,element-list)) node))))

(defun emacspeak-eww-view-helper  (filtered-dom)
  "View helper called by various filtering viewers."
  (declare (special emacspeak-eww-rename-result-buffer eww-shr-render-functions ))
  (let ((emacspeak-eww-rename-result-buffer nil)
        (inhibit-read-only t)
        (shr-external-rendering-functions eww-shr-render-functions))
    (eww-save-history)
    (erase-buffer)
    (goto-char (point-min))
    (shr-insert-document filtered-dom)
    (set-buffer-modified-p nil)
    (flush-lines "^ *$")
    (goto-char (point-min))
    (setq buffer-read-only t))
  (emacspeak-auditory-icon 'open-object)
  (emacspeak-speak-buffer))

(defun ems-eww-read-list (reader)
  "Return list of values  read using reader."
  (let (value-list  value done)
    (loop
     until done
     do
     (setq value (funcall reader))
     (cond
      (value (pushnew   value value-list))
      (t (setq done t))))
    value-list))

(defsubst ems-eww-read-id ()
  "Return id value read from minibuffer."
  (declare (special eww-id-cache))
  (unless eww-id-cache (error "No id to filter."))
  (let ((value (completing-read "Value: " eww-id-cache nil 'must-match)))
    (unless (zerop (length value)) value)))

(defun eww-view-dom-having-id (multi)
  "Display DOM filtered by specified id=value test.
Optional interactive arg `multi' prompts for multiple classes."
  (interactive "P")
  (emacspeak-eww-prepare-eww)
  (let
      ((dom
        (eww-dom-keep-if
         (emacspeak-eww-current-dom)
         (eww-attribute-list-tester
          (if multi
              (loop
               for i in (ems-eww-read-list 'ems-eww-read-id)
               collect (list 'id i))
            (list (list 'id (ems-eww-read-id))))))))
    (when dom (emacspeak-eww-view-helper dom))))

(defun eww-view-dom-not-having-id (multi)
  "Display DOM filtered by specified nodes not passing  id=value test.
Optional interactive arg `multi' prompts for multiple classes."
  (interactive "P")
  (emacspeak-eww-prepare-eww)
  (let ((dom
         (eww-dom-remove-if
          (emacspeak-eww-current-dom)
          (eww-attribute-list-tester
           (if multi
               (loop
                for i in (ems-eww-read-list 'ems-eww-read-id)
                collect (list 'id i))
             (list (list 'id (ems-eww-read-id))))))))
    (when dom (emacspeak-eww-view-helper dom))))

(defun ems-eww-read-attribute-and-value ()
  "Read attr-value pair and return as a list."
  (declare (special eww-id-cache eww-class-cache eww-role-cache))
  (unless (or eww-role-cache eww-id-cache eww-class-cache)
    (error "No attributes to filter."))
  (let(attr-names attr value)
    (when eww-class-cache (push "class" attr-names))
    (when eww-id-cache (push "id" attr-names))
    (when eww-role-cache (push "role" attr-names))
    (setq attr (completing-read "Attr: " attr-names nil 'must-match))
    (unless (zerop (length attr))
      (setq attr (intern attr))
      (setq value
            (completing-read
             "Value: "
             (cond
              ((eq attr 'id) eww-id-cache)
              ((eq attr 'class)eww-class-cache)
              ((eq attr 'role)eww-role-cache))
             nil 'must-match))
      (list attr value))))

(defun eww-view-dom-having-attribute (multi)
  "Display DOM filtered by specified attribute=value test.
Optional interactive arg `multi' prompts for multiple classes."
  (interactive "P")
  (emacspeak-eww-prepare-eww)
  (let ((dom
         (eww-dom-keep-if
          (emacspeak-eww-current-dom)
          (eww-attribute-list-tester
           (if multi
               (ems-eww-read-list 'ems-eww-read-attribute-and-value)
             (list  (ems-eww-read-attribute-and-value)))))))
    (when dom (emacspeak-eww-view-helper dom))))

(defun eww-view-dom-not-having-attribute (multi)
  "Display DOM filtered by specified nodes not passing  attribute=value test.
Optional interactive arg `multi' prompts for multiple classes."
  (interactive "P")
  (emacspeak-eww-prepare-eww)
  (let ((dom
         (eww-dom-remove-if
          (emacspeak-eww-current-dom)
          (eww-attribute-list-tester
           (if multi
               (ems-eww-read-list 'ems-eww-read-attribute-and-value)
             (list  (ems-eww-read-attribute-and-value)))))))
    (when dom (emacspeak-eww-view-helper dom))))

(defsubst ems-eww-read-class ()
  "Return class value read from minibuffer."
  (declare (special eww-class-cache))
  (unless eww-class-cache (error "No class to filter."))
  (let ((value (completing-read "Value: " eww-class-cache nil 'must-match)))
    (unless (zerop (length value)) value)))

(defun eww-view-dom-having-class (multi)
  "Display DOM filtered by specified class=value test.
Optional interactive arg `multi' prompts for multiple classes."
  (interactive "P")
  (emacspeak-eww-prepare-eww)
  (let ((dom
         (eww-dom-keep-if
          (emacspeak-eww-current-dom)
          (eww-attribute-list-tester
           (if multi
               (loop
                for c in (ems-eww-read-list 'ems-eww-read-class)
                collect (list 'class c))
             (list (list 'class (ems-eww-read-class))))))))
    (when dom (emacspeak-eww-view-helper dom))))

(defun eww-view-dom-not-having-class (multi)
  "Display DOM filtered by specified nodes not passing   class=value test.
Optional interactive arg `multi' prompts for multiple classes."
  (interactive "P")
  (emacspeak-eww-prepare-eww)
  (let ((dom
         (eww-dom-remove-if
          (emacspeak-eww-current-dom)
          (eww-attribute-list-tester
           (if multi
               (loop
                for c in (ems-eww-read-list 'ems-eww-read-class)
                collect (list 'class c))
             (list (list 'class (ems-eww-read-class))))))))
    (when dom (emacspeak-eww-view-helper dom))))

(defsubst ems-eww-read-role ()
  "Return role value read from minibuffer."
  (declare (special eww-role-cache))
  (unless eww-role-cache (error "No role to filter."))
  (let ((value (completing-read "Value: " eww-role-cache nil 'must-match)))
    (unless (zerop (length value)) value)))

(defun eww-view-dom-having-role (multi)
  "Display DOM filtered by specified role=value test.
Optional interactive arg `multi' prompts for multiple classes."
  (interactive "P")
  (emacspeak-eww-prepare-eww)
  (let ((dom
         (eww-dom-keep-if
          (emacspeak-eww-current-dom)
          (eww-attribute-list-tester
           (if multi
               (loop
                for r in (ems-eww-read-list 'ems-eww-read-role)
                collect (list 'role r))
             (list (list 'role (ems-eww-read-role))))))))
    (when dom (emacspeak-eww-view-helper dom))))

(defun eww-view-dom-not-having-role (multi)
  "Display DOM filtered by specified  nodes not passing   role=value test.
Optional interactive arg `multi' prompts for multiple classes."
  (interactive "P")
  (declare (special  eww-shr-render-functions ))
  (emacspeak-eww-prepare-eww)
  (let ((dom
         (eww-dom-remove-if
          (emacspeak-eww-current-dom)
          (eww-attribute-list-tester
           (if multi
               (loop
                for r in (ems-eww-read-list 'ems-eww-read-role)
                collect (list 'role r))
             (list (list 'role (ems-eww-read-role))))))))
    (when dom (emacspeak-eww-view-helper dom))))

(defsubst ems-eww-read-element ()
  "Return element  value read from minibuffer."
  (declare (special eww-element-cache))
  (let ((value (completing-read "Value: " eww-element-cache nil 'must-match)))
    (unless (zerop (length value)) (intern value))))

(defun eww-view-dom-having-elements (&optional multi)
  "Display DOM filtered by specified elements.
Optional interactive prefix arg `multi' prompts for multiple elements."
  (interactive "P")
  (emacspeak-eww-prepare-eww)
  (let ((dom
         (eww-dom-keep-if
          (emacspeak-eww-current-dom)
          (eww-elements-tester
           (if multi
               (ems-eww-read-list 'ems-eww-read-element)
             (list  (ems-eww-read-element)))))))
    (cond
     (dom (emacspeak-eww-view-helper dom))
     (t (message "Filtering failed.")))))

(defun eww-view-dom-not-having-element-list (multi)
  "Display DOM filtered by specified nodes not passing   el list.
Optional interactive prefix arg `multi' prompts for multiple elements."
  (interactive "P")
  (emacspeak-eww-prepare-eww)
  (let ((dom
         (eww-dom-remove-if
          (emacspeak-eww-current-dom)
          (eww-elements-tester
           (if multi
               (ems-eww-read-list 'ems-eww-read-element)
             (list (intern (ems-eww-read-element))))))))
    (when dom (emacspeak-eww-view-helper dom))))

(defun emacspeak-eww-restore ()
  "Restore buffer to pre-filtered canonical state."
  (interactive)
  (declare (special eww-history eww-history-position))
  (eww-restore-history(elt eww-history eww-history-position))
  (emacspeak-speak-mode-line)
  (emacspeak-auditory-icon 'open-object))

;;}}}
;;{{{ Element Navigation:
;;; Try only storing symbols, not strings.

(defvar emacspeak-eww-element-navigation-history nil
  "History for element navigation.")

(defun emacspeak-eww-next-element (el)
  "Move forward to the next specified element."
  (interactive
   (list
    (progn
      (emacspeak-eww-prepare-eww)
      (intern
       (completing-read "Element: "
                        eww-element-cache nil 'must-match
                        nil 'emacspeak-eww-element-navigation-history)))))
  (declare (special eww-element-cache emacspeak-eww-element-navigation-history))
  (let*
      ((start
        (or
         (when (get-text-property (point) el)
           (next-single-property-change (point) el ))
         (point)))
       (next (next-single-property-change start  el )))
    (cond
     (next
      (goto-char next)
      (setq emacspeak-eww-element-navigation-history (delq el emacspeak-eww-element-navigation-history))
      (push  el emacspeak-eww-element-navigation-history)
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-speak-region next (next-single-property-change next el)))
     (t (message "No next %s" el)))))

(defun emacspeak-eww-previous-element (el)
  "Move backward  to the previous  specified element."
  (interactive
   (list
    (progn
      (emacspeak-eww-prepare-eww)
      (intern (completing-read "Element: " eww-element-cache nil 'must-match
                               nil 'emacspeak-eww-element-navigation-history)))))
  (declare (special eww-element-cache  emacspeak-eww-element-navigation-history))
  (let* ((start
          (or
           (when (get-text-property  (point) el)
             (previous-single-property-change (1+ (point)) el ))
           (point)))
         (previous (previous-single-property-change  start  el)))
    (cond
     (previous
      (goto-char (or (previous-single-property-change previous el) (point-min)))
      (setq emacspeak-eww-element-navigation-history (delq el emacspeak-eww-element-navigation-history))
      (push  el emacspeak-eww-element-navigation-history)
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-speak-region (point) previous))
     (t (message "No previous  %s" el)))))

(defun emacspeak-eww-next-element-from-history ()
  "Uses element navigation history to decide where we jump."
  (interactive)
  (declare (special emacspeak-eww-element-navigation-history))
  (cond
   (emacspeak-eww-element-navigation-history
    (emacspeak-eww-next-element  (car emacspeak-eww-element-navigation-history)))
   (t (error "No elements in navigation history"))))

(defun emacspeak-eww-previous-element-from-history ()
  "Uses element navigation history to decide where we jump."
  (interactive)
  (declare (special emacspeak-eww-element-navigation-history))
  (cond
   (emacspeak-eww-element-navigation-history
    (emacspeak-eww-previous-element  (car emacspeak-eww-element-navigation-history)))
   (t (error "No elements in navigation history"))))

(defsubst emacspeak-eww-here-tags ()
  "Return list of enclosing tags at point."
  (let* ((eww-tags (text-properties-at (point))))
    (loop
     for i from 0 to (1- (length eww-tags)) by 2
     if (eq (plist-get eww-tags (nth i eww-tags)) 'eww-tag )
     collect (nth i eww-tags))))

(defsubst emacspeak-eww-read-tags-like-this(&optional prompt)
  "Read tag for like-this navigation."
  (let ((tags (emacspeak-eww-here-tags)))
    (cond
     ((null tags) (error "No enclosing element here."))
     ((= 1 (length tags))  (first tags))
     (t (intern
         (completing-read
          (or prompt "Jump to: ")
          (mapcar #'symbol-name tags)
          nil t
          nil emacspeak-eww-element-navigation-history))))))

(defun emacspeak-eww-next-element-like-this (element)
  "Moves to next element like current.
Prompts if content at point is enclosed by multiple elements."
  (interactive
   (list (emacspeak-eww-read-tags-like-this)))
  (emacspeak-eww-next-element  element))

(defun emacspeak-eww-previous-element-like-this (element)
  "Moves to next element like current.
Prompts if content at point is enclosed by multiple elements."
  (interactive
   (list (emacspeak-eww-read-tags-like-this)))
  (emacspeak-eww-previous-element  element))

(defun emacspeak-eww-speak-this-element (element)
  "Speaks  to next element like current.
Uses most recently navigated structural unit.
Otherwise, prompts if content at point is enclosed by multiple elements."
  (interactive
   (list
    (or (car emacspeak-eww-element-navigation-history)
    (emacspeak-eww-read-tags-like-this "Read: "))))
  (let ((start (point)))
    (save-excursion
      (emacspeak-eww-next-element  element)
      (emacspeak-auditory-icon 'select-object)
      (emacspeak-speak-region start (point)))))

(loop
 for  f in
 '(h h1 h2 h3 li table ol ul p)
 do
 (eval
  `(defun ,(intern (format "emacspeak-eww-next-%s" f)) (&optional speak)
     ,(format "Move forward to the next %s.
Optional interactive prefix arg speaks the structural unit." f)
     (interactive "P")
     (funcall 'emacspeak-eww-next-element (intern ,(format "%s" f)))
     (when speak
       (emacspeak-eww-speak-element-like-this (intern ,(format "%s" f))))))
 (eval
  `(defun ,(intern (format "emacspeak-eww-previous-%s" f)) (&optional speak)
     ,(format "Move backward to the next %s.
Optional interactive prefix arg speaks the structural unit." f)
     (interactive "P")
     (funcall 'emacspeak-eww-previous-element (intern ,(format "%s" f)))
     (when speak
       (emacspeak-eww-speak-element-like-this (intern ,(format "%s" f)))))))

;;}}}
;;{{{ Google Search  fixes:

(loop
 for f in
 '(url-retrieve-internal  url-truncate-url-for-viewing eww)
 do
 (eval
  `
  (defadvice ,f (before cleanup-url  pre act comp)
    "Canonicalize Google search URLs."
    (let ((u (ad-get-arg 0)))
      (cond
       ((and u (stringp u)
             (string-prefix-p (emacspeak-google-result-url-prefix) u))
        (ad-set-arg 0 (emacspeak-google-canonicalize-result-url u))))))))

(defadvice shr-copy-url (around emacspeak pre act comp)
  "Canonicalize Google URLs"
  ad-do-it
  (when (ems-interactive-p)
    (let ((u (car kill-ring)))
      (when
          (and u (stringp u)
               (string-prefix-p (emacspeak-google-result-url-prefix) u))
        (kill-new  (emacspeak-google-canonicalize-result-url u))))))

;;}}}
;;{{{ Masquerade

;;}}}
;;{{{  Google Knowledge Card:

(defun emacspeak-eww-google-knowledge-card ()
  "Show just the knowledge card.
Warning, this is fragile, and depends on a stable id for the
  knowledge card."
  (interactive)
  (declare (special eww-shr-render-functions emacspeak-eww-masquerade))
  (unless emacspeak-eww-masquerade
    (error "Repeat search after turning on masquerade mode to see knowledge cards."))
  (unless (eq major-mode 'eww-mode)
    (error "This command is only available in EWW"))
  (unless  emacspeak-google-toolbelt
    (error "This doesn't look like a Google results page."))
  (let*
      ((emacspeak-eww-rename-result-buffer nil)
       (value "rhs")
       (media "rg_meta")
       (inhibit-read-only t)
       (dom
        (eww-dom-remove-if
         (eww-dom-keep-if (emacspeak-eww-current-dom) (eww-attribute-tester 'id value))
         (eww-attribute-tester 'class media)))
       (shr-external-rendering-functions eww-shr-render-functions))
    (cond
     (dom
      (eww-save-history)
      (erase-buffer)
      (goto-char (point-min))
      (shr-insert-document dom)
      (set-buffer-modified-p nil)
      (flush-lines "^ *$")
      (goto-char (point-min))
      (setq buffer-read-only t)
      (emacspeak-speak-buffer))
     (t (message "Knowledge Card not found.")))    (emacspeak-auditory-icon 'open-object)))

(define-key emacspeak-google-keymap "k" 'emacspeak-eww-google-knowledge-card)
(define-key emacspeak-google-keymap "e" 'emacspeak-eww-masquerade)
;;}}}

(provide 'emacspeak-eww)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

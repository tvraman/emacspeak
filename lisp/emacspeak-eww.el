;;; emacspeak-eww.el --- Speech-enable EWW Browser  -*- lexical-binding: t; -*-
;;; $Id: emacspeak-eww.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description: Speech-enable EWW An Emacs Interface to eww
;;; Keywords: Emacspeak, Audio Desktop eww
;;{{{ LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;; $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{ Copyright:

;;;Copyright (C) 1995 -- 2021, T. V. Raman
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ introduction

;;; Commentary:

;;;EWW == Emacs Web Browser
;;;
;;; EWW is a light-weight Web browser built into Emacs starting with
;;; Emacs-24.4 . This module speech-enables EWW.
;;;
;;;It implements additional interactive commands for navigating the
;;; DOM. It also provides a set of filters for interactively filtering
;;; the DOM by various attributes such as id, class and role.
;;; Finally, this module updates EWW's built-in key-bindings with
;;; Emacspeak conveniences.

;;; @subsection Structured Navigation
;;;
;;; These commands move through section headers as defined in HTML.
;;;@table @kbd
;;;@item       1
;;;@command{emacspeak-eww-next-h1}
;;;Move to next @code{H1} heading.
;;;@item       2
;;;@command{emacspeak-eww-next-h2}
;;;Move to next @code{H2} heading.
;;;@item       3
;;;@command{emacspeak-eww-next-h3}
;;;Move to next @code{H3} heading.
;;;@item       4
;;;@command{emacspeak-eww-next-h4}
;;;Move to next @code{H4} heading.
;;;@item       .
;;;@command{emacspeak-eww-next-h}
;;;Move to next heading. (@code{H1}...@code{H4}).
;;;@item       M-1
;;;@command{emacspeak-eww-previous-h1}
;;;Move to previous @code{H1} heading.
;;;@item       M-2
;;;@command{emacspeak-eww-previous-h2}
;;;Move to previous @code{H2} heading.
;;;@item       M-3
;;;@command{emacspeak-eww-previous-h3}
;;;Move to previous @code{H3} heading.
;;;@item       M-4
;;;@command{emacspeak-eww-previous-h4}
;;;Move to previous @code{H4} heading.
;;;@item  ,
;;;@command{emacspeak-eww-previous-h}
;;;Move to previous heading (@code{H1}...@code{H4}).
;;;@end table
;;;
;;; This next set of DOM commands enable navigating by HTML elements.
;;;@table @kbd
;;;@item       M-SPC
;;;@command{emacspeak-eww-speak-this-element}
;;; Speak contents of current element.
;;;@item       J
;;;@command{emacspeak-eww-next-element-like-this}
;;;Jump to next element that is the same as the one under point.
;;; If there are multiple HTML elements under point,
;;;prompts for element-name using completion.
;;;@item       K
;;;@command{emacspeak-eww-previous-element-like-this}
;;;Jump to previous element that is the same as the one under point.
;;; If there are multiple HTML elements under point,
;;;prompts for element-name using completion.
;;;@item  N
;;;@command{emacspeak-eww-next-element-from-history}
;;;Jump to next element based on  previous J/K command history.
;;;@item       P
;;;@command{emacspeak-eww-previous-element-from-history}
;;;Jump to previous element based on  previous J/K history.
;;;@item       O
;;;@command{emacspeak-eww-previous-li}
;;;Jump to previous list item.
;;;@item       o
;;;@command{emacspeak-eww-next-li}
;;;Jump to next list item.
;;;@item       T
;;;@command{emacspeak-eww-previous-table}
;;;Jump to previous table in page.
;;;@item  t
;;;@command{emacspeak-eww-next-table}
;;;Jump to next table.
;;;@item       [
;;;             @command{emacspeak-eww-previous-p}
;;;             Jump to previous paragraph.
;;;             @item  ]
;;;@command{emacspeak-eww-next-p}
;;;Jump to next paragraph.
;;;@item       b
;;;@command{shr-previous-link}
;;;Jump to previous link.
;;;@item  f
;;;@command{shr-next-link}
;;;Jump to next link.
;;;@item  n
;;;@command{emacspeak-eww-next-element}
;;;Jump to next element.
;;;@item       p
;;;@command{emacspeak-eww-previous-element}
;;;Jump to previous element.
;;;@item       s
;;;@command{eww-readable}
;;;Use EWW's built-in readable tool.
;;;@item :
;;;@command{emacspeak-eww-tags-at-point}
;;;Display  currently active HTML tags at point.
;;;@end table
;;;

;;; @subsection Filtering Content Using The DOM
;;; These commands use EWW's HTML DOM to display different filtered
;;; views of the Web page.
;;; With an interactive prefix argument, these commands prompt for a
;;; list of filters.
;;; Command @command{emacspeak-eww-restore} bound to @kbd{DEL} can be used
;;; to restore the previous view.
;;;
;;;@table @kbd
;;;@item  A
;;;@command{eww-view-dom-having-attribute}
;;;Display DOM nodes having specified attribute. Valid attributes
;;;are available via completion.
;;;@item       C
;;;@command{eww-view-dom-having-class}
;;;Display DOM nodes having specified class. Valid classes
;;;are available via completion.
;;;@item  E
;;;@command{eww-view-dom-having-elements}
;;;Display specified elements from the Dom. Valid element names
;;;are available via completion.
;;;@item  I
;;;@command{eww-view-dom-having-id}
;;;Display DOM nodes having specified ID. Valid id values
;;;are available via completion.
;;;@item  R
;;;@command{eww-view-dom-having-role}
;;;Display DOM nodes having specified role. Valid roles
;;;are available via completion.
;;;@item       M-a
;;;@command{eww-view-dom-not-having-attribute}
;;;Filter out DOM nodes having specified attribute. Valid attribute values
;;;are available via completion.
;;;@item       M-c
;;;@command{eww-view-dom-not-having-class}
;;;Filter out DOM nodes having specified class. Valid class values
;;;are available via completion.
;;;@item       M-e
;;;@command{eww-view-dom-not-having-elements}
;;;Filter out  specified element DOM nodes. Valid element names
;;;are available via completion.
;;;@item       M-i
;;;@command{eww-view-dom-not-having-id}
;;;Dfilter out Display DOM nodes having specified ID. Valid id values
;;;are available via completion.
;;;@item       M-r
;;;@command{eww-view-dom-not-having-role}
;;;Filter out  DOM nodes having specified role. Valid role values
;;;are available via completion.
;;;@end table
;;; @subsection Diving Into (Focusing) On Specific Content
;;;
;;; Contrast this with filtering described in the previous section.
;;; There, we discussed commands that @emph{filter} the DOM to render
;;; specific types of elements. For HTML as spoken on the Web, there
;;; is a separate use-case that is helpful as a dual to filtering,
;;; namely, displaying a specific portion of a page, typically the
;;; contents of a @code{div} element.
;;; These elements often appear many times on a page, and can be
;;; deeply nested, making it difficult to focus on the relevant
;;; content on the page, e.g. news sites.
;;; Commands @code{emacspeak-eww-dive-into-div}
;;; help  in such cases, @kbd{C-d} renders the @code{div} containing
;;; point in a separate buffer
;;;  As with the filtering commands, @kbd{l} returns to the
;;; buffer where these commands were executed.
;;; Long-term users of Emacspeak who still remember Emacs-W3 will
;;; recognize this as the @emph{focus} command implemented by
;;; Emacspeak for W3.
;;; @subsection Updated  Commands For Following  Links

;;; These key-bindings are available when point is on a link. They
;;; enable context-specific actions for following links, e.g., to play
;;; media streams, or to open various feed-types such as @code{ATOM},
;;; @code{RSS}, or @code{OPML}.
;;;
;;;
;;; @table @kbd
;;; @item k
;;; @command{shr-copy-url}
;;; Copy URL under point to the kill-ring.
;;; @item ;
;;; @command{emacspeak-eww-play-media-at-point}
;;; Play media URL under point using @code{emacs-m-player}.
;;; @item U
;;; @command{emacspeak-eww-curl-play-media-at-point}
;;; Play media url under point by first downloading the URL using
;;; CURL. This is useful for sites that do multiple redirects before
;;; returning the actual media stream URL.
;;; @item C-o
;;; @command{emacspeak-feeds-opml-display}
;;; Display link under point as an @code{OPML} feed .
;;; @item C-r
;;; @command{emacspeak-feeds-rss-display}
;;; Display link under point as an @code{RSS} feed.
;;; @item C-a
;;; @command{emacspeak-feeds-atom-display}
;;; Display link under point as an @code{ATOM} feed.
;;; @item y
;;; @command{emacspeak-m-player-youtube-player}
;;; Play link under point as a Youtube stream.
;;; @end table
;;;
;;; @subsection Table Browsing

;;; Summary Of Keyboard Commands:
;;; @itemize
;;; @item @kbd{M-<left>} emacspeak-eww-table-previous-cell@MDash{}
;;; Speak previous cell.
;;; @item @kbd{M-<right>} emacspeak-eww-table-next-cell @MDash{}
;;; Speak previous cell.
;;; @item @kbd{M-<up>} emacspeak-eww-table-previous-row @MDash{}
;;; Speak cell above.
;;; @item @kbd{M-<down>} emacspeak-eww-table-next-row @MDash{}
;;; Speak  cell below.
;;; @item @kbd{M-.} emacspeak-eww-table-speak-cell @MDash{}
;;; Speak current cell.
;;; @item @kbd{M-,} emacspeak-eww-table-speak-dimensions @MDash{}
;;; Speak number of rows and columns.
;;; @item @kbd{C-t} emacspeak-eww-table-data @MDash{}
;;; Browse this table in Emacspeak's Table UI @MDash{} @xref{emacspeak-table-ui}.
;;; @end itemize

;;; Emacspeak EWW supports table navigation via keys @kbd{M-.},
;;;@kbd{M-LEFT} and @kbd{M-RIGHT},
;;; to speak the current, previous and next table cell
;;;respectively. The latter commands also move to the cell being
;;;spoken.  You can get  a sense of the table's size via @kbd{M-,}
;;;which speaks the number of rows and cells in the table. This works for plain tables, not nested tables; for nested
;;;tables, first have then @emph{unnested} using one of the XSLT
;;;transforms like @code{sort-tables}.

;;;@subsection Miscellaneous Commands

;;;@table @kbd
;;; @item  C-RET
;;; @command {emacspeak-eww-fillin-form-field}
;;; When on an input field, insert  username/password information
;;; accessed via auth-source.
;;;@item '
;;;@command{emacspeak-speak-rest-of-buffer}
;;;Speak rest of current Web page starting from point.
;;;@item *
;;;@command{eww-add-bookmark}
;;;Bookmark current Web page.
;;;@item = @command{dtk-toggle-punctuation-mode}
;;;Toggle punctuation mode.
;;;@item ?
;;;@command{emacspeak-google-similar-to-this-page}
;;;Google similarity search.
;;;@item C-t
;;;@item G @command{emacspeak-google-command}
;;;Prefix key to invoke Google-specific commands.
;;;@item L
;;;@command{emacspeak-eww-links-rel}
;;;Display any related links discovered via the document's @code{meta} tag.
;;;@item Q
;;;@command{emacspeak-kill-buffer-quietly}
;;;Delete this buffer.
;;;@item V
;;;@command{eww-view-source}
;;;Display Web page source.
;;;@item e
;;;@command{emacspeak-we-xsl-map}
;;;Prefix key for invoking XSLT-based filters.
;;;@item k
;;;@command{eww-copy-page-url}
;;;Copy page URL to kill-ring.
;;;@end table
;;;
;;; In addition, see commands in
;;; @xref{emacspeak-google},  for Google-Search specific commands, many of
;;; which are available via prefix-key @kbd{G}.

;;; @subsection Filtering Content Using XSLT And XPath

;;;@table @kbd
;;;@item C-c
;;;@command{emacspeak-we-junk-by-class-list}
;;;Prompts for list of class-names with completion,
;;;and filters out matching elements.
;;;@item C-f
;;;@command{emacspeak-we-count-matches}
;;;Prompts for XPath expression, and returns count of matching elements.
;;;@item C-p
;;;@command{emacspeak-we-xpath-junk-and-follow}
;;;Follows link under point, and displays that page
;;;after filtering by a specified XPath expression.
;;;@item C-t
;;;@command{emacspeak-we-count-tables}
;;;Display a count of tables in the page.
;;;@item C-x
;;;@command{emacspeak-we-count-nested-tables}
;;;Counts nested tables.
;;;@item C
;;;@command{emacspeak-we-extract-by-class-list}
;;;Prompts for a list of class-names, and displays matching elements.
;;;@item D
;;;@command{emacspeak-we-junk-by-class-list}
;;;Filters out elements  having specified class attributes.
;;;@item I
;;;@command{emacspeak-we-extract-by-id-list}
;;;Extracts elements by specified list of ID values.
;;;@item M
;;;@command{emacspeak-we-extract-tables-by-match-list}
;;;Extracts tables that match specified selection pattern.
;;;@item P
;;;@command{emacspeak-we-follow-and-extract-main}
;;;Follows link under point, and extracts readable content,
;;;by default, this is all paragraphs and headings.
;;;@item S
;;;@command{emacspeak-we-style-filter}
;;;Filters content by style attribute.
;;;@item T
;;;@command{emacspeak-we-extract-tables-by-position-list}
;;;Extracts tables by their position on the page.
;;;@item X
;;;@command{emacspeak-we-extract-nested-table-list}
;;;Extracts nested tables.
;;;@item a
;;;@command{emacspeak-we-xslt-apply}
;;;Prompt for and apply specified XSLT transform to current page.
;;;@item b
;;;@command{emacspeak-we-follow-and-filter-by-id}
;;;Follow link under point, and filter by specified id value.
;;;@item c
;;;@command{emacspeak-we-extract-by-class}
;;;Extracts elements by class.
;;;@item d
;;;@command{emacspeak-we-junk-by-class}
;;;Filters out elements having specified class value.
;;;@item e
;;;@command{emacspeak-we-url-expand-and-execute}
;;;Follow link under point, but pass the result to a custom executor.
;;;@item f
;;;@command{emacspeak-we-xslt-filter}
;;;Apply a specified XSLT filter (XPath) to current page.
;;;@item i
;;;@command{emacspeak-we-extract-by-id}
;;;Extract elements by id value.
;;;@item j
;;;@command{emacspeak-we-xslt-junk}
;;;Filter out elements matching specified pattern.
;;;@item k
;;;@command{emacspeak-we-toggle-xsl-keep-result}
;;;Debugging tool  --- retains the  HTML source after XSLT.
;;;@item m
;;;@command{emacspeak-we-extract-table-by-match}
;;;Extract matching table.
;;;@item p
;;;@command{emacspeak-we-xpath-filter-and-follow}
;;;Follow link under point, and filter results by a specified XPath filter.
;;;@item r
;;;@command{emacspeak-we-extract-by-role}
;;;Extract elements by specified role value.
;;;@item s
;;;@command{emacspeak-we-xslt-select}
;;;Select default XSLT transform that is applied before rendering the page.
;;;@item t
;;;@command{emacspeak-we-extract-table-by-position}
;;;Extracts tables by their position on the page.
;;;@item u
;;;@command{emacspeak-we-extract-matching-urls}
;;;Display matching links on the page.
;;;@item v
;;;@command{emacspeak-we-class-filter-and-follow-link}
;;;Follow link under point, and filter by specified class value.
;;;@item w
;;;@command{emacspeak-we-extract-by-property}
;;;Extract element using a combination of DOM attributes.
;;;@item x
;;;@command{emacspeak-we-extract-nested-table}
;;;Extract a nested table using a match-list.
;;;@item y
;;;@command{emacspeak-we-class-filter-and-follow}
;;;Follow link under point and filter by class values.
;;;@end table
;;; @subsection EWW And EBooks On The Emacspeak Audio Desktop
;;; Modules emacspeak-epub and emacspeak-bookshare provide EBook
;;; front-ends to EPub-2 and Daisy EBooks. Both modules now use EWW to
;;; render these EBooks. Module emacspeak-eww provides a simple
;;; bookmarking facility --- called eww-marks (to avoid confusion with
;;; EWW's Web Bookmarks). When reading an EBook, you can use @code{m}
;;; to create an EWW-mark at that position. These marks are
;;; automatically saved across Emacs sessions. To open a previously
;;; created eww-mark, use command @code{emacspeak-eww-open-mark} bound
;;; to @code{C-x r e}. This command reads a eww-mark name with
;;; completion. Use this command with an interactive prefix arg to
;;; delete a previously created eww-mark.
;;;

;;; Code:

;;}}}
;;{{{ Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(eval-when-compile(require 'subr-x))
(require 'emacspeak-preamble)
(require 'eww  )
(require 'dom)
(require 'dom-addons)
(require 'emacspeak-we)
(require 'emacspeak-google)
(declare-function emacspeak-epub-eww
                  "emacspeak-epub" (epub-file &optional broken-ncx))
(declare-function emacspeak-m-player "emacspeak-m-player" (resource &optional play-list))

;;}}}
;;{{{ Helpers:

;;;###autoload
(defsubst emacspeak-eww-browser-check ()
  "Browser check"
  (cl-assert  (eq major-mode 'eww-mode) t (error "Not in EWW")))

;;; Return URL under point or URL read from minibuffer.
;;;###autoload
(defun emacspeak-eww-read-url ()
  (or
   (shr-url-at-point nil)
   (read-string "URL:" (browse-url-url-at-point))))

;;; Generate functions emacspeak-eww-current-title and friends:

(cl-loop
 for name in
 '(title  source url dom)
 do
 (eval
  `(defun
       ,(intern (format "emacspeak-eww-current-%s" name)) ()
     , (format "Return eww-current-%s." name)
     (cl-declare (special eww-data))
     (plist-get eww-data
                ,(intern (format ":%s" name)))))
 (eval
  `(defun
       ,(intern (format "emacspeak-eww-set-%s" name)) (value)
     , (format "Set eww-current-%s." name)
     (cl-assert (boundp 'eww-data) nil "Not a EWW rendered page.")
     (plist-put eww-data
                ,(intern (format ":%s" name))
                value))))

;;}}}
;;{{{ Declare generated functions:

(declare-function emacspeak-eww-current-dom "emacspeak-eww" nil)
(declare-function emacspeak-eww-current-title "emacspeak-eww" nil)
(declare-function emacspeak-eww-set-dom "emacspeak-eww" (dom))
(declare-function emacspeak-eww-set-url "emacspeak-eww" (url))
(declare-function emacspeak-eww-set-title "emacspeak-eww" (title))

;;}}}
;;{{{ Setup EWW Initialization:

(defvar emacspeak-eww-url-at-point
  #'(lambda ()
      (let ((url (shr-url-at-point nil)))
        (cond
         ((and url ;;; google  Result
               (stringp url)
               (string-prefix-p (emacspeak-google-result-url-prefix) url))
          (emacspeak-google-canonicalize-result-url url))
         ((and url (stringp url))url)
         (t (error "No URL under point.")))))
  "EWW Url At point that also handle google specialities.")

(add-hook
 'eww-mode-hook
 #'(lambda ()
     (outline-minor-mode)
     (emacspeak-pronounce-toggle-use-of-dictionaries t)))

(defvar emacspeak-eww-masquerade t
  "Masquerade flag")

(defun emacspeak-eww-masquerade ()
  "Toggle masquerade."
  (interactive)
  (cl-declare (special emacspeak-eww-masquerade))
  (setq emacspeak-eww-masquerade (not emacspeak-eww-masquerade))
  (message "Turned %s masquerade"
           (if emacspeak-eww-masquerade "on" "off"))
  (emacspeak-auditory-icon (if emacspeak-eww-masquerade 'on 'off)))

(defvar  emacspeak-eww-masquerade-as
  (format "User-Agent: %s\r\n"
          "Mozilla/5.0 (X11; Linux x86_64) \
AppleWebKit/537.36 (KHTML, like Gecko) \
Chrome/86.0.4240.75 \
Safari/537.36"
          )
  "User Agent string sent when masquerading.")

;;; Advice note: Setting ad-return-value in one arm of the cond
;;; appears to perculate to both arms.

(defadvice url-http-user-agent-string (around emacspeak pre act comp)
  "Masquerade response"
  ad-do-it
  (cond
   (emacspeak-eww-masquerade
    (setq ad-return-value emacspeak-eww-masquerade-as))
   (t (setq ad-return-value "User-Agent: URL/Emacs \r\n"))))

(defcustom emacspeak-eww-inhibit-images nil
  "Turn this on to avoid rendering images."
  :type 'boolean
  :group 'emacspeak)

(declare-function emacspeak-feeds-feed-display
                  "emacspeak-feeds" (feed-url style &optional speak))

(defun emacspeak-eww-setup ()
  "Setup keymaps etc."
  (cl-declare (special
               eww-mode-map eww-link-keymap eww-text-map
               shr-inhibit-images emacspeak-eww-inhibit-images
               emacspeak-pronounce-common-xml-namespace-uri-pronunciations
               emacspeak-eww-masquerade))
  (ems--fastload "emacspeak-pronounce")
  (emacspeak-pronounce-augment-pronunciations
   'eww-mode emacspeak-pronounce-common-xml-namespace-uri-pronunciations)
  (emacspeak-pronounce-add-dictionary-entry
   'eww-mode
   emacspeak-speak-rfc-3339-datetime-pattern
   (cons 're-search-forward 'emacspeak-speak-decode-rfc-3339-datetime))
;;; turn off images on request
  (setq shr-inhibit-images emacspeak-eww-inhibit-images)
;;; remove "I" "o" from eww-link-keymap
  (cl-loop
   for c in
   '(?I ?o)
   do
   (when (assoc  c eww-link-keymap)
     (delete (assoc  c eww-link-keymap) eww-link-keymap)))
  (define-key eww-text-map  [C-return] 'emacspeak-eww-fillin-form-field)
  (define-key eww-link-keymap  "!" 'emacspeak-eww-shell-command-on-url-at-point)
  (define-key eww-link-keymap  "k" 'shr-copy-url)
  (define-key eww-link-keymap ";" 'emacspeak-eww-play-media-at-point)
  (define-key eww-link-keymap "U" 'emacspeak-eww-curl-play-media-at-point)
  (define-key eww-link-keymap (ems-kbd "C-o") 'emacspeak-feeds-opml-display)
  (define-key eww-link-keymap (ems-kbd "C-r") 'emacspeak-feeds-rss-display)
  (define-key eww-link-keymap (ems-kbd "C-a") 'emacspeak-feeds-atom-display)
  (define-key eww-link-keymap  "y" 'emacspeak-m-player-youtube-player)
  (cl-loop
   for binding  in
   '(
     (":" emacspeak-eww-tags-at-point)
     ("\"" emacspeak-eww-reading-settings)
     ("V" eww-view-source)
     ("'" emacspeak-speak-rest-of-buffer)
     ("*" eww-add-bookmark)
     ("," emacspeak-eww-previous-h)
     ("." emacspeak-eww-next-h)
     ("1" emacspeak-eww-next-h1)
     ("2" emacspeak-eww-next-h2)
     ("3" emacspeak-eww-next-h3)
     ("4" emacspeak-eww-next-h4)
     ("=" dtk-toggle-punctuation-mode)
     ("?" emacspeak-google-similar-to-this-page)
     ("A" eww-view-dom-having-attribute)
     ("C" eww-view-dom-having-class)
     ("C-d" emacspeak-eww-dive-into-div)
     ("C-t" emacspeak-eww-table-data)
     ("C-e" emacspeak-prefix-command)
     ("M-<left>" emacspeak-eww-table-previous-cell)
     ("M-<up>"  emacspeak-eww-table-previous-row)
     ("M-<down>"  emacspeak-eww-table-next-row)
     ("M-<right>"  emacspeak-eww-table-next-cell)
     ("M-." emacspeak-eww-table-speak-cell)
     ("M-," emacspeak-eww-table-speak-dimensions)
     ("E" eww-view-dom-having-elements)
     ("G" emacspeak-google-command)
     ("c" emacspeak-eww-browse-chrome)
     ("j" emacspeak-eww-browse-eaf)
     ("I" eww-view-dom-having-id)
     ("J" emacspeak-eww-next-element-like-this)
     ("K" emacspeak-eww-previous-element-like-this)
     ("M-SPC" emacspeak-eww-speak-this-element)
     ("M-0" emacspeak-eww-previous-h)
     ("M-1" emacspeak-eww-previous-h1)
     ("M-2" emacspeak-eww-previous-h2)
     ("M-3" emacspeak-eww-previous-h3)
     ("M-4" emacspeak-eww-previous-h4)
     ("M-a" eww-view-dom-not-having-attribute)
     ("M-c" eww-view-dom-not-having-class)
     ("M-e" eww-view-dom-not-having-elements)
     ("M-i" eww-view-dom-not-having-id)
     ("M-r" eww-view-dom-not-having-role)
     ("L" emacspeak-eww-links-rel)
     ("N" emacspeak-eww-next-element-from-history)
     ("O" emacspeak-eww-previous-li)
     ("P" emacspeak-eww-previous-element-from-history)
     ("Q" emacspeak-kill-buffer-quietly)
     ("R" eww-view-dom-having-role)
     ("T" emacspeak-eww-previous-table)
     ("[" emacspeak-eww-previous-p)
     ("DEL" emacspeak-eww-restore)
     ("]" emacspeak-eww-next-p)
     ("b" shr-previous-link)
     ("e" emacspeak-we-xsl-map)
     ("f" shr-next-link)
     ("k" eww-copy-page-url)
     ("n" emacspeak-eww-next-element)
     ("o" emacspeak-eww-next-li)
     ("p" emacspeak-eww-previous-element)
     ("s" eww-readable)
     ("t" emacspeak-eww-next-table)
     ("m" emacspeak-eww-add-mark)
     )
   do
   (emacspeak-keymap-update eww-mode-map binding)))

(emacspeak-eww-setup)

;;}}}
;;{{{play media:

(defun emacspeak-eww-play-media-at-point (&optional  playlist-p)
  "Play media url under point.
Optional interactive prefix arg `playlist-p' treats
 link as a playlist.  A second interactive prefix arg adds
 mplayer option -allow-dangerous-playlist-parsing"
  (interactive "P")
  (cl-declare (special emacspeak-m-player-media-history
                       emacspeak-eww-url-at-point))
  (let ((url
         (or (funcall emacspeak-eww-url-at-point)
             (browse-url-url-at-point))))
    (cl-assert (stringp url) t "No URL under point." )
    (message "Playing media  URL under point")
    (kill-new url)
    (push (list url (if playlist-p t nil)) emacspeak-m-player-media-history)
    (emacspeak-m-player  url  playlist-p)))

(defun emacspeak-eww-curl-play-media-at-point ()
  "Use Curl to pull a URL, then pass
the first line to MPlayer as a playlist.
Useful in handling double-redirect from TuneIn."
  (interactive)
  (let ((url
         (if emacspeak-eww-url-at-point
             (funcall emacspeak-eww-url-at-point)
           (browse-url-url-at-point))))
    (setq url
          (cl-first
           (split-string
            (shell-command-to-string (format "curl --silent '%s'" url))
            "\n")))
    (message "Playing redirected media  URL under point: %s" url)
    (emacspeak-m-player url t)))

;;}}}
;;{{{ Inline Helpers:

(defun emacspeak-eww-prepare-eww ()
  "Ensure that we are in an EWW buffer."
  (cl-declare (special major-mode  emacspeak-eww-cache-updated))
  (unless (eq major-mode 'eww-mode) (error "Not in EWW buffer."))
  (unless (emacspeak-eww-current-dom) (error "No DOM!"))
  (unless emacspeak-eww-cache-updated
    (eww-update-cache (emacspeak-eww-current-dom))))

(defun emacspeak-eww-post-render-actions ()
  "Post-render actions."
  (emacspeak-eww-prepare-eww))

;;}}}
;;{{{ Viewing Page metadata: meta, links

(defun emacspeak-eww-links-rel ()
  "Display Link tags of type rel.  Web pages for which alternate links
are available are cued by an auditory icon on the header line."
  (interactive)
  (emacspeak-eww-prepare-eww)
  (let ((alt (dom-alternate-links (emacspeak-eww-current-dom)))
        (base (eww-current-url)))
    (cond
     ((null alt) (message "No alternate links."))
     (t
      (with-temp-buffer
        (insert "<table><th>Type</th><th>URL</th></tr>\n")
        (cl-loop
         for a in alt do
         (insert "<tr>")
         (insert
          (format "<td>%s</td>\n"
                  (or (dom-attr a 'title)
                      (dom-attr a 'type)
                      (dom-attr a 'media)
                      "")))
         (insert
          (format "<td><a href='%s'>%s</td>\n"
                  (shr-expand-url (dom-attr a 'href) base)
                  (shr-expand-url (dom-attr a 'href) base)))
         (insert "</tr>\n"))
        (insert "</table>\n")
        (browse-url-of-buffer))))))

;;}}}
;;{{{ Map Faces To Voices:

(voice-setup-add-map
 '(
   (shr-abbreviation  voice-bolden-extra)
   (shr-h1  voice-bolden)
   (shr-h2  voice-bolden-medium)
   (shr-h3 voice-lighten)
   (shr-h4 voice-lighten-medium)
   (shr-h5 voice-lighten-extra)
   (shr-h6  voice-smoothen)
   (eww-invalid-certificate  voice-lighten-extra)
   (eww-valid-certificate voice-bolden)
   (eww-form-submit voice-animate)
   (eww-form-checkbox voice-monotone-extra)
   (eww-form-select voice-animate)
   (eww-form-text voice-lighten)
   (eww-form-file voice-smoothen)
   (eww-form-textarea voice-brighten)
   (shr-selected-link  voice-animate)
   (shr-strike-through voice-annotate)))

;;}}}
;;{{{ Advice Interactive Commands:

(cl-loop
 for f in
 '(eww-up-url eww-top-url
              eww-next-url eww-previous-url
              eww-back-url eww-forward-url)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak"
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)
       (dtk-speak (emacspeak-eww-current-title))))))

(defvar-local emacspeak-eww-style nil
  "Record if we applied an  xsl style in this buffer.")

(defvar-local emacspeak-eww-feed nil
  "Record if this eww buffer is displaying a feed.")

(defvar-local emacspeak-eww-url-template nil
  "Record if this eww buffer is displaying a url-template.")

;;;Check cache if URL already open, otherwise cache.

(defadvice eww-reload (around emacspeak pre act comp)
  "Check buffer local settings for feed buffers.
If buffer was result of displaying a feed, reload feed.
If we came from a url-template, reload that template.
Retain previously set punctuations  mode."
  (add-hook 'emacspeak-eww-post-process-hook 'emacspeak-eww-post-render-actions)
  (cond
   ((and (eww-current-url)
         emacspeak-eww-feed
         emacspeak-eww-style)
                                        ; this is a displayed feed
    (let ((p dtk-punctuation-mode)
          (r dtk-speech-rate)
          (u (eww-current-url))
          (s emacspeak-eww-style))
      (kill-buffer)
      (add-hook
       'emacspeak-eww-post-process-hook
       #'(lambda ()
           (dtk-set-punctuations p)
           (dtk-set-rate r))
       'at-end)
      (emacspeak-feeds-feed-display u s 'speak)))
   ((and (eww-current-url) emacspeak-eww-url-template)
                                        ; this is a url template
    (let
        ((n emacspeak-eww-url-template)
         (p dtk-punctuation-mode)
         (r dtk-speech-rate))
      (add-hook
       'emacspeak-eww-post-process-hook
       #'(lambda nil
           (dtk-set-punctuations p)
           (dtk-set-rate r))
       'at-end)
      (kill-buffer)
      (emacspeak-url-template-open (emacspeak-url-template-get  n))))
   (t ad-do-it)))

(cl-loop
 for f in
 '(eww eww-open-in-new-buffer eww-reload eww-open-file)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak"
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)))))

(defvar emacspeak-eww-rename-result-buffer t
  "Result buffer is renamed to document title.")

(defun emacspeak-eww-after-render-hook ()
  "Setup Emacspeak for rendered buffer. "
  (let ((title (emacspeak-eww-current-title))
        (alt (dom-alternate-links (emacspeak-eww-current-dom))))
    (when (= 0 (length title)) (setq title "EWW: Untitled"))
    (when emacspeak-eww-rename-result-buffer (rename-buffer title 'unique))
    (when alt
      (put-text-property 0 2 'auditory-icon 'mark-object  header-line-format))
    (emacspeak-speak-voice-annotate-paragraphs)
    (cond
     (emacspeak-eww-post-process-hook
      (emacspeak-eww-run-post-process-hook))
     (t (emacspeak-speak-mode-line)))))

(add-hook 'eww-after-render-hook 'emacspeak-eww-after-render-hook)

(defadvice eww-add-bookmark (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'mark-object)))

(defadvice eww-beginning-of-text (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice eww-end-of-text(after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'mark-object)))

(defadvice eww-bookmark-browse (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'open-object)))

(defadvice eww-bookmark-kill (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'delete-object)))

(defadvice eww-bookmark-yank(after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'yank-object)))

(defadvice eww-list-bookmarks(after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'open-object)))

(cl-loop
 for f in
 '(eww-next-bookmark eww-previous-bookmark)
 do
 (eval
  `(defadvice ,f(after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p) (emacspeak-auditory-icon 'select-object))
     (emacspeak-speak-line))))

(defadvice eww-quit(after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'close-object)))

(cl-loop
 for f in
 '(eww-change-select
   eww-toggle-checkbox
   eww-submit)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'button)))))

(cl-loop
 for f in
 '(shr-next-link shr-previous-link)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "speak."
     (ems-with-messages-silenced ad-do-it)
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'button)
       (emacspeak-speak-region
        (point)
        (next-single-property-change (point) 'help-echo
                                     nil (point-max)))))))

;;; Handle emacspeak-we-url-executor

(defadvice eww-follow-link (around emacspeak pre act comp)
  "Respect emacspeak-we-url-executor if set."
  (cl-declare (special emacspeak-we-url-executor))
  (emacspeak-auditory-icon 'button)
  (let ((emacspeak-eww-masquerade t))
    (cond
     ((and (ems-interactive-p)
           (functionp emacspeak-we-url-executor)
           (y-or-n-p "Use custom executor? "))
      (let ((url (get-text-property (point) 'shr-url)))
        (unless url (error "No URL  under point"))
        (funcall emacspeak-we-url-executor url)))
     (t ad-do-it))))

;;}}}
;;{{{ web-pre-process

;;;###autoload
(defun emacspeak-eww-autospeak()
  "Setup post process hook to speak the Web page. "
  (add-hook
   'emacspeak-eww-post-process-hook
   #'(lambda nil
       (cl-declare (special emacspeak-we-xpath-filter))
       (setq emacspeak-we-xpath-filter
             emacspeak-we-paragraphs-xpath-filter)
       (dtk-set-punctuations-to-some)
       (emacspeak-speak-buffer))
   'at-end))

;;;###autoload
(defvar emacspeak-eww-pre-process-hook nil
  "Pre-process hook -- to be used for XSL preprocessing etc.")
;;;###autoload
(defun emacspeak-eww-run-pre-process-hook (&rest _ignore)
  "Run web pre process hook."
  (cl-declare (special emacspeak-eww-pre-process-hook))
  (when     emacspeak-eww-pre-process-hook
    (condition-case nil
        (let ((inhibit-read-only t))
          (run-hooks  'emacspeak-eww-pre-process-hook))
      ((debug error)  (message "Caught error  in pre-process hook.")
       (setq emacspeak-eww-pre-process-hook nil)))
    (setq emacspeak-eww-pre-process-hook nil)))

;;}}}
;;{{{ web-post-process

;;;###autoload
(defvar emacspeak-eww-post-process-hook nil
  "Set locally to a  site specific post processor.
Note that the Web browser should reset this hook after using it.")

;;;###autoload
(defun emacspeak-eww-run-post-process-hook (&rest _ignore)
  "Run web post process hook."
  (cl-declare (special emacspeak-eww-post-process-hook))
  (when     emacspeak-eww-post-process-hook
    (condition-case nil
        (let ((inhibit-read-only t))
          (run-hooks  'emacspeak-eww-post-process-hook))
      ((debug error)  (message "Caught error  in post-process hook.")
       (setq emacspeak-eww-post-process-hook nil)))
    (setq emacspeak-eww-post-process-hook nil)))

;;}}}
;;{{{ xslt transform on request:

(defadvice eww-display-html (before emacspeak pre act comp)
  "Apply XSLT transform if requested."
  (cl-declare (special emacspeak-eww-pre-process-hook
                       emacspeak-we-xsl-transform emacspeak-we-xsl-p))
  (save-excursion
    (cond
     (emacspeak-eww-pre-process-hook (emacspeak-eww-run-pre-process-hook))
     ((and emacspeak-we-xsl-p emacspeak-we-xsl-transform)
      (emacspeak-xslt-region
       emacspeak-we-xsl-transform (point) (point-max)
       emacspeak-we-xsl-params)))))

;;}}}
;;{{{ DOM Structure In Rendered Buffer:

(cl-loop
 for  tag in
 '(h1 h2 h3 h4 h5 h6 div                ; sectioning
      math                              ; mathml
      ul ol dl                          ; Lists
      li dt dd p                        ; block-level: bullets, paras
      pre form blockquote                   ; block-level
      a b it em span                    ; in-line
      table)
 do
 (eval
  `
  (defadvice ,(intern (format "shr-tag-%s" tag)) (around eww-tag pre act comp)
    (let ((orig (point)))
      ad-do-it
      (let ((start
             (if (char-equal (following-char) ?\n)
                 (min (point-max) (1+ orig))
               orig))
            (end
             (if (> (point) orig)
                 (1- (point))
               (point))))
        (put-text-property start end
                           (quote ,tag) 'eww-tag)
        (when (memq (quote ,tag) '(h1 h2 h3 h4 h5 h6))
          (put-text-property start end 'h 'eww-tag)))))))
;;; Handle MathML math element:

(defun shr-tag-math (dom)
  "Handle Math Nodes from MathML"
  (shr-ensure-newline)
  (shr-generic dom)
  (shr-ensure-newline))

;;}}}
;;{{{ Advice readable
(defadvice eww-readable (around emacspeak pre act comp)
  "Speak contents."
  (let ((inhibit-read-only t))
    ad-do-it
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-buffer)))

;;}}}
;;{{{  Customize image loading:

(defadvice eww-display-image (around emacspeak pre act comp)
  "Image inhibition"
  (unless emacspeak-eww-inhibit-images ad-do-it))

;;}}}
;;{{{ element, class, role, id caches:

(defvar-local emacspeak-eww-cache-updated nil
  "Records if caches are updated.")

;;; Mark cache to be dirty if we restore history:

(defadvice eww-restore-history (after emacspeak pre act comp)
  "mark cache dirty."
  (setq emacspeak-eww-cache-updated nil)
  (emacspeak-eww-prepare-eww))

(defvar-local eww-id-cache nil
  "Cache of id values. Is buffer-local.")

(defvar-local eww-class-cache nil
  "Cache of class values. Is buffer-local.")

(defvar-local eww-role-cache nil
  "Cache of role values. Is buffer-local.")

(defvar-local eww-itemprop-cache nil
  "Cache of itemprop values. Is buffer-local.")

(defvar-local eww-property-cache nil
  "Cache of property values. Is buffer-local.")

;;; Holds element names as strings.

(defvar-local eww-element-cache nil
  "Cache of element names. Is buffer-local.")

(defun eww-update-cache (dom)
  "Update element, role, class and id cache."
  (cl-declare (special
               eww-element-cache eww-id-cache
               eww-property-cache eww-itemprop-cache
               eww-role-cache eww-class-cache emacspeak-eww-cache-updated))
  (when (listp dom)                     ; build cache
    (let ((id (dom-attr dom 'id))
          (class (dom-attr dom 'class))
          (role (dom-attr dom 'role))
          (itemprop (dom-attr dom 'itemprop))
          (property (dom-attr dom 'property))
          (el (symbol-name (dom-tag dom)))
          (children (dom-children dom)))
      (when id (cl-pushnew id eww-id-cache :test #'string=))
      (when class
        (let ((classes (split-string class " ")))
          (cl-loop for c in classes do
                   (cl-pushnew c eww-class-cache :test #'string=))))
      (when itemprop (cl-pushnew itemprop eww-itemprop-cache :test #'string=))
      (when role (cl-pushnew role eww-role-cache :test #'string=))
      (when property (cl-pushnew property eww-property-cache :test #'string=))
      (when el (cl-pushnew el eww-element-cache :test #'string=))
      (when children (mapc #'eww-update-cache children)))
    (setq emacspeak-eww-cache-updated t)))

;;}}}
;;{{{ Filter DOM:

(defun emacspeak-eww-tag-article (dom)
  "Tag article, then render."
  (let ((start (point)))
    (shr-generic dom)
    (put-text-property start (point) 'article 'eww-tag)))

(defvar emacspeak-eww-shr-render-functions
  '((article . emacspeak-eww-tag-article)
    (title . eww-tag-title)
    (form . eww-tag-form)
    (input . eww-tag-input)
    (textarea . eww-tag-textarea)
    (math . shr-tag-math)
    (meta . eww-tag-meta)
    (button . eww-form-submit)
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
                  (dom-children dom)))))
      (when filtered
        (push (dom-attributes dom) filtered)
        (push (dom-tag dom) filtered))))))

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
                        (dom-children dom)))))
      (when filtered
        (push (dom-attributes dom) filtered)
        (push (dom-tag dom) filtered) filtered)))))

(defun eww-attribute-list-tester (attr-list)
  "Return predicate that tests for attr=value from members of
attr-value list for use as a DOM filter."
  (eval
   `#'(lambda (node)
        (let (attr  value found)
          (cl-loop
           for pair in (quote ,attr-list)
           until found
           do
           (setq attr (cl-first pair)
                 value (cl-second pair))
           (setq found (member value (split-string (dom-attr  node attr)))))
          (when found node)))))

(defun eww-attribute-tester (attr value)
  "Return predicate that tests for attr=value for use as a DOM filter."
  (eval
   `#'(lambda (node)
        (when
            (string= (dom-attr node (quote ,attr)) ,value) node))))

(defun eww-elements-tester (element-list)
  "Return predicate that tests for presence of element in element-list
for use as a DOM filter."
  (eval
   `#'(lambda (node)
        (when (memq (dom-tag node) (quote ,element-list)) node))))

(defun emacspeak-eww-view-helper  (filtered-dom)
  "View helper called by various filtering viewers."
  (cl-declare (special emacspeak-eww-rename-result-buffer
                       emacspeak-eww-shr-render-functions))
  (let ((emacspeak-eww-rename-result-buffer nil)
        (url (eww-current-url))
        (title  (format "%s: Filtered" (emacspeak-eww-current-title)))
        (inhibit-read-only t)
        (shr-external-rendering-functions emacspeak-eww-shr-render-functions))
    (eww-save-history)
    (erase-buffer)
    (goto-char (point-min))
;;;(setq shr-base (shr-parse-base url))
    (shr-insert-document filtered-dom)
    (emacspeak-eww-set-dom filtered-dom)
    (emacspeak-eww-set-url url)
    (emacspeak-eww-set-title title)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (setq buffer-read-only t))
  (eww-update-header-line-format)
  (emacspeak-auditory-icon 'open-object)
  (emacspeak-speak-buffer))

(defun emacspeak-eww-read-list (reader)
  "Return list of values  read using reader."
  (let (value-list  value done)
    (cl-loop
     until done
     do
     (setq value (funcall reader))
     (cond
      (value (cl-pushnew   value value-list :test #'string=))
      (t (setq done t))))
    value-list))

(defun emacspeak-eww-read-id ()
  "Return id value read from minibuffer."
  (cl-declare (special eww-id-cache))
  (unless eww-id-cache (error "No id to filter."))
  (let ((value (completing-read "Value: " eww-id-cache nil 'must-match)))
    (unless (zerop (length value)) value)))

(defun eww-view-dom-having-id (&optional multi)
  "Display DOM filtered by specified id=value test.
Optional interactive arg `multi' prompts for multiple ids."
  (interactive "P")
  (emacspeak-eww-prepare-eww)
  (let ((dom (emacspeak-eww-current-dom))
        (filter (if multi #'dom-by-id-list #'dom-by-id))
        (id  (if multi
                 (emacspeak-eww-read-list 'emacspeak-eww-read-id)
               (emacspeak-eww-read-id))))
    (setq dom (funcall filter dom id))
    (when dom
      (emacspeak-eww-view-helper
       (dom-html-from-nodes dom (eww-current-url))))))

(defun eww-view-dom-not-having-id (&optional multi)
  "Display DOM filtered by specified nodes not passing  id=value test.
Optional interactive arg `multi' prompts for multiple ids."
  (interactive "P")
  (emacspeak-eww-prepare-eww)
  (let ((dom
         (eww-dom-remove-if
          (emacspeak-eww-current-dom)
          (eww-attribute-list-tester
           (if multi
               (cl-loop
                for i in (emacspeak-eww-read-list 'emacspeak-eww-read-id)
                collect (list 'id i))
             (list (list 'id (emacspeak-eww-read-id))))))))
    (when dom
      (emacspeak-eww-view-helper
       (dom-html-add-base
        dom (eww-current-url))))))

(defun emacspeak-eww-read-attribute-and-value ()
  "Read attr-value pair and return as a list."
  (cl-declare (special eww-id-cache eww-class-cache eww-role-cache
                       eww-property-cache eww-itemprop-cache))
  (unless (or eww-role-cache eww-id-cache eww-class-cache
              eww-itemprop-cache eww-property-cache)
    (error "No attributes to filter."))
  (let(attr-names attr value)
    (when eww-class-cache (push "class" attr-names))
    (when eww-id-cache (push "id" attr-names))
    (when eww-itemprop-cache (push "itemprop" attr-names))
    (when eww-property-cache (push "property" attr-names))
    (when eww-role-cache (push "role" attr-names))
    (setq attr (completing-read "Attr: " attr-names nil 'must-match))
    (unless (zerop (length attr))
      (setq attr (intern attr))
      (setq value
            (completing-read
             "Value: "
             (cond
              ((eq attr 'id) eww-id-cache)
              ((eq attr 'itemprop) eww-itemprop-cache)
              ((eq attr 'property) eww-property-cache)
              ((eq attr 'class)eww-class-cache)
              ((eq attr 'role)eww-role-cache))
             nil 'must-match))
      (list attr value))))

(defun eww-view-dom-having-attribute (&optional multi)
  "Display DOM filtered by specified attribute=value test.
Optional interactive arg `multi' prompts for multiple classes."
  (interactive "P")
  (emacspeak-eww-prepare-eww)
  (let ((dom
         (eww-dom-keep-if
          (dom-child-by-tag (emacspeak-eww-current-dom) 'html)
          (eww-attribute-list-tester
           (if multi
               (emacspeak-eww-read-list 'emacspeak-eww-read-attribute-and-value)
             (list  (emacspeak-eww-read-attribute-and-value)))))))
    (when dom
      (emacspeak-eww-view-helper
       (dom-html-add-base dom   (eww-current-url))))))

(defun eww-view-dom-not-having-attribute (&optional multi)
  "Display DOM filtered by specified nodes not passing  attribute=value test.
Optional interactive arg `multi' prompts for multiple classes."
  (interactive "P")
  (emacspeak-eww-prepare-eww)
  (let ((dom
         (eww-dom-remove-if
          (dom-child-by-tag (emacspeak-eww-current-dom) 'html)
          (eww-attribute-list-tester
           (if multi
               (emacspeak-eww-read-list 'emacspeak-eww-read-attribute-and-value)
             (list  (emacspeak-eww-read-attribute-and-value)))))))
    (when dom
      (dom-html-add-base dom   (eww-current-url))
      (emacspeak-eww-view-helper dom))))

(defun emacspeak-eww-read-class ()
  "Return class value read from minibuffer."
  (cl-declare (special eww-class-cache))
  (unless eww-class-cache (error "No class to filter."))
  (let ((value (completing-read "Value: " eww-class-cache nil 'must-match)))
    (unless (zerop (length value)) value)))

(defun eww-view-dom-having-class (&optional multi)
  "Display DOM filtered by specified class=value test.
Optional interactive arg `multi' prompts for multiple classes."
  (interactive "P")
  (emacspeak-eww-prepare-eww)
  (let ((dom  (emacspeak-eww-current-dom))
        (filter (if multi #'dom-by-class-list #'dom-by-class))
        (class  (if multi
                    (emacspeak-eww-read-list 'emacspeak-eww-read-class)
                  (emacspeak-eww-read-class))))
    (setq dom (funcall filter dom class))
    (when dom
      (emacspeak-eww-view-helper
       (dom-html-from-nodes dom (eww-current-url))))))

(defun eww-view-dom-not-having-class (&optional multi)
  "Display DOM filtered by specified nodes not passing   class=value test.
Optional interactive arg `multi' prompts for multiple classes."
  (interactive "P")
  (emacspeak-eww-prepare-eww)
  (let ((dom
         (eww-dom-remove-if
          (emacspeak-eww-current-dom)
          (eww-attribute-list-tester
           (if multi
               (cl-loop
                for c in (emacspeak-eww-read-list 'emacspeak-eww-read-class)
                collect (list 'class c))
             (list (list 'class (emacspeak-eww-read-class))))))))
    (when dom
      (emacspeak-eww-view-helper
       (dom-html-add-base
        dom (eww-current-url))))))

(defun emacspeak-eww-read-role ()
  "Return role value read from minibuffer."
  (cl-declare (special eww-role-cache))
  (unless eww-role-cache (error "No role to filter."))
  (let ((value (completing-read "Value: " eww-role-cache nil 'must-match)))
    (unless (zerop (length value)) value)))

(defun emacspeak-eww-read-property ()
  "Return property value read from minibuffer."
  (cl-declare (special eww-property-cache))
  (unless eww-property-cache (error "No property to filter."))
  (let ((value (completing-read "Value: " eww-property-cache nil 'must-match)))
    (unless (zerop (length value)) value)))

(defun emacspeak-eww-read-itemprop ()
  "Return itemprop value read from minibuffer."
  (cl-declare (special eww-itemprop-cache))
  (unless eww-itemprop-cache (error "No itemprop to filter."))
  (let ((value (completing-read "Value: " eww-itemprop-cache nil 'must-match)))
    (unless (zerop (length value)) value)))

(defun eww-view-dom-having-role (multi)
  "Display DOM filtered by specified role=value test.
Optional interactive arg `multi' prompts for multiple classes."
  (interactive "P")
  (emacspeak-eww-prepare-eww)
  (let ((dom (emacspeak-eww-current-dom))
        (filter  (if multi #'dom-by-role-list #'dom-by-role))
        (role  (if multi
                   (emacspeak-eww-read-list 'emacspeak-eww-read-role)
                 (emacspeak-eww-read-role))))
    (setq dom (funcall filter dom role))
    (when dom
      (emacspeak-eww-view-helper
       (dom-html-from-nodes dom (eww-current-url))))))

(defun eww-view-dom-not-having-role (multi)
  "Display DOM filtered by specified  nodes not passing   role=value test.
Optional interactive arg `multi' prompts for multiple classes."
  (interactive "P")
  (cl-declare (special  emacspeak-eww-shr-render-functions))
  (emacspeak-eww-prepare-eww)
  (let ((dom
         (eww-dom-remove-if
          (emacspeak-eww-current-dom)
          (eww-attribute-list-tester
           (if multi
               (cl-loop
                for r in (emacspeak-eww-read-list 'emacspeak-eww-read-role)
                collect (list 'role r))
             (list (list 'role (emacspeak-eww-read-role))))))))
    (when dom
      (emacspeak-eww-view-helper
       (dom-html-add-base
        dom
        (eww-current-url))))))

(defun eww-view-dom-having-property (multi)
  "Display DOM filtered by specified property=value test.
Optional interactive arg `multi' prompts for multiple classes."
  (interactive "P")
  (emacspeak-eww-prepare-eww)
  (let ((dom (emacspeak-eww-current-dom))
        (filter  (if multi #'dom-by-property-list #'dom-by-property))
        (property  (if multi
                       (emacspeak-eww-read-list 'emacspeak-eww-read-property)
                     (emacspeak-eww-read-property))))
    (setq dom (funcall filter dom property))
    (when dom
      (emacspeak-eww-view-helper
       (dom-html-from-nodes dom (eww-current-url))))))

(defun eww-view-dom-not-having-property (multi)
  "Display DOM filtered by specified  nodes not passing   property=value test.
Optional interactive arg `multi' prompts for multiple classes."
  (interactive "P")
  (cl-declare (special  emacspeak-eww-shr-render-functions))
  (emacspeak-eww-prepare-eww)
  (let ((dom
         (eww-dom-remove-if
          (emacspeak-eww-current-dom)
          (eww-attribute-list-tester
           (if multi
               (cl-loop
                for r in (emacspeak-eww-read-list 'emacspeak-eww-read-property)
                collect (list 'property r))
             (list (list 'property (emacspeak-eww-read-property))))))))
    (when
        dom
      (emacspeak-eww-view-helper
       (dom-html-add-base dom
                          (eww-current-url))))))

(defun eww-view-dom-having-itemprop (multi)
  "Display DOM filtered by specified itemprop=value test.
Optional interactive arg `multi' prompts for multiple classes."
  (interactive "P")
  (emacspeak-eww-prepare-eww)
  (let ((dom (emacspeak-eww-current-dom))
        (filter  (if multi #'dom-by-itemprop-list #'dom-by-itemprop))
        (itemprop  (if multi
                       (emacspeak-eww-read-list 'emacspeak-eww-read-itemprop)
                     (emacspeak-eww-read-itemprop))))
    (setq dom (funcall filter dom itemprop))
    (when dom
      (emacspeak-eww-view-helper
       (dom-html-from-nodes dom (eww-current-url))))))

(defun eww-view-dom-not-having-itemprop (multi)
  "Display DOM filtered by specified  nodes not passing   itemprop=value test.
Optional interactive arg `multi' prompts for multiple classes."
  (interactive "P")
  (cl-declare (special  emacspeak-eww-shr-render-functions))
  (emacspeak-eww-prepare-eww)
  (let ((dom
         (eww-dom-remove-if
          (emacspeak-eww-current-dom)
          (eww-attribute-list-tester
           (if multi
               (cl-loop
                for r in (emacspeak-eww-read-list 'emacspeak-eww-read-itemprop)
                collect (list 'itemprop r))
             (list (list 'itemprop (emacspeak-eww-read-itemprop))))))))
    (when dom
      (emacspeak-eww-view-helper
       (dom-html-add-base
        dom (eww-current-url))))))
(defun emacspeak-eww-read-element ()
  "Return element  value read from minibuffer."
  (cl-declare (special eww-element-cache))
  (let ((value (completing-read "Value: " eww-element-cache nil 'must-match)))
    (unless (zerop (length value)) (intern value))))

(defun eww-view-dom-having-elements (&optional multi)
  "Display DOM filtered by specified elements.
Optional interactive prefix arg `multi' prompts for multiple elements."
  (interactive "P")
  (emacspeak-eww-prepare-eww)
  (let ((dom (emacspeak-eww-current-dom))
        (filter  (if multi #'dom-by-tag-list #'dom-by-tag))
        (tag (if multi
                 (emacspeak-eww-read-list 'emacspeak-eww-read-element)
               (emacspeak-eww-read-element))))
    (setq dom (funcall filter dom tag))
    (cond
     (dom
      (emacspeak-eww-view-helper
       (dom-html-from-nodes dom (eww-current-url))))
     (t (message "Filtering failed.")))))

(defun eww-view-dom-not-having-elements (multi)
  "Display DOM filtered by specified nodes not passing   el list.
Optional interactive prefix arg `multi' prompts for multiple elements."
  (interactive "P")
  (emacspeak-eww-prepare-eww)
  (let ((dom
         (eww-dom-remove-if
          (emacspeak-eww-current-dom)
          (eww-elements-tester
           (if multi
               (emacspeak-eww-read-list 'emacspeak-eww-read-element)
             (list  (emacspeak-eww-read-element)))))))
    (when dom
      (emacspeak-eww-view-helper
       (dom-html-add-base
        dom (eww-current-url))))))

(defun emacspeak-eww-restore ()
  "Restore buffer to pre-filtered canonical state."
  (interactive)
  (cl-declare (special eww-history eww-history-position))
  (eww-restore-history(elt eww-history eww-history-position))
  (emacspeak-speak-mode-line)
  (emacspeak-auditory-icon 'open-object))

;;}}}
;;{{{ Filters For Non-interactive  Use:

(defun eww-display-dom-filter-helper (filter arg)
  "Helper for display filters."
  (emacspeak-eww-prepare-eww)
  (let ((dom (funcall  filter  (emacspeak-eww-current-dom)arg)))
    (when dom
      (emacspeak-eww-view-helper
       (dom-html-from-nodes dom (eww-current-url))))))

(defun eww-display-dom-by-id (id)
  "Display DOM filtered by specified id."
  (eww-display-dom-filter-helper #'dom-by-id  id))

(defun eww-display-dom-by-id-list (id-list)
  "Display DOM filtered by specified id-list."

  (eww-display-dom-filter-helper #'dom-by-id-list  id-list))

(defun eww-display-dom-by-class (class)
  "Display DOM filtered by specified class."

  (eww-display-dom-filter-helper #'dom-by-class  class))

(defun eww-display-dom-by-class-list (class-list)
  "Display DOM filtered by specified class-list."

  (eww-display-dom-filter-helper #'dom-by-class-list  class-list))

(defun eww-display-dom-by-element (tag)
  "Display DOM filtered by specified tag."
  (eww-display-dom-filter-helper #'dom-by-tag  tag))

(defun eww-display-dom-by-element-list (tag-list)
  "Display DOM filtered by specified element-list."

  (eww-display-dom-filter-helper #'dom-by-tag-list  tag-list))

(defun eww-display-dom-by-role (role)
  "Display DOM filtered by specified role."
  (eww-display-dom-filter-helper #'dom-by-role  role))

(defun eww-display-dom-by-role-list (role-list)
  "Display DOM filtered by specified role-list."
  (eww-display-dom-filter-helper #'dom-by-role-list  role-list))

;;}}}
;;{{{ Element Navigation:
;;; Try only storing symbols, not strings.

(defvar emacspeak-eww-element-navigation-history nil
  "History for element navigation.")
(defun emacspeak-eww-icon-for-element (el)
  "Return auditory icon for element `el'."
  (cond
   ((memq el '(li dt)) 'item)
   ((memq el '(h h1 h2 h3 h4 h5 h6)) 'section)
   ((memq el '(p ul ol dd dl)) 'paragraph)
   (t 'large-movement)))

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
  (cl-declare (special eww-element-cache
                       emacspeak-eww-element-navigation-history))
  (let*
      ((start
        (or
         (when (get-text-property (point) el)
           (next-single-property-change (point) el))
         (point)))
       (next (next-single-property-change start  el)))
    (when next
      (goto-char next)
      (cl-pushnew el  emacspeak-eww-element-navigation-history)
      (when (called-interactively-p 'interactive)
        (emacspeak-auditory-icon (emacspeak-eww-icon-for-element el))
        (emacspeak-speak-region
         next
         (next-single-property-change next el nil  (point-max)))))))

(defun emacspeak-eww-previous-element (el)
  "Move backward  to the previous  specified element."
  (interactive
   (list
    (progn
      (emacspeak-eww-prepare-eww)
      (intern
       (completing-read "Element: " eww-element-cache nil 'must-match
                        nil 'emacspeak-eww-element-navigation-history)))))
  (cl-declare (special eww-element-cache
                       emacspeak-eww-element-navigation-history))
  (let* ((start
          (or
           (when (get-text-property  (point) el)
             (previous-single-property-change (1+ (point)) el))
           (point)))
         (previous (previous-single-property-change  start  el)))
    (when previous
      (goto-char (or (previous-single-property-change previous el) (point-min)))
      (cl-pushnew  el emacspeak-eww-element-navigation-history)
      (emacspeak-auditory-icon (emacspeak-eww-icon-for-element el))
      (emacspeak-speak-region (point) previous))))

(defun emacspeak-eww-next-element-from-history ()
  "Uses element navigation history to decide where we jump."
  (interactive)
  (cl-declare (special emacspeak-eww-element-navigation-history))
  (cond
   (emacspeak-eww-element-navigation-history
    (funcall-interactively #'emacspeak-eww-next-element
                           (car emacspeak-eww-element-navigation-history)))
   (t (error "No elements in navigation history"))))

(defun emacspeak-eww-previous-element-from-history ()
  "Uses element navigation history to decide where we jump."
  (interactive)
  (cl-declare (special emacspeak-eww-element-navigation-history))
  (cond
   (emacspeak-eww-element-navigation-history
    (funcall-interactively #'emacspeak-eww-previous-element
                           (car emacspeak-eww-element-navigation-history)))
   (t (error "No elements in navigation history"))))

(defun emacspeak-eww-here-tags ()
  "Return list of enclosing tags at point."
  (let* ((eww-tags (text-properties-at (point))))
    (cl-loop
     for i from 0 to (1- (length eww-tags)) by 2
     if (eq (plist-get eww-tags (nth i eww-tags)) 'eww-tag)
     collect (nth i eww-tags))))

(defun emacspeak-eww-read-tags-like-this(&optional prompt)
  "Read tag for like-this navigation."
  (let ((tags (emacspeak-eww-here-tags)))
    (cond
     ((null tags) (error "No enclosing element here."))
     ((= 1 (length tags))  (cl-first tags))
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
  (funcall-interactively #'emacspeak-eww-next-element  element))

(defun emacspeak-eww-previous-element-like-this (element)
  "Moves to next element like current.
Prompts if content at point is enclosed by multiple elements."
  (interactive
   (list (emacspeak-eww-read-tags-like-this)))
  (funcall-interactively #'emacspeak-eww-previous-element  element))

(defun emacspeak-eww-speak-this-element (element)
  "Speaks  to next element like current. "
  (interactive
   (list
    (or
     (cl-first (emacspeak-eww-here-tags))
     (car emacspeak-eww-element-navigation-history))))
  (let ((start (point)))
    (save-excursion
      (emacspeak-eww-next-element  element)
      (emacspeak-auditory-icon 'select-object)
      (emacspeak-speak-region start (point)))))
;;; Generate next and previous structural navigators:
(defcustom emacspeak-eww-autospeak t
  "Turn this on to make section navigation autospeak.
This also reverses the meaning of the prefix-arg to section nav
  commands."
  :type 'boolean
  :group 'emacspeak-eww)

(cl-loop
 for  f in
 '(h h1 h2 h3 h4 h5 h6 li dt dd table ol ul dl p)
 do
 (eval
  `(defun ,(intern (format "emacspeak-eww-next-%s" f)) (&optional speak)
     ,(format
       "Move forward to the next %s.
Optional interactive prefix arg speaks the %s.  Second
interactive prefix toggles this flag.  See user option
`emacspeak-eww-autospeak' on how to reverse this behavior.
Second interactive prefix arg toggles default value of this flag.
The %s is automatically spoken if there is no user activity."
       f f f)
     (interactive "P")
     (cl-declare (special emacspeak-eww-autospeak))
     (let ((s (intern ,(format "%s" f))))
       (when (memq s '(h1 h2 h3 h4))
         (emacspeak-auditory-icon 'section))
       (funcall-interactively #'emacspeak-eww-next-element s)
       (when (and speak (= 16 (car speak)))
         (setq emacspeak-eww-autospeak (not emacspeak-eww-autospeak)))
       (when
           (or speak
               emacspeak-eww-autospeak
               (and (called-interactively-p 'interactive) (sit-for 4.0)))
         (emacspeak-auditory-icon 'item)
         (let ((start  (point)))
           (condition-case nil
               (save-excursion
                 (funcall #'emacspeak-eww-next-element s)
                 (emacspeak-speak-region start (point)))
             (error nil)))))))
 (eval
  `(defun ,(intern (format "emacspeak-eww-previous-%s" f)) (&optional speak)
     ,(format "Move backward to the next %s.
Optional interactive prefix arg speaks the %s.
Second interactive prefix toggles this flag.
See user option `emacspeak-eww-autospeak' on how to reverse this behavior.
The %s is automatically spoken if there is no user activity."
              f f f)
     (interactive "P")
     (cl-declare (special emacspeak-eww-autospeak))
     (let ((s (intern ,(format "%s" f))))
       (when (memq s '(h1 h2 h3 h4))
         (emacspeak-auditory-icon 'section))
       (funcall-interactively #'emacspeak-eww-previous-element s)
       (when (and speak (= 16 (car speak)))
         (setq emacspeak-eww-autospeak (not emacspeak-eww-autospeak)))
       (when (or speak
                 emacspeak-eww-autospeak
                 (sit-for 3.0))
         (emacspeak-auditory-icon 'item)
         (let ((start  (point)))
           (condition-case nil
               (save-excursion
                 (funcall #'emacspeak-eww-next-element s)
                 (emacspeak-speak-region start (point)))
             (error nil))))))))

;;}}}
;;{{{ Google Search  fixes:

(cl-loop
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
;;{{{ Speech-enable EWW buffer list:

(defun emacspeak-eww-speak-buffer-line ()
  "Speak EWW buffer line."
  (cl-assert (eq major-mode 'eww-buffers-mode) nil
             "Not in an EWW buffer listing.")
  (let ((buffer (get-text-property (line-beginning-position) 'eww-buffer)))
    (if buffer
        (dtk-speak (buffer-name buffer))
      (message "Can't find an EWW buffer for this line. "))))

(defadvice eww-list-buffers (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-eww-speak-buffer-line)))

(defadvice eww-buffer-kill (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-eww-speak-buffer-line)))

(defadvice eww-buffer-select (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'open-object)))

(cl-loop
 for f in
 '(eww-buffer-show-next eww-buffer-show-previous)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'select-object)
       (emacspeak-eww-speak-buffer-line)))))

;;}}}
;;{{{  EWW Filtering shortcuts:

;;}}}
;;{{{ Tags At Point:

(defun emacspeak-eww-tags-at-point ()
  "Display tags at point."
  (interactive)
  (let ((tags (emacspeak-eww-here-tags)))
    (print tags)
    (dtk-speak-list tags)))

;;}}}
;;{{{ Handling Media (audio/video)

;;; This should ideally be handled through mailcap. At present, EWW
;;; sets eww-use-external-browser-for-content-type to match
;;; audio/video (only) and hands those off to
;;; eww-browse-with-external-browser. Below, we advice
;;; eww-browse-with-external-browser to use emacspeak-m-player
;;; instead.
(defadvice eww-browse-with-external-browser(around emacspeak pre act comp)
  "Use our m-player integration."
  (let* ((url (or (ad-get-arg 0) ""))
         (media-p (string-match emacspeak-media-extensions url)))
    (cond
     (media-p (emacspeak-m-player url))
     (t ad-do-it))))

;;}}}
;;{{{ eww-marks:

;;; Bookmarks for use in reading ebooks with EWW:
;;; They are called eww-marks to distinguish them from web bookmarks

(defvar emacspeak-eww-marks-file
  (expand-file-name "eww-marks" emacspeak-user-directory)
  "File where we save EWW marks.")
(cl-defstruct emacspeak-eww-mark
  type                             ; daisy, epub, epub-3
  book                             ; pointer to book --- type-specific
  point                            ; location in book
  name                             ; name of mark
  )

(defun emacspeak-eww-marks-load ()
  "Load saved marks."
  (interactive)
  (cl-declare (special emacspeak-eww-marks emacspeak-eww-marks-file))
  (when (file-exists-p emacspeak-eww-marks-file)
    (ems--fastload emacspeak-eww-marks-file)
    emacspeak-eww-marks))

(defvar emacspeak-eww-marks
  (cond
   ((file-exists-p emacspeak-eww-marks-file)
    (emacspeak-eww-marks-load))
   (t
    (make-hash-table :test #'equal)))
  "Stores   EWW-marks.")

(defun emacspeak-eww-add-mark (name)
  "Interactively add a mark with name title+`name' at current position."
  (interactive
   (list
    (concat
     (emacspeak-eww-current-title)": "
     (let ((input (read-from-minibuffer "Mark: " nil nil nil nil "current")))
       (if (zerop (length input))
           "current" input)))))
  (cl-declare (special emacspeak-eww-marks major-mode
                       emacspeak-epub-this-epub emacspeak-bookshare-this-book))
  (let ((bm
         (make-emacspeak-eww-mark
          :name name
          :type
          (cond
           ((bound-and-true-p emacspeak-epub-this-epub) 'epub)
           ((bound-and-true-p emacspeak-bookshare-this-book)'daisy)
           ((and (string-match "^file:///" (eww-current-url))
                 (not (string-match "^file:///tmp" (eww-current-url))))
            'local-file)
           (t (error "EWW marks only work in Local EWW pages,EPub and Bookshare buffers.")))
          :book
          (or
           (bound-and-true-p emacspeak-bookshare-this-book)
           (bound-and-true-p emacspeak-epub-this-epub)
           (substring (eww-current-url) 7))
          :point (point))))
    (puthash  name bm emacspeak-eww-marks)
    (emacspeak-eww-marks-save)
    (emacspeak-auditory-icon 'mark-object)
    (message "Created  EWW mark %s." name)))

(defun emacspeak-eww-jump-to-mark (bm)
  "Jump to eww-mark `bm' if  there is a buffer displaying that content."
  (let ((book  (emacspeak-eww-mark-book bm))
        (type (emacspeak-eww-mark-type bm))
        (point (emacspeak-eww-mark-point bm))
        (buffer nil))
    (setq
     buffer
     (cond
      ((eq type 'local-file)
       (cl-find-if
        #'(lambda (b)
            (string= book (with-current-buffer b
                            (and (eww-current-url)
                                 (substring (eww-current-url) 7)))))
        (buffer-list)))
      ((eq type 'epub)
       (require 'emacspeak-epub)
       (cl-find-if
        #'(lambda (b)
            (string= book (with-current-buffer b emacspeak-epub-this-epub)))
        (buffer-list)))
      ((eq type 'daisy)
       (require 'emacspeak-bookshare)
       (cl-find-if
        #'(lambda (b)
            (string= book
                     (with-current-buffer b emacspeak-bookshare-this-book)))
        (buffer-list)))
      (t (error "Unknown book type %s" type))))
    (when buffer
      (funcall-interactively #'pop-to-buffer buffer)
      (when point (goto-char point))
      (emacspeak-auditory-icon 'large-movement)
      t)))

(defun emacspeak-eww-delete-mark (name)
  "Interactively delete a mark with name `name' at current position."
  (interactive "sMark Name: ")
  (cl-declare (special emacspeak-eww-marks))
  (remhash name emacspeak-eww-marks)
  (emacspeak-eww-marks-save)
  (emacspeak-auditory-icon 'delete-object)
  (message "Removed Emacspeak EWW mark %s" name))

(declare-function emacspeak-bookshare-eww "emacspeak-bookshare" (directory))

;;;###autoload
(defun emacspeak-eww-open-mark (name &optional delete)
  "Open EWW marked location.  With optional interactive prefix
arg `delete', delete that mark instead."
  (interactive
   (list
    (progn
      (when (hash-table-empty-p emacspeak-eww-marks)
        (error "No Emacspeak EWW Marks found."))
      (completing-read "Mark: " emacspeak-eww-marks))
    current-prefix-arg))
  (cl-declare (special emacspeak-eww-marks))
  (cond
   (delete (emacspeak-eww-delete-mark name)
           (emacspeak-auditory-icon 'delete-object))
   (t
    (let* ((bm (gethash name emacspeak-eww-marks))
           (handler nil)
           (type (emacspeak-eww-mark-type bm))
           (point (emacspeak-eww-mark-point bm))
           (book (emacspeak-eww-mark-book bm)))
      (cl-assert  type nil "Mark type is not set.")
      (cl-assert book nil "Book not set.")
      (cond
       ((emacspeak-eww-jump-to-mark bm) t) ;;; Found a buffer with
;;; book open.
       (t ;;; so we need to first open the book:
        (setq handler
              (cond
               ((eq type 'daisy) #'emacspeak-bookshare-eww)
               ((eq type 'epub) #'emacspeak-epub-eww)
               ((eq type 'local-file) #'eww-open-file)
               (t (error "Unknown book type."))))
        (when point
          (add-hook
           'emacspeak-eww-post-process-hook
           #'(lambda ()
               (goto-char point)
               (emacspeak-auditory-icon 'large-movement))
           'at-end)
          (when (eq type 'local-file)
            (add-hook 'emacspeak-eww-post-process-hook
                      #'emacspeak-speak-line
                      'at-end))
          )
        (funcall handler book)))))))

(defun emacspeak-eww-marks-save ()
  "Save Emacspeak EWW marks."
  (interactive)
  (cl-declare (special emacspeak-eww-marks-file emacspeak-eww-marks))
  (when (hash-table-p emacspeak-eww-marks)
    (emacspeak--persist-variable 'emacspeak-eww-marks
                                 emacspeak-eww-marks-file)))

(defvar emacspeak-eww-marks-save-timer
  (run-at-time 3600 3600  #'emacspeak-eww-marks-save)
  "Idle timer for saving EWW marks.")

;;}}}
;;{{{ quick setup for reading:

(defun emacspeak-eww-reading-settings  ()
  "Setup speech-rate, punctuation and split-caps for reading prose."
  (interactive)
  (cl-declare (special dtk-speech-rate-base dtk-speech-rate-step))
  (dtk-set-rate (+ dtk-speech-rate-base (* dtk-speech-rate-step  3)))
  (dtk-set-punctuations 'some)
  (when dtk-split-caps(dtk-toggle-split-caps))
  (emacspeak-speak-rest-of-buffer))

;;}}}
;;{{{ Shell Command On URL Under Point:
(defvar emacspeak-eww-url-shell-commands
  (delete nil
          (list
           (expand-file-name "cbox" emacspeak-etc-directory)
           (expand-file-name "cbox-left" emacspeak-etc-directory)
           (expand-file-name "cbox-right" emacspeak-etc-directory)
           (expand-file-name "cbox-amp" emacspeak-etc-directory)
           (executable-find "youtube-dl")))
  "Shell commands we permit on URL under point.")

(defun emacspeak-eww-shell-command-on-url-at-point (&optional prefix)
  "Run specified shell command on URL at point.
Warning: Running shell script cbox through this fails mysteriously."
  (interactive "P")
  (cl-declare (special emacspeak-eww-url-shell-commands))
  (cl-assert (shr-url-at-point prefix) t "No URL at point.")
  (let ((url (shr-url-at-point prefix))
        (cmd
         (completing-read "Shell Command: " emacspeak-eww-url-shell-commands)))
    (shell-command (format "%s '%s'" cmd url))
    (emacspeak-auditory-icon 'task-done)))
;;}}}
;;{{{Smart Tabs:

(defvar emacspeak-eww-smart-tabs
  (make-hash-table :test #'eq)
  "Cache of  URL->Tabs mappings.")

(defsubst emacspeak-eww-smart-tabs-put (key url)
  " Add a  `URL'tou our smart tabs cache. "
  (cl-declare (special emacspeak-eww-smart-tabs))
  (puthash key url emacspeak-eww-smart-tabs))

(defsubst emacspeak-eww-smart-tabs-get (key)
  "Retrieve URL stored in `KEY'"
  (cl-declare (special emacspeak-eww-smart-tabs))
  (gethash key  emacspeak-eww-smart-tabs))

(defun emacspeak-eww-smart-tabs-add (char url )
  "Add a URL to the specified location in smart tabs."
  (interactive
   (list
    (read-char-exclusive "Tab:")
    (read-from-minibuffer "URL:")))
  (cl-declare (special emacspeak-eww-smart-tabs))
  (emacspeak-eww-smart-tabs-put char url)
  (emacspeak-auditory-icon 'close-object))

;;;###autoload
(defun emacspeak-eww-smart-tabs (char &optional define)
  "Open URL in EWW keyed by  `char'.
To associate a URL with a char, use this command
with an interactive prefix arg. "
  (interactive
   (list
    (read-char-exclusive "EWWTab:")
    current-prefix-arg))
  (cl-declare (special emacspeak-eww-smart-tabs))
  (unless
      (and
       (bound-and-true-p emacspeak-eww-smart-tabs)
       (not (hash-table-empty-p emacspeak-eww-smart-tabs)))
    (emacspeak-eww-smart-tabs-load))
  (when define
    (emacspeak-eww-smart-tabs-add char (read-from-minibuffer "URL:")))
  (let ((url (emacspeak-eww-smart-tabs-get char)))
    (cl-assert (stringp url) t "No URL stored in this location.")
    (emacspeak-auditory-icon 'button)
    (eww url)))

(defun emacspeak-eww-smart-tabs-save ()
  "Save our smart tabs to a file for reloading."
  (interactive)
  (when
      (and
       (bound-and-true-p emacspeak-eww-smart-tabs)
       (not (hash-table-empty-p emacspeak-eww-smart-tabs)))
    (emacspeak--persist-variable
     'emacspeak-eww-smart-tabs
     (expand-file-name "smart-eww-tabs" emacspeak-user-directory))))

(add-hook
 'kill-emacs-hook
 #'emacspeak-eww-smart-tabs-save)

(defun emacspeak-eww-smart-tabs-load ()
  "Load our smart tabsfrom a file."
  (interactive)
  (cl-declare (special emacspeak-user-directory))
  (when
      (file-exists-p
       (expand-file-name "smart-eww-tabs" emacspeak-user-directory))
    (ems--fastload
     (expand-file-name "smart-eww-tabs" emacspeak-user-directory))))

;;}}}
;;{{{Form filling:

(defun emacspeak-eww-fillin-form-field ()
  "Fill in user or passwd field using auth-source backend."
  (interactive)
  (emacspeak-eww-browser-check)
  (let ((url (eww-current-url))
        (result nil))
    (cl-assert url t "No current url")
    (setq result
          (cl-case
              (read-char "u  User, p Password")
            (?u  (url-user-for-url url))
            (?p  (url-password-for-url url))
            (otherwise nil)))
    (cl-assert result t "No value found to insert here")
    (when result (insert result))
    (emacspeak-speak-line)))

;;}}}
;;{{{Enable Table Browsing:

;;; Only works for plain tables, not nested tables.
;;; Point has to be within the displayed table.
;;; Property values are part of the content,
;;; And consequently the DOM ends up pointing back at itself.
;;; This makes looking at the DOM hard, doesn't appear to have any
;;;other negatives.
;;; Overlays may avoid this problem.

(defadvice shr-tag-table-1 (around emacspeak pre act comp)
  "Cache pointer to table dom as a text property,
and add relevant properties to the rendered region."
  (let ((table-dom (ad-get-arg 0))
        (start (point)))
    ad-do-it
    (unless (get-text-property start 'table-dom)
      (add-text-properties
       start (point)
       (list
        'auditory-icon 'fill-object
        'table-start start
        'table-end (1-  (point))
        'table-dom table-dom)))
    ad-return-value))

(defvar-local emacspeak-eww-table-cell 0
  "Track current table cell to enable table navigation.
Value is specified as a position in the list of table cells.")

(defsubst emacspeak-eww-table-table ()
  "Return table cells as a table, a 2d structure."
  (let* ((data nil)
         (table (get-text-property (point) 'table-dom))
         (head (dom-by-tag table 'th)))
    (cl-assert table t "No table here.")
    (setq data
          (cl-loop
           for r in (dom-by-tag table 'tr) collect
           (cl-loop
            for c in
            (append
             (dom-by-tag r 'th)
             (dom-by-tag r 'td))
            collect
            (string-trim (dom-node-as-text c)))))
    ;;; handle head case differently:
    (if head
        (apply #'vector (mapcar #'vconcat  (cdr data)))
      (apply #'vector (mapcar #'vconcat  data)))))

(defsubst emacspeak-eww-table-cells ()
  "Returns  table cells as a list."
  (let* ((table (get-text-property (point) 'table-dom))
         (head (dom-by-tag table 'th)))
    (cond
     (head (cdr (append head (dom-by-tag table 'td))))
     (t (dom-by-tag table 'td)))))

(defsubst emacspeak-eww-table-row-count ()
  "Returns number of table rows."
  (length (dom-by-tag (get-text-property (point) 'table-dom) 'tr)))

(defsubst emacspeak-eww-table-cell-count ()
  "Returns number of  table cells."
  (length (emacspeak-eww-table-cells)))

(defun emacspeak-eww-table-speak-dimensions ()
  "Speak number of rows and cells."
  (interactive)
  (dtk-speak
   (format "Table with %s rows and %s cells"
           (emacspeak-eww-table-row-count) (emacspeak-eww-table-cell-count))))

(defsubst emacspeak-eww-table-speak-cell ()
  "Speak current cell."
  (interactive)
  (cl-declare (special emacspeak-eww-table-cell))
  (dtk-speak
   (dom-node-as-text
    (elt (emacspeak-eww-table-cells) emacspeak-eww-table-cell))))

(defun emacspeak-eww-table-previous-row (&optional prefix)
  "Speak  cell after moving to previous row.
 Optional interactive prefix arg moves to start of table."
  (interactive "P")
  (cl-declare (special emacspeak-eww-table-cell))
  (emacspeak-eww-browser-check)
  (cond
   (prefix
    (goto-char (get-text-property (point) 'table-start))
    (setq emacspeak-eww-table-cell 0))
   (t
    (let* ((n-rows (emacspeak-eww-table-row-count))
           (n-cells (emacspeak-eww-table-cell-count))
           (quotient (/ n-cells n-rows)))
      (cl-assert
       (>= emacspeak-eww-table-cell quotient)
       t "On first row.")
      (cl-decf emacspeak-eww-table-cell quotient)
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-eww-table-speak-cell)))))

(defun emacspeak-eww-table-next-row (&optional prefix)
  "Speak  cell after moving to next row.
 Optional interactive prefix arg moves to end of table."
  (interactive "P")
  (cl-declare (special emacspeak-eww-table-cell))
  (emacspeak-eww-browser-check)
  (cond
   (prefix
    (goto-char (get-text-property (point) 'table-end))
    (setq
     emacspeak-eww-table-cell
     (1- (length (emacspeak-eww-table-cells)))))
   (t
    (let* ((n-rows (emacspeak-eww-table-row-count))
           (n-cells (emacspeak-eww-table-cell-count))
           (quotient (/ n-cells n-rows)))
      (cl-assert
       (< (+ emacspeak-eww-table-cell quotient) n-cells)
       t "On last row.")
      (cl-incf emacspeak-eww-table-cell quotient)
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-eww-table-speak-cell)))))

(defun emacspeak-eww-table-next-cell (&optional prefix)
  "Speak next cell after making it current.
Interactive prefix arg moves to the last cell in the table."
  (interactive "P")
  (cl-declare (special emacspeak-eww-table-cell))
  (emacspeak-eww-browser-check)
  (cl-assert
   (< (1+ emacspeak-eww-table-cell) (length (emacspeak-eww-table-cells)))
   t "On last cell.")
  (cond
   (prefix
    (goto-char (get-text-property (point) 'table-end))
    (cl-incf emacspeak-eww-table-cell
             (1- (length (emacspeak-eww-table-cells)))))
   (t
    (goto-char (next-single-property-change (point) 'display))
    (skip-syntax-forward " ")
    (cl-incf emacspeak-eww-table-cell 1)
    (goto-char (next-single-property-change (point) 'display))))
  (emacspeak-auditory-icon 'left)
  (emacspeak-eww-table-speak-cell))

(defun emacspeak-eww-table-previous-cell (&optional prefix)
  "Speak previous cell after making it current.
With interactive prefix arg, move to the start of the table."
  (interactive "P")
  (cl-declare (special emacspeak-eww-table-cell))
  (emacspeak-eww-browser-check)
  (when  (zerop emacspeak-eww-table-cell  ) (error  "On first cell."))
  (cond
   (prefix
    (goto-char (get-text-property (point) 'table-start))
    (setq emacspeak-eww-table-cell 0)
    (goto-char (get-text-property (point) 'table-start)))
   (t
    (goto-char (previous-single-property-change (point) 'display))
    (skip-syntax-backward " ")
    (cl-decf emacspeak-eww-table-cell 1)))
  (emacspeak-auditory-icon 'right)
  (emacspeak-eww-table-speak-cell))

(defun emacspeak-eww-table-data ()
  "View  table at point as a data table using Emacspeak Table UI."
  (interactive)
  (let ((data (emacspeak-eww-table-table))
        (data-table nil)
        (inhibit-read-only  t)
        (buffer
         (get-buffer-create
          (format  "Table: %s" (emacspeak-eww-current-title)))))
    (setq data-table (emacspeak-table-make-table data))
    (emacspeak-table-prepare-table-buffer data-table buffer)))

;;}}}
;;{{{Dive Into DOM: div

(defadvice shr-tag-div (around eww-dom pre act comp)
  "Persist dom to the div node as a text property."
  (let ((start (point)))
    ad-do-it
    (unless (get-text-property start 'eww-dom)
      (put-text-property
       start (point)
       'eww-dom (ad-get-arg 0)))
    ad-return-value))

(defun emacspeak-eww-dive-into-div ()
  "Focus on current div by rendering it in a new buffer."
  (interactive)
  (let ((dom (get-text-property (point) 'eww-dom)))
    (cl-assert (memq 'div (emacspeak-eww-here-tags) ) t "No div here.")
    (emacspeak-eww-view-helper
     (dom-html-from-nodes (list dom) (eww-current-url)))))

;;}}}
;;{{{Open With External Browser: EAF, Chrome

(declare-function eaf-open-browser "eaf-browser" (url &optional args))

(defun emacspeak-eww-browse-eaf  (url)
  "Launch async EAF browser."
  (interactive (list (emacspeak-eww-read-url)))
  (unless(require 'eaf)
    (error "Install Emacs Application Framework"))
  (require 'eaf-browser)
  (make-thread #'(lambda ()(eaf-open-browser url))))

(defun emacspeak-eww-browse-chrome (url)
  "Open with Chrome."
  (interactive (list (emacspeak-eww-read-url)))
  (browse-url-chrome url))

;;}}}
(provide 'emacspeak-eww)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

;;;Auto generated

;;;### (autoloads (gcal-emacs-calendar-setup gcal-show-event gcal-show-calendar
;;;;;;  gcal-delete-event gcal-add-event gcal-read-event) "gcal"
;;;;;;  "gcal.el" (17895 27547))
;;; Generated autoloads from gcal.el

(autoload (quote gcal-read-event) "gcal" "\
Prompt user for event params and return an event structure.

\(fn TITLE CONTENT WHERE START END WHO TRANSPARENCY STATUS)" t nil)

(autoload (quote gcal-add-event) "gcal" "\
Add a calendar event.

\(fn)" t nil)

(autoload (quote gcal-delete-event) "gcal" "\
Delete a calendar event.

\(fn EVENT-URI)" t nil)

(autoload (quote gcal-show-calendar) "gcal" "\
Show calendar for currently authenticated user.

\(fn &optional CALENDAR START-MIN START-MAX)" t nil)

(autoload (quote gcal-show-event) "gcal" "\
Show event at URL.

\(fn URL)" t nil)

(define-prefix-command (quote gcal-calendar-prefix-map))

(autoload (quote gcal-emacs-calendar-setup) "gcal" "\
Setup GCal keybindings in Emacs calendar.

\(fn)" nil nil)

;;;***

;;;### (autoloads (greader-find-feeds greader-star greader-add-label
;;;;;;  greader-untag-feed greader-tag-feed greader-title-feed greader-unsubscribe-feed
;;;;;;  greader-subscribe-feed greader-feed-list greader-preferences
;;;;;;  greader-reading-list) "greader" "greader.el" (17895 4046))
;;; Generated autoloads from greader.el

(autoload (quote greader-reading-list) "greader" "\
Ensure our cookies are live, and get the reading list.
Optional interactive prefix `state' prompts for state to retrieve

e.g., starred.

\(fn &optional STATE)" t nil)

(autoload (quote greader-preferences) "greader" "\
Ensure our cookies are live, and get all preferences for this
user.

\(fn)" t nil)

(autoload (quote greader-feed-list) "greader" "\
Retrieve list of subscribed feeds.
Optional interactive prefix arg `sort' sorts feeds based on newly
arrived articles.

\(fn &optional SORT)" t nil)

(autoload (quote greader-subscribe-feed) "greader" "\
Subscribe to specified feed.

\(fn FEED-URL)" t nil)

(autoload (quote greader-unsubscribe-feed) "greader" "\
UnSubscribe from specified feed.

\(fn FEED-URL)" t nil)

(autoload (quote greader-title-feed) "greader" "\
Title  specified feed.

\(fn FEED-URL)" t nil)

(autoload (quote greader-tag-feed) "greader" "\
Tag  specified feed.

\(fn FEED-URL)" t nil)

(autoload (quote greader-untag-feed) "greader" "\
Remove Tag from specified feed.

\(fn FEED-URL)" t nil)

(autoload (quote greader-add-label) "greader" "\
Add label to this item.

\(fn ITEM-URL LABEL)" t nil)

(autoload (quote greader-star) "greader" "\
Star this item.

\(fn ITEM-URL)" t nil)

(autoload (quote greader-find-feeds) "greader" "\
Find feeds matching query.

\(fn QUERY)" t nil)

;;;***

;;;### (autoloads nil nil ("g-auth.el" "g-autogen.el" "g-cus-load.el"
;;;;;;  "g-load-path.el" "g-utils.el" "g.el" "json.el") (17895 29504
;;;;;;  291557))

;;;***

;;;### (autoloads (gblogger-publish gblogger-put-entry gblogger-post-entry
;;;;;;  gblogger-new-entry gblogger-edit-entry gblogger-atom-display
;;;;;;  gblogger-blog) "gblogger" "gblogger.el" (17895 4046))
;;; Generated autoloads from gblogger.el

(autoload (quote gblogger-blog) "gblogger" "\
Retrieve and display feed of feeds after authenticating.

\(fn)" t nil)

(autoload (quote gblogger-atom-display) "gblogger" "\
Retrieve and display specified feed after authenticating.

\(fn URL)" t nil)

(autoload (quote gblogger-edit-entry) "gblogger" "\
Retrieve entry and prepare it for editting.
The retrieved entry is placed in a buffer ready for editing.
`url' is the URL of the entry.

\(fn URL)" t nil)

(autoload (quote gblogger-new-entry) "gblogger" "\
Create a new Blog post.

\(fn URL)" t nil)

(autoload (quote gblogger-post-entry) "gblogger" "\
Post buffer contents  as  updated entry.

\(fn)" t nil)

(autoload (quote gblogger-put-entry) "gblogger" "\
PUT buffer contents as new entry.

\(fn)" t nil)

(autoload (quote gblogger-publish) "gblogger" "\
Publish current entry.

\(fn)" t nil)

;;;***

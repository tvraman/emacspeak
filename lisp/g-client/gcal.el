;;; gcal.el --- Google Calendar
;;;$Id: gcal.el,v 1.30 2006/09/28 17:47:44 raman Exp $
;;; $Author: raman $
;;; Description:  Google Calendar
;;; Keywords: Google   Atom API
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; gcal| T. V. Raman |raman@cs.cornell.edu
;;; An emacs interface to Reader|
;;; $Date: 2006/09/28 17:47:44 $ |
;;;  $Revision: 1.30 $ |
;;; Location undetermined
;;; License: GPL
;;;

;;}}}
;;{{{ Copyright:

;;; Copyright (c) 2006 and later, Google Inc.
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without modification,
;;; are permitted provided that the following conditions are met:

;;;     * Redistributions of source code must retain the above copyright notice,
;;;       this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright notice,
;;;       this list of conditions and the following disclaimer in the documentation
;;;       and/or other materials provided with the distribution.
;;;     * The name of the author may not be used to endorse or promote products
;;;       derived from this software without specific prior written permission.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
;;; WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;{{{  introduction

;;; Google Calendar
;;; Create, Browse, Find ...  integrate Emacs Calendar with
;;; Google Calendar.
;;; http://code.google.com/apis/calendar/overview.html

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'calendar)
(require 'g-utils)
(require 'g-auth)
(require 'browse-url)

;;}}}
;;{{{ Customizations

(defgroup gcal nil
  "Google Calendar"
  :group 'g)

(defcustom gcal-user-email nil
  "Mail address that identifies calendar user."
  :type '(choice
          (const :tag "none" nil)
          (string :tag "username@gmail.com" ""))
  :group 'gcal)

(defcustom gcal-user-password nil
  "Password for authenticating to calendar account."
  :type '(radio (const :tag "Prompt for password" nil)
                (string :tag "Save password in .emacs"))
  :group 'gcal)

;;}}}
;;{{{ Constants

(defconst gcal-service-name "cl"
  "Service name for accessing Google Calendar.")

(defsubst gcal-p (service)
  "Check if this is Calendar."
  (declare (special gcal-service-name))
  (string-equal service gcal-service-name))

;;}}}
;;{{{ Calendar Authenticate

(defsubst make-gcal-auth ()
  "Make a new gcal auth handle."
  (declare (special gcal-service-name
                    gcal-user-email gcal-user-password))
  (make-g-auth :service gcal-service-name
               :email gcal-user-email
               :password gcal-user-password))

(defvar gcal-auth-handle
  (make-gcal-auth)
  "G auth handle used for signing into calendar.")

(defun gcal-authenticate ()
  "Authenticate into Google Calendar."
  (declare (special gcal-auth-handle))
  (g-authenticate gcal-auth-handle))

;;}}}
;;{{{ Event Template:

;;; template for calendar event.
;;; Format string takes following:
;; (title content
;;        author-name author-email
;;        transparency status
;;        where
;;        when-start when-end)

(defvar gcal-event-template
  "<entry xmlns='http://www.w3.org/2005/Atom' xmlns:gd='http://schemas.google.com/g/2005'>
<category scheme='http://schemas.google.com/g/2005#kind' term='http://schemas.google.com/g/2005#event'>
</category>
<title type='text'>%s </title>
<content type='text'>%s </content>
<author>
<name>%s </name>
<email>%s </email>
</author>
<gd:transparency value='%s'></gd:transparency>
<gd:eventStatus value='%s'></gd:eventStatus>
<gd:where valueString='%s'></gd:where>
<gd:when startTime='%s' endTime='%s'></gd:when>
%s
</entry> "
  "XML template for  calendar events.")

(defvar gcal-attendee-template
  "<gd:who rel='%s'
    valueString='%s'
    email='%s'>
    <gd:attendeeStatus value='%s'/>
    <gd:attendeeType value='%s'/>
</gd:who>
"
  "XML template  for attendees.")

;;}}}
;;{{{ Event, attendee Structure

(defstruct gcal-event
  title content
  (author-name (user-login-name))
  author-email
  transparency status
  where
  when-start when-end
  who)

(defun gcal-attendee-list-as-xml (attendees)
  "Return XML representation of list of attendees."
  (declare (special gcal-attendee-template))
  (mapconcat
   #'(lambda (w)
       (format gcal-attendee-template
               (or (gcal-attendee-role w)  "")
               (or (gcal-attendee-cn w) "")
               (or (gcal-attendee-email w) "")
               (or (gcal-attendee-resource-type w) "")
               (or (gcal-attendee-status w) "")
               ))
   attendees
   "\n"))

(defun gcal-event-as-xml (e)
  "Return XML representation of event."
  (declare (special gcal-event-template))
  (format gcal-event-template
          (gcal-event-title e) (gcal-event-content e)
          (gcal-event-author-name e) (gcal-event-author-email e)
          (gcal-event-transparency e) (gcal-event-status e)
          (gcal-event-where e)
          (gcal-event-when-start e) (gcal-event-when-end e)
          (gcal-attendee-list-as-xml
           (gcal-event-who e))))

(defstruct gcal-attendee
  email
  cn                                    ;firstname lastname
  status                                ; needs-action, confirmed
  role                       ; required participant, optional etc
  resource-type                         ; individual, room
  )

;;}}}
;;{{{  Reading events

(defvar gcal-event-transparency-alist
  '((opaque . "http://schemas.google.com/g/2005#event.opaque"))
  "Alist of event transparency types.")

(defsubst gcal-event-transparency-value (type)
  "Return identifier for specified event transparency type."
  (declare (special gcal-event-transparency-alist))
  (let ((pair (assoc type gcal-event-transparency-alist)))
    (when pair (cdr pair))))

(defvar gcal-event-status-alist
  '((confirmed
     . "http://schemas.google.com/g/2005#event.confirmed"))
  "Alist of event status types.")

(defsubst gcal-event-status-value (type)
  "Return identifier for specified event status type."
  (declare (special gcal-event-status-alist))
  (let ((pair (assoc type gcal-event-status-alist)))
    (when pair (cdr pair))))

(defsubst gcal-read-transparency ()
  "Get transparency for event."
  (declare (special gcal-event-transparency-alist))
  (gcal-event-transparency-value 'opaque))

(defsubst gcal-read-status ()
  "Get status for event."
  (declare (special gcal-event-status-alist))
  (gcal-event-status-value 'confirmed))
(defcustom gcal-calendar-view
  (expand-file-name "gcal-view.xsl" g-directory)
  "XSL transform used to view event feeds."
  :group 'gcal)

(defcustom gcal-event-view
  (expand-file-name "gevent-view.xsl" g-directory)
  "XSL transform used to view event feeds."
  :group 'gcal)

(defcustom gcal-event-time-format "%Y-%m-%dT%H:%M:%S.000"
  "Time format used for events."
  :type 'string
  :group 'gcal)

(defsubst gcal-format-time-string (&optional time-spec)
  "Like format-time-string, except we produce a zone offset per
  Google Calendar spec. `time-spec' defaults to current time."
  (declare (special gcal-event-time-format))
  (or time-spec
      (setq time-spec (current-time)))
  (let* ((z (format-time-string "%z"))
         (zone (format "%s:%s" (substring z  0 -2) (substring z -2))))
    (format "%s%s"
            (format-time-string gcal-event-time-format time-spec)
            zone)))

(defsubst gcal-read-time (prompt)
  "Read time and return it suitably formatted."
  (read-from-minibuffer prompt
                        (gcal-format-time-string)))

(defun gcal-read-calendar-date (prompt )
  "Smart date collector.
Prompts with `prompt'.
This function is sensitive to calendar mode when prompting."
  (let ((default (format-time-string "%m/%d/%Y"))
        (result nil))                   ; today is default
    (when (eq major-mode 'calendar-mode)
      (let ((date (calendar-cursor-to-nearest-date)))
        (setq default
              (format "%s/%s/%s"
                      (first date)
                      (second date)
                      (third date)))))
    (setq result
          (read-from-minibuffer prompt
                                default
                                nil nil nil
                                default))
    (setq result
          (mapcar 'read
                  (split-string result "/")))
    (gcal-format-time-string
     (apply 'encode-time
            0 0 0
            (second result)
            (first result)
            (list (third result ))))))

(defun gcal-read-calendar-time (prompt)
  "Smart time collector.
Prompts with `prompt'.
Default date is assumed to be today, or  the date selected when
  in the calendar."
  (let ((date nil)
        (now nil)
        (result nil))                   ; today is default
    (cond
     ((eq major-mode 'calendar-mode)
      (setq date (calendar-cursor-to-nearest-date))
      (setq date (list (second date)
                       (first date)
                       (third date))))
     (t  (setq now (decode-time))
         (setq date
               (list (nth 3 now)
                     (nth 4 now)
                     (nth 5 now)))))
    (setq result
          (read-from-minibuffer prompt
                                (format-time-string "%H:%M")
                                nil nil nil
                                (format-time-string "%H:%M")))
    (setq result
          (mapcar 'read
                  (split-string result ":")))
    (gcal-format-time-string
     (apply 'encode-time
            0 (second result) (first result)
            date))))

(defsubst gcal-read-who (prompt)
  "Read list of participants."
  (let ((who nil)
        (participant nil)
        (email nil))
    (while
        (>
         (length
          (setq email (read-from-minibuffer
                       (format "%s email: Blank to quit" prompt) )))
         0)
      (setq participant
            (make-gcal-attendee
             :email email
             :cn email
             :role "http://schemas.google.com/g/2005#event.attendee"))
      (push participant who))
    who))

;;;###autoload
(defun gcal-read-event (title content
                              where
                              start end
                              who
                              transparency status)
  "Prompt user for event params and return an event structure."
  (interactive
   (list
    (read-from-minibuffer "Title: ")
    (read-from-minibuffer "Content: ")
    (read-from-minibuffer "Where: ")
    (gcal-read-calendar-time "Start Time: ")
    (gcal-read-calendar-time "End Time: ")
    (gcal-read-who "Participant: ")
    (gcal-read-transparency)
    (gcal-read-status)))
  (declare (special gcal-auth-handle))
  (let ((event (make-gcal-event
                :author-email (g-auth-email gcal-auth-handle))))
    (setf (gcal-event-title  event) title
          (gcal-event-content event) content
          (gcal-event-where event) where
          (gcal-event-when-start event) start
          (gcal-event-when-end event) end
          (gcal-event-who event) who
          (gcal-event-transparency  event) transparency
          (gcal-event-status event) status)
    event))

;;}}}
;;{{{ Events:

(defvar gcal-default-feed-url
  "http://www.google.com/calendar/feeds/default/private/full"
  "URL for default calendar feed for currently authenticated
user.")

(defvar gcal-private-feed-url
  "http://www.google.com/calendar/feeds/%s/private/full"
  "URL for private calendar feed using authentication.")

(defsubst gcal-private-feed-url ()
  "Return private feed for authenticated user."
  (declare (special gcal-private-feed-url
                    gcal-auth-handle))
  (format gcal-private-feed-url
          (g-url-encode (g-auth-email gcal-auth-handle))))

(defsubst gcal-post-event (event location)
  "Post event via HTTPS to location and return resulting HTTP headers."
  (declare (special g-cookie-options gcal-auth-handle
                    g-curl-program g-curl-common-options))
  (g-using-scratch
   (insert (gcal-event-as-xml event))
   (let ((cl (format "-H Content-length:%s" (buffer-size)))
         (status nil))
     (shell-command-on-region
      (point-min) (point-max)
      (format
       "%s %s %s %s %s -i -X POST --data-binary @- %s 2>/dev/null"
       g-curl-program g-curl-common-options cl
       (g-authorization gcal-auth-handle)
       g-cookie-options
       location)
      (current-buffer) 'replace)
     (list (g-http-headers (point-min) (point-max))
           (g-http-body (point-min) (point-max))))))

;;;###autoload
(defun gcal-add-event ()
  "Add a calendar event."
  (interactive)
  (declare (special gcal-auth-handle))
  (g-auth-ensure-token gcal-auth-handle)
  (let ((event (call-interactively 'gcal-read-event))
        (status nil)
        (headers nil)
        (body nil)
        (response nil)
        (location nil))
    (setq response
          (gcal-post-event event (gcal-private-feed-url)))
    (setq headers (first response)
          body (second response))
    (setq status (g-http-header "Status" headers))
    (when (string= "302" status)
      (setq location (g-http-header "Location" headers))
      (unless location
        (error "Could not find redirect."))
      (setq response  (gcal-post-event event location)))
    (setq headers (first response)
          body (second response))
    (when  (string-equal "201" (g-http-header "Status" headers))
      (and (> 0(length body))
           (g-display-xml-string body
                                 gcal-calendar-view))
      (message "Event added as %s"
               (g-http-header "Location" headers)))))

(defun gcal-quick-add-event ()
  "Add a calendar event using simple English."
  (interactive)
  (declare (special gcal-auth-handle))
  (g-auth-ensure-token gcal-auth-handle)
  (let ((event (read-from-minibuffer "Quick Add Event: ")))
    (g-using-scratch
     (shell-command
      (format
       "%s -i   %s %s  %s"
       g-curl-program g-curl-common-options
       (g-authorization gcal-auth-handle)
       (format "%s/event?ctext=%s"
               (gcal-private-feed-url)
               (g-url-encode event)))))))

;;;###autoload
(defun gcal-delete-event (event-uri)
  "Delete a calendar event."
  (interactive
   (list
    (read-from-minibuffer "Event URL: "
                          (browse-url-url-at-point))))
  (declare (special gcal-auth-handle))
  (g-auth-ensure-token gcal-auth-handle)
  (let ((headers nil)
        (body nil)
        (response nil))
    (g-using-scratch
     (shell-command
      (format
       "%s %s %s %s -i -X DELETE %s 2>/dev/null"
       g-curl-program g-curl-common-options
       (g-authorization gcal-auth-handle)
       g-cookie-options
       event-uri)
      (current-buffer) 'replace))))

(defcustom gcal-event-accept
  (expand-file-name "gevent-accept.xsl" g-directory)
  "XSL transform used to accept events."
  :group 'gcal)

(defun gcal-accept-event (event-uri)
  "Accept (RSVP)  a calendar event."
  (interactive
   (list
    (read-from-minibuffer "Event URL: "
                          (browse-url-url-at-point))))
  (declare (special gcal-auth-handle g-cookie-options
                    gcal-event-accept))
  (g-auth-ensure-token gcal-auth-handle)
  (g-using-scratch
   (shell-command
    (format
     "%s %s %s %s  -X GET %s 2>/dev/null"
     g-curl-program g-curl-common-options
     (g-authorization gcal-auth-handle)
     g-cookie-options
     event-uri)
    (current-buffer) 'replace)
   (shell-command-on-region
    (point-min) (point-max)
    (format
     "%s --param 'email' \"'%s'\" %s - 2>/dev/null"
     g-xslt-program
     (g-auth-email gcal-auth-handle)
     gcal-event-accept )
    (current-buffer) 'replace)
   (let ((cl (format "-H Content-length:%s" (buffer-size))))
     (shell-command-on-region
      (point-min) (point-max)
      (format
       "%s %s %s %s %s -i -X PUT --data-binary @- %s 2>/dev/null"
       g-curl-program g-curl-common-options cl
       (g-authorization gcal-auth-handle)
       g-cookie-options
       event-uri)
      (current-buffer) 'replace))))

;;;###autoload
(defun gcal-show-calendar (&optional calendar start-min start-max)
  "Show calendar for currently authenticated user."
  (interactive
   (list
    (if current-prefix-arg
        (read-from-minibuffer "Calendar URI:"
                              (gcal-private-feed-url))
      (gcal-private-feed-url))
    (if current-prefix-arg
        (read-from-minibuffer "Start:"
                              (format-time-string
                               gcal-event-time-format
                               (current-time))))
    (if current-prefix-arg
        (read-from-minibuffer "End:"
                              (format-time-string
                               gcal-event-time-format (current-time))))))
  (declare (special gcal-auth-handle
                    gcal-calendar-view))
  (g-auth-ensure-token gcal-auth-handle)
  (g-display-result
   (format
    "%s %s %s %s  '%s%s' 2>/dev/null"
    g-curl-program g-curl-common-options
    (g-authorization gcal-auth-handle)
    g-cookie-options
    (or calendar (gcal-private-feed-url))
    (cond
     ((and (null start-min)
           (null start-max))
      "?orderby=starttime")
     (t
      (format "?orderby=starttime&start-min=%s&start-max=%s"
              (or start-min "")
              (or start-max "")))))
   gcal-calendar-view))

(defcustom gcal-calendar-agenda-days 5
  "Number of days for which we show an agenda by default."
  :type 'integer
  :group 'gcal)

(defun gcal-calendar-get-date (&optional date )
  "Get GCal date from a calendar date spec.
Default is to use calendar date under point."
  (setq date (or date (calendar-current-date)))
  (gcal-format-time-string
   (apply 'encode-time
          0 0 0
          (second date)
          (first date)
          (list (third date)))))

(defun gcal-calendar-agenda  (&optional calendar)
  "Show agenda for currently authenticated user.
With interactive prefix arg, prompts for calendar to show. This
command is best invoked from within the emacs calendar. It uses
point and mark in the calendar to determine the days for which we
show the agenda. If no mark is set in the calendar, the agenda is
shown for the next `gcal-calendar-agenda-days' days following the
date under point."
  (interactive
   (list
    (if current-prefix-arg
        (read-from-minibuffer "Calendar URI:"
                              (gcal-private-feed-url))
      (gcal-private-feed-url))))
  (declare (special gcal-auth-handle
                    gcal-calendar-view
                    calendar-mark-ring))
  (let* ((start-min
          (gcal-calendar-get-date
           (when (eq major-mode 'calendar-mode)
             (calendar-cursor-to-nearest-date))))
         (start-max
          (cond
           ((and (eq major-mode 'calendar-mode)
                 (car calendar-mark-ring))
            (gcal-calendar-get-date (car calendar-mark-ring)))
           ((eq major-mode 'calendar-mode)
            (gcal-calendar-get-date
             (calendar-gregorian-from-absolute
              (+ (calendar-absolute-from-gregorian (calendar-cursor-to-nearest-date))
                 gcal-calendar-agenda-days))))
           (t
            (gcal-calendar-get-date
             (calendar-gregorian-from-absolute
              (+ (calendar-absolute-from-gregorian (calendar-current-date))
                 gcal-calendar-agenda-days)))))))
    (g-auth-ensure-token gcal-auth-handle)
    (g-display-result
     (format
      "%s %s %s %s '%s%s' 2>/dev/null"
      g-curl-program g-curl-common-options
      (g-authorization gcal-auth-handle)
      g-cookie-options
      (or calendar (gcal-private-feed-url))
      (cond
       ((and (null start-min)
             (null start-max))
        "?orderby=starttime")
       (t
        (format "?orderby=starttime&start-min=%s&start-max=%s"
                (or start-min "")
                (or start-max "")))))
     gcal-calendar-view)))

;;;###autoload
(defun gcal-show-event (url)
  "Show event at URL."
  (interactive "sURL:")
  (declare (special gcal-auth-handle
                    gcal-event-view))
  (g-auth-ensure-token gcal-auth-handle)
  (g-display-result
   (format
    "%s %s %s %s   %s 2>/dev/null"
    g-curl-program g-curl-common-options
    (g-authorization gcal-auth-handle)
    g-cookie-options
    url)
   gcal-event-view))

;;}}}
;;{{{ Interfacing with Emacs Calendar:
;;;###autoload
(define-prefix-command  'gcal-calendar-prefix-map)

;;;###autoload
(defun gcal-emacs-calendar-setup ()
  "Setup GCal keybindings in Emacs calendar."
  (declare (special calendar-mode-map
                    gcal-calendar-prefix-map))
  (define-key calendar-mode-map "c" 'gcal-calendar-prefix-map)
  (loop for binding  in
        '(
          ("a" gcal-add-event)
          ("c" gcal-calendar-agenda)
          ("s" gcal-show-calendar)
          )
        do
        (define-key gcal-calendar-prefix-map (first binding)
          (second binding))))

;;}}}
(provide 'gcal)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

;;;_ org2blogger.el --- Code for exporting org-mode to g-client

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: convenience, tools, outlines

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;_ , Commentary:

;; 


;;;_ , Requires

(require 'org)
(require 'org-exp)
(require 'gblogger)

;;;_. Body

(defun org2gblogger ()
   "Export the current org buffer to gblogger."
   
   (interactive)
   (unless (org-mode-p)
      (error "Only useful in an org-mode buffer"))
   
   (let*
      ((plist (org-infile-export-plist))
	 (title
	    (or
	       (plist-get plist :title)
	       nil))
	 (str
	    (org-export-as-html nil nil nil 'string t))
	 (url
	    gblogger-posting-url))
      
      (gblogger-new-entry url title str)))


;;;_. Footers
;;;_ , Provides

(provide 'org2blogger)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; org2blogger.el ends here

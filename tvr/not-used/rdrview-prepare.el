;; inspired by  https://jiewawa.me/2024/04/another-way-of-integrating-mozilla-readability-in-emacs-eww/

(  defun   rdr-eww-rename-buffer   () 
   (  when   (  eq   major-mode   'eww-mode  ) 
     (  when-let   ((  string   (  or   (  plist-get   eww-data   :title  ) 
                            (  plist-get   eww-data   :url  )))) 
       (  format   "%s *eww*"   string  ))))

(  setq   eww-auto-rename-buffer   'rdr-eww-rename-buffer  )
(  define-minor-mode   eww-rdrview-mode 
   "Toggle whether to use   `rdrview'   to make eww buffers more readable." 
   :lighter   " rdrview" 
   (  if   eww-rdrview-mode 
       (  progn 
         (  setq   eww-retrieve-command   '  (  "rdrview"   "-T"   "title,sitename,body"   "-H"  )) 
         (  add-hook   'eww-after-render-hook   #'  eww-rdrview-update-title  )) 
     (  progn 
       (  setq   eww-retrieve-command   nil  ) 
       (  remove-hook   'eww-after-render-hook   #'
          eww-rdrview-update-title  ))))

(  defun   eww-rdrview-update-title   () 
   "Change title key in   `eww-data'   with first line of buffer.
  It should be the title of the web page as returned by   `rdrview'  " 
   (  save-excursion 
     (  goto-char   (  point-min  )) 
     (  plist-put   eww-data   :title   (  string-trim   (  thing-at-point   'line   t  )))) 
   (  eww--after-page-change  ))

(  defun   eww-rdrview-toggle-and-reload   () 
  "Toggle   `eww-rdrview-mode'   and reload page in current eww buffer." 
  (  interactive  ) 
  (  if   eww-rdrview-mode   (  eww-rdrview-mode   -1  ) 
    (  eww-rdrview-mode   1  )) 
  (  eww-reload  ))

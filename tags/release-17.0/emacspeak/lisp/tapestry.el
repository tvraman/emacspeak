;;;  Tools to configure your GNU Emacs windows
;;; Copyright (C) 1991, 1993, 1994, 1995, 1997 Kyle E. Jones 
;;;

;;;$Id$
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to kyle@uunet.uu.net) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; Send bug reports to kyle@uunet.uu.net.

(provide 'tapestry)

(defvar tapestry-version "1.08")

;; Pass state information between the tapestry-set-window-map
;; and tapestry-set-buffer-map stages.  UGH.  The reason for this
;; is explained in tapestry-set-buffer-map.
(defvar tapestry-windows-changed nil)

(defun tapestry (&optional frame-list)
  "Returns a list containing complete information about the current
configuration of Emacs frames, windows, buffers and cursor
positions.  Call the function set-tapestry with the list that this function
returns to restore the configuration.

Optional first arg FRAME-LIST should be a list of frames; only
configuration information about these frames will be returned.

The configuration information is returned in a form that can be saved and
restored across multiple Emacs sessions."
  (let ((frames (or frame-list (tapestry-frame-list)))
	(frame-map (tapestry-frame-map))
	(sf (tapestry-selected-frame))
	(other-maps nil))
    (unwind-protect
	(while frames
	  (tapestry-select-frame (car frames))
	  (setq other-maps (cons (list (tapestry-window-map)
				       (tapestry-buffer-map)
				       (tapestry-position-map))
				 other-maps)
		frames (cdr frames)))
      (tapestry-select-frame sf))
    (list frame-map other-maps)))

(defun set-tapestry (map &optional n root-window-edges)
  "Restore the frame/window/buffer configuration described by MAP,
which should be a list previously returned by a call to
tapestry.

Optional second arg N causes frame reconfiguration to be skipped
and the windows of the current frame will configured according to
the window map of the Nth frame in MAP.

Optional third arg ROOT-WINDOW-EDGES non-nil should be a list
containing the edges of a window in the current frame.  This list
should be in the same form as returned by the `window-edges'
function.  The window configuration from MAP will be restored in
this window.  If no window with these exact edges exists, a
window that lies entirely within the edge coordinates will be
expanded until the edge coordinates match or the window bounded by
ROOT-WINDOW-EDGES is entirely contained within the expanded
window.  If no window entirely within the ROOT-WINDOW-EDGES edge
coordinates can be found, the window with the greatest overlap of
ROOT-WINDOW-EDGES will be used."
  (let ((sf (tapestry-selected-frame))
	(tapestry-windows-changed nil)
	frame-list frame-map other-maps other-map)
    (setq frame-map (nth 0 map)
	  other-maps (nth 1 map))
    (if (and root-window-edges (null n))
	(setq n 1))
    (if n
	(let (first-window)
	  (setq other-map (nth (1- n) other-maps))
	  (if (null other-map)
	      (error "No such map, %d" n))
	  (setq first-window
		(tapestry-set-window-map (nth 0 other-map) root-window-edges))
	  (tapestry-set-buffer-map (nth 1 other-map) first-window)
	  (tapestry-set-position-map (nth 2 other-map) first-window))
      (tapestry-set-frame-map frame-map)
      ;; frame list is reversed relative to the map order because
      ;; created frames are added to the head of the list instead
      ;; of the tail.
      (setq frame-list (nreverse (tapestry-frame-list)))
      (unwind-protect
	  (while other-maps
	    (tapestry-select-frame (car frame-list))
	    (tapestry-set-window-map (nth 0 (car other-maps)))
	    (tapestry-set-buffer-map (nth 1 (car other-maps)))
	    (tapestry-set-position-map (nth 2 (car other-maps)))
	    (setq other-maps (cdr other-maps)
		  frame-list (cdr frame-list)))
	(and (tapestry-frame-live-p sf) (tapestry-select-frame sf))))))

(defun tapestry-frame-map ()
  (let ((map (mapcar 'tapestry-frame-parameters (tapestry-frame-list)))
	list cell frame-list)
    (setq list map
	  frame-list (tapestry-frame-list))
    (while list
      (setq cell (assq 'minibuffer (car list)))
      (if (and cell (windowp (cdr cell)))
	  (if (eq (tapestry-window-frame (cdr cell)) (car frame-list))
	      (setcdr cell t)
	    (setcdr cell 'none)))
      (setq list (cdr list)
	    frame-list (cdr frame-list)))
    map ))

(defun tapestry-set-frame-map (map)
  ;; some parameters can only be set only at frame creation time.
  ;; so all existing frames must die.
  (let ((doomed-frames (tapestry-frame-list)))
    (while map
      (tapestry-make-frame (car map))
      (setq map (cdr map)))
    (while doomed-frames
      (tapestry-delete-frame (car doomed-frames))
      (setq doomed-frames (cdr doomed-frames)))))

(defun tapestry-window-map ()
  (let (maps map0 map1 map0-edges map1-edges x-unchanged y-unchanged)
    (setq maps (mapcar 'tapestry-window-edges (tapestry-window-list)))
    (while (cdr maps)
      (setq map0 maps)
      (while (cdr map0)
	(setq map1 (cdr map0)
	      map0-edges (tapestry-find-window-map-edges (car map0))
	      map1-edges (tapestry-find-window-map-edges (car map1))
	      x-unchanged (and (= (car map0-edges) (car map1-edges))
			       (= (nth 2 map0-edges) (nth 2 map1-edges)))
	      y-unchanged (and (= (nth 1 map0-edges) (nth 1 map1-edges))
			       (= (nth 3 map0-edges) (nth 3 map1-edges))))
	(cond ((and (not x-unchanged) (not y-unchanged))
	       (setq map0 (cdr map0)))
	      ((or (and x-unchanged (eq (car (car map0)) '-))
		   (and y-unchanged (eq (car (car map0)) '|)))
	       (nconc (car map0) (list (car map1)))
	       (setcdr map0 (cdr map1)))
	      (t
	       (setcar map0 (list (if x-unchanged '- '|)
				  (car map0)
				  (car map1)))
	       (setcdr map0 (cdr map1))))))
    (car maps)))

(defun tapestry-set-window-map (map &optional root-window-edges)
  (let ((map-width (tapestry-compute-map-width map))
	(map-height (tapestry-compute-map-height map))
	(root-window nil))
    (if root-window-edges
	(let (w-list w-edges w-area
		     exact-w inside-w overlap-w max-overlap overlap)
	  (while (null root-window)
	    (setq exact-w nil
		  inside-w nil
		  overlap-w nil
		  max-overlap -1
		  w-list (tapestry-window-list))
	    (while w-list
	      (setq w-edges (tapestry-window-edges (car w-list))
		    w-area (tapestry-window-area w-edges))
	      (if (equal w-edges root-window-edges)
		  (setq exact-w (car w-list)
			w-list nil)
		(setq overlap (tapestry-window-overlap w-edges
						       root-window-edges)
		      overlap (if overlap (tapestry-window-area overlap) 0)
		      w-area (tapestry-window-area w-edges))
		(if (< max-overlap overlap)
		    (setq max-overlap overlap
			  overlap-w (car w-list)))
		;; set inside-w each time we find a window inside
		;; the root window edges.  FSF Emacs gives space
		;; to the window above or to the left if there is
		;; such a window.  therefore we want to find the
		;; inside window that is bottom-most or right-most so that
		;; when we delete it, its space will be given to
		;; what will be the root window.
		(if (= w-area overlap)
		    (setq inside-w (car w-list)))
		(setq w-list (cdr w-list))))
	    (cond (exact-w (setq root-window exact-w))
		  (inside-w
		   ;; how could a window be inside the root window
		   ;; edges and there only be one window?  a
		   ;; multi-line minibuffer, that's how!
		   (if (not (one-window-p t))
		       (delete-window inside-w)))
		  (t (setq root-window overlap-w))))
	  (tapestry-apply-window-map map map-width map-height root-window)
	  (setq tapestry-windows-changed t)
	  root-window )
      (if (tapestry-windows-match-map map map-width map-height)
	  (tapestry-first-window)
	(delete-other-windows)
	(setq root-window (selected-window))
	(tapestry-apply-window-map map map-width map-height root-window)
	(setq tapestry-windows-changed t)
	root-window ))))

(defun tapestry-buffer-map ()
  (let ((w-list (tapestry-window-list))
	b list)
    (while w-list
      (setq b (window-buffer (car w-list))
	    list (cons (list (buffer-file-name b)
			     (buffer-name b))
		       list)
	    w-list (cdr w-list)))
    (nreverse list)))

;; This version of tapestry-set-buffer-map unconditionally set
;; the window buffer.  This confused XEmacs 19.14's scroll-up
;; function when scrolling VM presentation buffers.
;; end-of-buffer was never signaled after a scroll.  You can
;; duplicate this by creating a buffer that can be displayed
;; fully in the current window and then run
;;
;;    (progn
;;      (set-window-buffer (selected-window) (current-buffer))
;;      (scroll-up nil))
;;;;;;;;;;;
;;(defun tapestry-set-buffer-map (buffer-map &optional first-window)
;;  (let ((w-list (tapestry-window-list first-window)) wb)
;;    (while (and w-list buffer-map)
;;      (setq wb (car buffer-map))
;;      (set-window-buffer
;;       (car w-list)
;;       (if (car wb)
;;	   (or (get-file-buffer (car wb))
;;	       (find-file-noselect (car wb)))
;;	 (get-buffer-create (nth 1 wb))))
;;      (setq w-list (cdr w-list)
;;	    buffer-map (cdr buffer-map)))))

(defun tapestry-set-buffer-map (buffer-map &optional first-window)
  (let ((w-list (tapestry-window-list first-window))
	current-wb proposed-wb cell)
    (while (and w-list buffer-map)
      (setq cell (car buffer-map)
	    proposed-wb (if (car cell)
			    (or (get-file-buffer (car cell))
				(find-file-noselect (car cell)))
			  (get-buffer-create (nth 1 cell)))
	    current-wb (window-buffer (car w-list)))
      ;; Setting the window buffer to the same value it already
      ;; has seems to confuse XEmacs' scroll-up function.  But
      ;; _not_ setting after windows torn down seem to cause
      ;; window point to sometimes drift away from point at
      ;; redisplay time.  The solution (hopefully!) is to track
      ;; when windows have been rearranged and unconditionally do
      ;; the set-window-buffer, otherwise do it only if the
      ;; window buffer and the prosed window buffer differ.
      (if (or tapestry-windows-changed (not (eq proposed-wb current-wb)))
	  (set-window-buffer (car w-list) proposed-wb))
      (setq w-list (cdr w-list)
	    buffer-map (cdr buffer-map)))))

(defun tapestry-position-map ()
  (let ((sw (selected-window))
	(w-list (tapestry-window-list))
	list)
    (while w-list
      (setq list (cons (list (window-start (car w-list))
			     (window-point (car w-list))
			     (window-hscroll (car w-list))
			     (eq (car w-list) sw))
		       list)
	    w-list (cdr w-list)))
    (nreverse list)))

(defun tapestry-set-position-map (position-map &optional first-window)
  (let ((w-list (tapestry-window-list first-window))
	(osw (selected-window))
	sw p)
    (while (and w-list position-map)
      (setq p (car position-map))
      (and (car p) (set-window-start (car w-list) (car p)))
      (and (nth 1 p) (set-window-point (car w-list) (nth 1 p)))
      (and (nth 2 p) (set-window-hscroll (car w-list) (nth 2 p)))
      (and (nth 3 p) (setq sw (car w-list)))
      ;; move this buffer up in the buffer-list
      (select-window (car w-list))
      (setq w-list (cdr w-list)
	    position-map (cdr position-map)))
    (select-window (or sw osw))))

(defun tapestry-apply-window-map (map map-width map-height current-window
				      &optional
				      root-window-width
				      root-window-height)
  (let ((window-min-height 1)
	(window-min-width 1)
	horizontal)
    (if (null root-window-width)
	(setq root-window-height (window-height current-window)
	      root-window-width (window-width current-window)))
    (while map
      (cond
       ((numberp (car map)) (setq map nil))
       ((eq (car map) '-) (setq horizontal nil))
       ((eq (car map) '|) (setq horizontal t))
       (t
	(if (cdr map)
	    (split-window
	     current-window
	     (if horizontal
		 (/ (* (tapestry-compute-map-width (car map))
		       root-window-width)
		    map-width)
	       (/ (* (tapestry-compute-map-height (car map))
		     root-window-height)
		  map-height))
	     horizontal))
	(if (not (numberp (car (car map))))
	    (setq current-window
		  (tapestry-apply-window-map (car map)
					     map-width map-height
					     current-window
					     root-window-width
					     root-window-height)))
	(and (cdr map) (setq current-window (next-window current-window 0)))))
      (setq map (cdr map)))
    current-window ))

(defun tapestry-windows-match-map (map
				   &optional
				   map-width map-height
				   window-map
				   window-map-width
				   window-map-height)
  (or map-width
      (setq map-width (tapestry-compute-map-width map)
	    map-height (tapestry-compute-map-height map)))
  (or window-map
      (setq window-map (tapestry-window-map)
	    window-map-height (tapestry-compute-map-height window-map)
	    window-map-width (tapestry-compute-map-width window-map)))
  (let ((result t))
    (cond ((numberp (car map))
	   (and (numberp (car window-map))
		(= (/ (* (nth 0 map) window-map-width)
		      map-width)
		   (nth 0 window-map))
		(= (/ (* (nth 1 map) window-map-height)
		      map-height)
		   (nth 1 window-map))
		(= (/ (* (nth 2 map) window-map-width)
		      map-width)
		   (nth 2 window-map))
		(= (/ (* (nth 3 map) window-map-height)
		      map-height)
		   (nth 3 window-map))))
	  ((eq (car map) '-)
	   (if (not (eq (car window-map) '-))
	       nil
	     (setq map (cdr map)
		   window-map (cdr window-map))
	     (while (and result map window-map)
	       (setq result (tapestry-windows-match-map (car map)
							map-width
							map-height
							(car window-map)
							window-map-width
							window-map-height)
		     map (cdr map)
		     window-map (cdr window-map)))
	     (and result (null map) (null window-map))))
	  ((eq (car map) '|)
	   (if (not (eq (car window-map) '|))
	       nil
	     (setq map (cdr map)
		   window-map (cdr window-map))
	     (while (and result map window-map)
	       (setq result (tapestry-windows-match-map (car map)
							map-width
							map-height
							(car window-map)
							window-map-width
							window-map-height)
		     map (cdr map)
		     window-map (cdr window-map)))
	     (and result (null map) (null window-map)))))))

(defun tapestry-find-window-map-edges (map)
  (let (nw-edges se-edges)
    (setq nw-edges map)
    (while (and (consp nw-edges) (not (numberp (car nw-edges))))
      (setq nw-edges (car (cdr nw-edges))))
    (setq se-edges map)
    (while (and (consp se-edges) (not (numberp (car se-edges))))
      (while (cdr se-edges)
	(setq se-edges (cdr se-edges)))
      (setq se-edges (car se-edges)))
    (if (eq nw-edges se-edges)
	nw-edges
      (setq nw-edges (copy-sequence nw-edges))
      (setcdr (nthcdr 1 nw-edges) (nthcdr 2 se-edges))
      nw-edges )))

(defun tapestry-compute-map-width (map)
  (let ((edges (tapestry-find-window-map-edges map)))
    (- (nth 2 edges) (car edges))))

(defun tapestry-compute-map-height (map)
  (let ((edges (tapestry-find-window-map-edges map)))
    (- (nth 3 edges) (nth 1 edges))))

;; delq is to memq as delassq is to assq
(defun tapestry-delassq (elt list)
  (let ((prev nil)
	(curr list))
    (while curr
      (if (eq elt (car (car curr)))
	  (if (null prev)
	      (setq list (cdr list) curr list)
	    (setcdr prev (cdr curr))
	    (setq curr (cdr curr)))
	(setq prev curr curr (cdr curr))))
    list ))

(defun tapestry-remove-frame-parameters (map params)
  (let (frame-map)
    (while params
      (setq frame-map (nth 0 map))
      (while frame-map
	(setcar frame-map (tapestry-delassq (car params) (car frame-map)))
	(setq frame-map (cdr frame-map)))
      (setq params (cdr params)))))

(defun tapestry-nullify-tapestry-elements (map &optional buf-file-name buf-name
					       window-start window-point
					       window-hscroll selected-window)
  (let (p)
    (setq map (nth 1 map))
    (while map
      (setq p (nth 1 (car map)))
      (while p
	(and buf-file-name (setcar (car p) nil))
	(and buf-name (setcar (cdr (car p)) nil))
	(setq p (cdr p)))
      (setq p (nth 2 (car map)))
      (while p
	(and window-start (setcar (car p) nil))
	(and window-point (setcar (cdr (car p)) nil))
	(and window-hscroll (setcar (nthcdr 2 (car p)) nil))
	(and selected-window (setcar (nthcdr 3 (car p)) nil))
	(setq p (cdr p)))
      (setq map (cdr map)))))

(defun tapestry-replace-tapestry-element (map what function)
  (let (mapi mapj p old new)
    (cond ((eq what 'buffer-file-name)
	   (setq mapi 1 mapj 0))
	  ((eq what 'buffer-name)
	   (setq mapi 1 mapj 1))
	  ((eq what 'window-start)
	   (setq mapi 2 mapj 0))
	  ((eq what 'window-point)
	   (setq mapi 2 mapj 1))
	  ((eq what 'window-hscroll)
	   (setq mapi 2 mapj 2))
	  ((eq what 'selected-window)
	   (setq mapi 2 mapj 3)))
    (setq map (nth 1 map))
    (while map
      (setq p (nth mapi (car map)))
      (while p
	(setq old (nth mapj (car p))
	      new (funcall function old))
	(if (not (equal old new))
	    (setcar (nthcdr mapj (car p)) new))
	(setq p (cdr p)))
      (setq map (cdr map)))))

(defun tapestry-window-list (&optional first-window)
  (let* ((first-window (or first-window (tapestry-first-window)))
	 (windows (cons first-window nil))
	 (current-cons windows)
	 (w (next-window first-window 'nomini)))
    (while (not (eq w first-window))
      (setq current-cons (setcdr current-cons (cons w nil)))
      (setq w (next-window w 'nomini)))
    windows ))

(defun tapestry-first-window ()
  (if (eq (tapestry-selected-frame)
	  (tapestry-window-frame (minibuffer-window)))
      (next-window (minibuffer-window))
    (let ((w (selected-window))
	  (top (or (cdr (assq 'menu-bar-lines (tapestry-frame-parameters))) 0))
	  edges)
      (while (or (not (= 0 (car (setq edges (tapestry-window-edges w)))))
		 ;; >= instead of = because in FSF Emacs 19.2x
		 ;; (whenever the Lucid menubar code was added) the
		 ;; menu-bar-lines frame parameter == 1 when the
		 ;; Lucid menubar is enabled even though the
		 ;; menubar doesn't steal the first line from the
		 ;; window.
		 (not (>= top (nth 1 edges))))
	(setq w (next-window w 'nomini)))
      w )))

(defun tapestry-window-area (edges)
  (* (- (nth 3 edges) (nth 1 edges))
     (- (nth 2 edges) (nth 0 edges))))

(defun tapestry-window-overlap (e0 e1)
  (let (top left bottom right)
    (cond ((and (<= (nth 0 e0) (nth 0 e1)) (< (nth 0 e1) (nth 2 e0)))
	   (setq left (nth 0 e1)))
	  ((and (<= (nth 0 e1) (nth 0 e0)) (< (nth 0 e0) (nth 2 e1)))
	   (setq left (nth 0 e0))))
    (cond ((and (< (nth 0 e0) (nth 2 e1)) (<= (nth 2 e1) (nth 2 e0)))
	   (setq right (nth 2 e1)))
	  ((and (< (nth 0 e1) (nth 2 e0)) (<= (nth 2 e0) (nth 2 e1)))
	   (setq right (nth 2 e0))))
    (cond ((and (<= (nth 1 e0) (nth 1 e1)) (< (nth 1 e1) (nth 3 e0)))
	   (setq top (nth 1 e1)))
	  ((and (<= (nth 1 e1) (nth 1 e0)) (< (nth 1 e0) (nth 3 e1)))
	   (setq top (nth 1 e0))))
    (cond ((and (< (nth 1 e0) (nth 3 e1)) (<= (nth 3 e1) (nth 3 e0)))
	   (setq bottom (nth 3 e1)))
	  ((and (< (nth 1 e1) (nth 3 e0)) (<= (nth 3 e0) (nth 3 e1)))
	   (setq bottom (nth 3 e0))))
    (and left top right bottom (list left top right bottom))))

(defun tapestry-window-edges (&optional window)
  (if (fboundp 'window-pixel-edges)
      (let ((edges (window-pixel-edges window))
	    tmp)
	(setq tmp edges)
	(setcar tmp (/ (car tmp) (face-width 'default)))
	(setq tmp (cdr tmp))
	(setcar tmp (/ (car tmp) (face-height 'default)))
	(setq tmp (cdr tmp))
	(setcar tmp (/ (car tmp) (face-width 'default)))
	(setq tmp (cdr tmp))
	(setcar tmp (/ (car tmp) (face-height 'default)))
	edges )
    (window-edges window)))

;; We call these functions instead of calling the Emacs 19 frame
;; functions directly to let this package work with v18 Emacs.

(defun tapestry-frame-list ()
  (if (fboundp 'frame-list)
      (frame-list)
    (list nil)))

(defun tapestry-frame-parameters (&optional f)
  (if (fboundp 'frame-parameters)
      (frame-parameters f)
    nil ))

(defun tapestry-window-frame (w)
  (if (fboundp 'window-frame)
      (window-frame w)
    nil ))

(defun tapestry-modify-frame-parameters (f alist)
  (if (fboundp 'modify-frame-parameters)
      (modify-frame-parameters f alist)
    nil ))

(defun tapestry-select-frame (f)
  (if (fboundp 'select-frame)
      (select-frame f)
    nil ))

(defun tapestry-selected-frame ()
  (if (fboundp 'selected-frame)
      (selected-frame)
    nil ))

(defun tapestry-next-frame (&optional f all)
  (if (fboundp 'next-frame)
      (next-frame f all)
    nil ))

(defun tapestry-make-frame (&optional alist)
  (if (fboundp 'make-frame)
      (make-frame alist)
    nil ))

(defun tapestry-delete-frame (&optional f)
  (if (fboundp 'delete-frame)
      (delete-frame f)
    nil ))

(defun tapestry-frame-live-p (f)
  (if (fboundp 'frame-live-p)
      (frame-live-p f)
    t ))

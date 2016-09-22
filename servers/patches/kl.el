;;Author: Qianli Liao  -*- lexical-binding: t; -*-
;;March 16 2012
;;Version: 0.1
;;Content: 
;;1.basic functionality of emacspeak-queue
;;2.emacspeak commint output filter
;;3.speak message from emacs-jabber  
;;the message from jabber will be queued and after you switch to that buffer
;;, execute emacspeak-queue-pause-or-resume (bind that to a key), and the queued message will be spoken one by one.
;;you can also pause between messages using the above function. 
;;This function can be used in any context where you use emacspeak queue feature 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  for users ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom emacspeak-queue-interval-between-chunk 0.15
  "the rough time interval between queued chunks, if set lower
 than 0.1, the chunk may not be spoken completely. A time equal
 to or bigger than 0.15 second is preferred. " )
(setq emacspeak-queue-interval-between-chunk 0.15 )
(defvar emacspeak-queue-comint-output-p nil 
"a flag indicate whether the comint/shell output will be chunked and pushed into the queue. 
t means we apply the emacspeak-queue feature in the comint/shell output.
nil means we use the default comint-output-filter that does not have emacspeak-queue feature. ")

(defun emacspeak-queue-comint-output-toggle ()
"toggle between emacspeak-queue comint/shell output and non-queue output "
  (interactive) 
  (if emacspeak-queue-comint-output-p
      (emacspeak-queue-comint-output-off)
    (emacspeak-queue-comint-output-on)
    )
  )

;;pause or resume
(defun emacspeak-queue-pause-or-resume ()
  "one function for toggle between pause or resume"
  (interactive) 
  (if emacspeak-processing-queue-p
      (emacspeak-queue-pause)
    (emacspeak-queue-speak-queue)
    )
  )


;;stop or pause 
(defun emacspeak-queue-pause ()
  "stop or pause the speech of the queue in this buffer"
  (interactive) 
  (emacspeak-queue-stop-processing-queue)
  )

;;speak
(defun emacspeak-queue-speak-queue ()
  "start or resume the speech of the queue in this buffer, with
interval indicated by variable emacspeak-queue-interval-between-chunk"
  (interactive) 
  (emacspeak-queue-speak-entire-queue emacspeak-queue-interval-between-chunk)
  )

;;stop and clear
(defsubst emacspeak-queue-clear-queue ()
  "stop and clear the emacspeak speech queue in this buffer"
  (interactive) 
  (clear-queue emacspeak-queue-queue)
  )










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;for developers;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;queue data structure 
(defstruct q
  (key #'identity)
  (last nil)
  (elements nil))
;;;; Basic Operations on Queues
(defun make-empty-queue () (make-q))

(defun empty-queue? (q)
  "is this a empty queue?"
  (= (length (q-elements q)) 0))

(defun queue-front (q)
  "Return the element at the front of the queue."
  (elt (q-elements q) 0))

(defun remove-front (q)
  "Remove the element from the front of the queue and return it."
  (if (listp (q-elements q))
      (pop (q-elements q))
    (heap-extract-min (q-elements q) (q-key q))))

(defun clear-queue (q)
  (while (remove-front q))
  )

(defun enqueue-at-front (q items)
  "Add a list of items to the front of the queue."
  (setf (q-elements q) (nconc items (q-elements q))))

(defun enqueue-at-end (q items)
  "Add a list of items to the end of the queue."
  ;; To make this more efficient, keep a pointer to the last cons in the queue
  (cond ((null items) nil)
	((or (null (q-last q)) (null (q-elements q)))
	 (setf (q-last q) (last items)
	       (q-elements q) (nconc (q-elements q) items)))
	(t (setf (cdr (q-last q)) items
		 (q-last q) (last items)))))






;;;;;;;;;;;;;;;;;;;;variables 
;;queue
(defvar emacspeak-queue-queue (make-empty-queue))
(make-variable-buffer-local 'emacspeak-queue-queue)

;;some flags
(defvar emacspeak-processing-queue-buffer nil
  "keep track of the last buffer which used emacspeak-queue
utility, in order to set the emacspeak-processing-queue-p flag
back to nil even when you switch to another buffer")
(defvar emacspeak-processing-queue-p nil
  "a flag to indicate whether we are speaking the queue in this
buffer or not, and thus determine whether we need to pause or
resume speech (it is used by emacspeak-queue-pause-or-resume
function ")
(make-variable-buffer-local 'emacspeak-processing-queue-p)
(defvar emacspeak-queue-buffer-local-counter t
  "to make emacspeak-queue-queue buffer-local,
 emacspeak-queue-buffer-local-counter is used to determine
 whether the emacspeak-queue-queue has been made buffer local, if
 yes, then this value should be nil, otherwise it should be t")
(make-variable-buffer-local 'emacspeak-queue-buffer-local-counter)

;;constant for speaking-p
;;qianli: machine dependant??
(defvar emacspeak-alsa-frame-number "(2097152)")
(defvar emacspeak-alsa-frame-number-when-idle-3 "(-32)" )
(defvar emacspeak-alsa-frame-number-when-idle-1 "(2097151)" )
(defvar emacspeak-alsa-frame-number-when-idle-2 "(2097152)" )



;;;;;;;;;;;;;;;;;;;;;;;;;functions 
;;;main functions
(defsubst emacspeak-queue-enqueue-region (start end &optional enqueue-buffer )
  "enqueue version of emacspeak-speak-region : almost the same as emacspeak-speak-region except dtk-speak is changed to emacspeak-queue-enqueue "
  (interactive "r" )
  (declare (special emacspeak-speak-voice-annotated-paragraphs
                    inhibit-point-motion-hooks
                    voice-lock-mode))
  (let ((inhibit-point-motion-hooks t))
    (when (and voice-lock-mode
               (not emacspeak-speak-voice-annotated-paragraphs))
      (save-restriction
        (narrow-to-region start end )
        (emacspeak-speak-voice-annotate-paragraphs)))
    (emacspeak-handle-action-at-point)
    (if enqueue-buffer
	(emacspeak-queue-enqueue (buffer-substring-no-properties start end ) enqueue-buffer )
      (emacspeak-queue-enqueue (buffer-substring-no-properties start end )	)
      )
    ))

(defsubst emacspeak-queue-enqueue-and-speak-region (start end  &optional interval enqueue-buffer)
  "same as the emacspeak-queue-enqueue-region, except i add a emacspeak-queue-speak-entire-queue at the end of this function , let it to speak imediately , INTERVAL is the time between speech ofelements of queue."
  (interactive "r" )
  (declare (special emacspeak-speak-voice-annotated-paragraphs
                    inhibit-point-motion-hooks
                    voice-lock-mode))
  (if (not interval)
      (setq interval emacspeak-queue-interval-between-chunk)
    )
  (let ((inhibit-point-motion-hooks t))
    (when (and voice-lock-mode
               (not emacspeak-speak-voice-annotated-paragraphs))
      (save-restriction
	(narrow-to-region start end )
	(emacspeak-speak-voice-annotate-paragraphs)))
    (emacspeak-handle-action-at-point)
    (emacspeak-queue-chunk-and-enqueue (buffer-substring-no-properties start end) enqueue-buffer)
    ;;qianli: for test purpose
    ;; (emacspeak-queue-enqueue-test (buffer-substring-no-properties start end ) )

    (if (not emacspeak-processing-queue-p)
	(emacspeak-queue-speak-entire-queue interval)
      )
    ))

(defsubst emacspeak-queue-enqueue-and-speak-string (string &optional  interval enqueue-buffer)
  "same as the emacspeak-queue-enqueue-and-speak-region. except we use string instead of region"
  (interactive "r" )  
  (declare (special emacspeak-speak-voice-annotated-paragraphs
		    inhibit-point-motion-hooks
		    voice-lock-mode))
  (if (not interval)  (setq interval emacspeak-queue-interval-between-chunk))
  (let ((inhibit-point-motion-hooks t))
    (emacspeak-queue-chunk-and-enqueue string enqueue-buffer)
    
    (if (not emacspeak-processing-queue-p)
	(emacspeak-queue-speak-entire-queue interval)
      )
    )
  )

(defsubst emacspeak-queue-enqueue-and-speak-region-no-chunk (start end &optional interval)
  "Qianli: it is a obsolete function. It is a  no chunk version of emacspeak-queue-enqueue-and-speak-region, but as it turns out, we cannot finish speaking a big output without chunking it (if we have a emacspeak-queue-interval-between-chunk smaller enough). It confused me.  " 
  (interactive "r" )
  (declare (special emacspeak-speak-voice-annotated-paragraphs
		    inhibit-point-motion-hooks
		    voice-lock-mode))
  (if (not interval)  (setq interval emacspeak-queue-interval-between-chunk))
  (let ((inhibit-point-motion-hooks t))
    (when (and voice-lock-mode
	       (not emacspeak-speak-voice-annotated-paragraphs))
      (save-restriction
	(narrow-to-region start end )
	(emacspeak-speak-voice-annotate-paragraphs)))
    (emacspeak-handle-action-at-point)
    
    (emacspeak-queue-enqueue (buffer-substring-no-properties start end ) )
    ;;for test
    ;;    (emacspeak-queue-enqueue-test (buffer-substring-no-properties start end ))
    (if (not emacspeak-processing-queue-p)
	(emacspeak-queue-speak-entire-queue interval)
      )))

(defun emacspeak-queue-enqueue (text &optional enqueue-buffer)
  ;;the newline in the text will be deleted  since it seems that will affect speech
  ;;for shell filter , it need to indicate the buffer is shell ,since the emacspeak-queue-queue is buffer local
  (save-current-buffer
    (if enqueue-buffer 
	(set-buffer enqueue-buffer))
    ;;to make emacspeak-queue-queue buffer-local this
    ;;emacspeak-queue-buffer-local-counter is used to determine whether
    ;;the emacspeak-queue-queue has been made buffer local, if yes,
    ;;then this value should be nil, otherwise it should be t
    (if emacspeak-queue-buffer-local-counter
	;; you must set it to (make-empty-queue) ,since if you set it to emacspeak-queue-queue , then it is linked with the global value
	(progn
	  (setq emacspeak-queue-buffer-local-counter nil)
	  (setq emacspeak-queue-queue (make-empty-queue))	
	  )  )
    (let ((text-after (delete-newline-in-string text)))
      (if (and text-after
	       (not (equal text-after "") )
	       )
	  (progn
	    (enqueue-at-end emacspeak-queue-queue (list text-after) )
	    )
	nil
	)
      )
    ;;(setq emacspeak-queue-queue (make-empty-queue))
    ))




;;timer process to call repeatedly to check if the speaker has finished speaking,
(defun emacspeak-queue-idle-speak-queue ()
  "speak the elements in the queue when the alsa pcm player is not speaking, it is done by running this function with timer. if the queue is empty , it cancels its function timer "
  (interactive) 
  (if  (emacspeak-queue-empty-p)	 
      (progn
	(cancel-function-timers 'emacspeak-queue-idle-speak-queue)
	(emacspeak-queue-set-processing-to-nil)
	)
    (if (not (emacspeak-speaking-p))
	(progn
	  (emacspeak-queue-dequeue-and-speak )
	  ;;(setq test-queue-cut nil)
	  ;;(setq test-queue-cut-in (+ test-queue-cut-in 1))
	  
	  ))))


(defun emacspeak-queue-dequeue-and-speak ()
  "pop the front of the queue and speak it"

  (let ( (speech-text (remove-front emacspeak-queue-queue)))
    (dtk-stop-and-speak speech-text) ) )

(defun dtk-stop-and-speak  (text &optional ignore-skim)
  "stop immediately version of dtk-speak"

  (dtk-stop)
  (dtk-speak text ignore-skim)
  )

(defun emacspeak-queue-chunk-and-enqueue (text &optional enqueue-buffer)
  "basically modified from dtk-speak, but this function does not speak , it only chuncks input to pieces and enqueue them"
  (declare (special dtk-speaker-process dtk-stop-immediately
		    tts-strip-octals
		    inhibit-point-motion-hooks
		    dtk-speak-server-initialized emacspeak-use-auditory-icons
		    dtk-speech-rate
		    dtk-speak-nonprinting-chars
		    dtk-speak-treat-embedded-punctuations-specially
		    dtk-quiet  dtk-chunk-separator-syntax
		    voice-lock-mode   dtk-punctuation-mode
		    dtk-split-caps
		    emacspeak-pronounce-pronunciation-table
		    selective-display ))
  
  (let ((inhibit-point-motion-hooks t)
	(invisibility-spec buffer-invisibility-spec)
	(syntax-table (syntax-table ))
	(inherit-speaker-process dtk-speaker-process)
	(pronunciation-table emacspeak-pronounce-pronunciation-table)
	(use-auditory-icons emacspeak-use-auditory-icons)
	(inherit-chunk-separator-syntax dtk-chunk-separator-syntax )
	(inherit-speak-nonprinting-chars
	 dtk-speak-nonprinting-chars)
	(inherit-strip-octals tts-strip-octals)
	(complement-separator(dtk-complement-chunk-separator-syntax ))
	(speech-rate dtk-speech-rate)
	(inherit-enable-multibyte-characters enable-multibyte-characters)
	(dtk-scratch-buffer (get-buffer-create " *dtk-scratch-buffer* "))
	(start 1)
	(end nil )
	(mode dtk-punctuation-mode)
	(split-caps dtk-split-caps)
	(voice-lock voice-lock-mode ))
    (save-current-buffer
      ;;      dtk-scratch-buffer does not work sometimes
      ;;      (get-buffer-create  "*dtk-queue-scratch*" ) 
      ;;      (set-buffer  "*dtk-queue-scratch*" ) 
      (set-buffer  dtk-scratch-buffer ) 
      (setq buffer-undo-list t)
      (let ((inhibit-read-only t))
	(erase-buffer)
                                        ; inherit environment
	(setq buffer-invisibility-spec invisibility-spec
	      dtk-chunk-separator-syntax inherit-chunk-separator-syntax
	      dtk-speaker-process inherit-speaker-process
	      dtk-speech-rate speech-rate
	      emacspeak-use-auditory-icons use-auditory-icons
	      dtk-punctuation-mode mode
	      dtk-split-caps split-caps
	      dtk-speak-nonprinting-chars
	      inherit-speak-nonprinting-chars
	      tts-strip-octals inherit-strip-octals
	      voice-lock-mode voice-lock)
	(set-syntax-table syntax-table )
	(set-buffer-multibyte inherit-enable-multibyte-characters)
	(insert  text)
	(delete-invisible-text)
	(when pronunciation-table
	  (tts-apply-pronunciations
	   pronunciation-table))
	(dtk-unicode-replace-chars mode)
	(dtk-handle-repeating-patterns mode)
	(dtk-quote mode))
      (goto-char (point-min))
      (skip-syntax-forward inherit-chunk-separator-syntax)
      (while (and (not (eobp))
		  ;;qianli add
		  ;;(set-buffer  "*dtk-queue-scratch*" ) 
		  (dtk-move-across-a-chunk
		   inherit-chunk-separator-syntax
		   complement-separator))
                                        ;if we matched a punctuation,
                                        ;treat this as a chunk only if the punctuation is followed
                                        ;by white space
                                        ;dtk-speak-treat-embedded-punctuations-specially
                                        ;has been T for a long time
	(unless
	    (and (char-after  (point))
		 (= (char-syntax (preceding-char )) ?.)
		 (not (= 32 (char-syntax (following-char )))))
	  (setq end (point ))
	  
	  (emacspeak-queue-enqueue-region  start end enqueue-buffer )
	  (setq start  end)))         ; end while
                                        ; process trailing text
      (or  (= start (point-max))
	   (emacspeak-queue-enqueue-region start (point-max) enqueue-buffer  )  )))

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun emacspeak-speaking-p ()
  "since dtk-interp-speaking-p only send request to the asynchronous speaker process, if we do not use sleep-for or the time specified is too short , the new-frame-number-1 will be the emacspeak-alsa-frame-number responded by the last call of this function. Note that using sleep-for or sit-for in timer functions may cause unpredictable bugs, according to GNU Emacs Manual 39.10 Timers for Delayed Execution. " 
  (let ((new-frame-number-1)
	)
    (dtk-interp-speaking-p)
    ;;wait for process comunication finished, the waiting time is flexible , based on the processingg speed 
    (sleep-for 0.01)
    (setq new-frame-number-1 emacspeak-alsa-frame-number)
    
    (if (or (equal new-frame-number-1 emacspeak-alsa-frame-number-when-idle-1)
	    (equal new-frame-number-1 emacspeak-alsa-frame-number-when-idle-2)
	    (equal new-frame-number-1 emacspeak-alsa-frame-number-when-idle-3)
	    )
	nil
      t)
    ))



;;;;;;;;;;;;;;;;output filter for the speaker process 
(defun emacspeak-outloud-output-filter (q s)
  "Act on output of emacspeak-outloud interface process P that has sent S."
  ;; based on vr-mode.el
  (unless (stringp s)
    (message "Non-string %S given to emacspeak-outloud-output-filter" s))

  (setq emacspeak-outloud-reading-string s)
  (cond
   ((and (string-match "Bad return code" emacspeak-outloud-reading-string)
	 (string-match "([0-9]+)" emacspeak-outloud-reading-string 1)
	 )
    (setq emacspeak-alsa-frame-number (substring-no-properties emacspeak-outloud-reading-string (match-beginning 0) (match-end 0) ) )
    )
   (t )
   )
  )
;;;;;;;set filter 
(set-process-filter dtk-speaker-process 'emacspeak-outloud-output-filter)



;;;;;;;;;;;;; set the flag to indicate whether the queue is being processed in the current buffer
(defun emacspeak-queue-set-processing-to-t()
  "set emacspeak-processing-queue-p  to t, and record the current buffer"
  (setq emacspeak-processing-queue-p t)
  (setq emacspeak-processing-queue-buffer (current-buffer))
  )
(defun emacspeak-queue-set-processing-to-nil()
  "set emacspeak-processing-queue-p  to nil in the corresponding buffer.
 What's more, it will set the emacspeak-processing-queue-p to nil
 in the current buffer,since it does not hurt to set it to nil in
 the current buffer also to prevent some unpredictable bugs (
 because if it is t, the emacspeak-queue-enqueue-and-speak
 functions will not trigger the speech automatically."
  (with-current-buffer emacspeak-processing-queue-buffer
    (setq emacspeak-processing-queue-p nil)
    )
  )
;;run the timer 
(defun emacspeak-queue-speak-entire-queue (num)
  "set a timer to run emacspeak-queue-idle-speak-queue, after speaking everything in the queue , emacspeak-queue-idle-speak-queue will cancel the timer by itself. "
  (interactive)
  (cancel-function-timers 'emacspeak-queue-idle-speak-queue)
  (run-with-timer num num 'emacspeak-queue-idle-speak-queue)
  (emacspeak-queue-set-processing-to-t)
  )
;;stop processing queue
(defun emacspeak-queue-stop-processing-queue ()
  "cancel processing the queue, that is, we cancel the timer emacspeak-queue-idle-speak-queue. But we will keep the remaining content in the queue, you can let emacspeak speak them again by calling emacspeak-queue-speak-entire-queue."
  (interactive)
  (cancel-function-timers 'emacspeak-queue-idle-speak-queue)
  (emacspeak-queue-set-processing-to-nil)
  )


;;;small and miscellaneous functions

(defun emacspeak-queue-empty-p ()
  (empty-queue? emacspeak-queue-queue)
  )

(defun delete-newline-in-string(pre-str)
  "a functions to delete the newlines in a string"
  (let ((str nil) (cons (split-string pre-str "\n" t)) )
    
    (while (cdr cons) 
      (setq str (concat str (car cons) " "))
      (setq cons (cdr cons))
      )
    (setq str (concat str (car cons)))
    (if (equal str "")
	nil
      str)))


;;;;;;;;;;;;;something need to be changed or added in emacspeak 

;;send a message to speak process to ask if the speaker is speaking 
(defsubst dtk-interp-speaking-p ()
  (declare (special dtk-speaker-process))
  (process-send-string dtk-speaker-process
		       "pa\n"))

;;add a filter to emacspeak outloud process
(defun tts-restart ()
  "Use this to nuke the currently running TTS server and restart it."
  (interactive)
  (dtk-initialize )
  (set-process-filter dtk-speaker-process 'emacspeak-outloud-output-filter)
  )


;;emacspeak comint filter 
(defun emacspeak-queue-comint-output-off ()

  (eval-expression
   `(defadvice comint-output-filter (around emacspeak pre act)
     "Make comint speak its output."
     (let ((inhibit-read-only t)
	   (monitor emacspeak-comint-output-monitor)
	   (buffer (process-buffer (ad-get-arg 0)))
	   (dtk-stop-immediately nil))
       (set-buffer buffer)
       ad-do-it
       (when (and (boundp 'comint-last-prompt-overlay)
		  comint-last-prompt-overlay)
	 (add-text-properties
	  (overlay-start comint-last-prompt-overlay)
	  (overlay-end comint-last-prompt-overlay)
	  (list
	   'personality
	   'emacspeak-comint-prompt-personality
	   'rear-sticky nil)))
       (when (and
	      comint-last-output-start
	      (or emacspeak-comint-autospeak emacspeak-speak-comint-output)
	      (or monitor (eq (window-buffer) buffer)))
	 (emacspeak-speak-region comint-last-output-start (point )))
       ad-return-value)

     );;end of define advice
   );;end of eval
  (setq  emacspeak-queue-comint-output-p nil)
  (message "emacspeak-queue-comint-output turned off")
  )

(defun emacspeak-queue-comint-output-on ()



  (eval-expression 
   ;;emacspeak queue version of comint output filter 
   `(defadvice comint-output-filter (around emacspeak pre act)
     "Make comint speak its emacspeak-queue version"
     (let ((inhibit-read-only t)
	   (monitor emacspeak-comint-output-monitor)
	   (buffer (process-buffer (ad-get-arg 0)))
	   ;;qianli revised 
	   (dtk-stop-immediately t))
       (set-buffer buffer)
       ad-do-it
       (when (and (boundp 'comint-last-prompt-overlay)
		  comint-last-prompt-overlay)
	 (add-text-properties
	  (overlay-start comint-last-prompt-overlay)
	  (overlay-end comint-last-prompt-overlay)
	  (list
	   'personality
	   'emacspeak-comint-prompt-personality
	   'rear-sticky nil)))
       (when (and
	      comint-last-output-start
	      (or emacspeak-comint-autospeak emacspeak-speak-comint-output)
	      (or monitor (eq (window-buffer) buffer)))
	 (save-excursion
	   ;;qianli  revised    
	   (setq orig-point (point))
	   (goto-char (point-max))
	   (beginning-of-line 0)
	   (setq end-of-output  (point) )
	   (goto-char  orig-point )
	   (setq last-output-start  comint-last-output-start)
	   
	   (emacspeak-queue-enqueue-and-speak-string (buffer-substring-no-properties last-output-start end-of-output) 0.15 "*shell*")
	   )
	 ad-return-value)
       )
     );;end of advice
   );;end of eval

  (setq emacspeak-queue-comint-output-p t)
  (message "emacspeak-queue-comint-output turned on")
  )

(emacspeak-queue-comint-output-on)








;;emacspeak queue jabber 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice jabber-process-chat  (after emacspeak-jabber-process-chat)

  (let ((from (jabber-xml-get-attribute xml-data 'from))
	(error-p (jabber-xml-get-children xml-data 'error))
	(body-text (car (jabber-xml-node-children
			 (car (jabber-xml-get-children
			       xml-data 'body)))))
	)
    (setq jabber-buffer (jabber-chat-get-buffer
			 (jabber-xml-get-attribute xml-data 'from) ))
    (if (and body-text
	     (not (equal body-text ""))
	     )
	(progn
	  
	  (emacspeak-queue-enqueue-and-speak-string body-text emacspeak-queue-interval-between-chunk jabber-buffer )
	  )
      )
)   )

(ad-activate 'jabber-process-chat)

×‡Z{ÝøÑ­·s½vs}8mÍ|Ó—›

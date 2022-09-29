;;; NNTP.el: NNTP commands for Gnews
;;; Copyright (C) 1987, 1988 by Matthew P Wiener; all rights reserved.

;;; Send bugs, complaints, suggestions etc to weemba@garnet.berkeley.edu

;; Gnews is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY.  No author or distributor accepts responsibility to
;; anyone for the consequences of using it or for whether it serves any
;; particular purpose or works at all, unless he says so in writing.
;; Refer to the Gnews General License for full details.

;; Everyone is granted permission to copy, modify and redistribute Gnews,
;; but only under the conditions described in the Gnews General License.
;; A copy of this license is supposed to have been given to you along with
;; Gnews so you can know your rights and responsibilities.  It should be
;; accessible with the key sequence "ESC l l" while in News mode.  Among 
;; other things, the copyright notice and this notice must be preserved on
;; all copies.

;;; Commands for talking to the NNTP process.

(defvar nntp nil "Process that runs NNTP.")
(defvar nntp-index nil "Process that runs the NNTP index.")
(defvar nntp-process "" "Name of the NNTP process.")
(defvar nntp-buffer nil "Buffer for the NNTP process.")

(defvar nntp-eot nil "Non-nil if NNTP has finished responding.")
(defvar nntp-info nil "The last NNTP informative message.")
(defvar nntp-connect-message "" "Message received on initial NNTP connection.")
(defvar nntp-version nil "NNTP version number")

(defvar nntp-exec-verbose nil
  "*Non-nil means echo NNTP commands and messages.")
(defvar nntp-exec-log-p nil
  "*Non-nil means log NNTP commands and messages inside nntp-exec-log.")
(defvar nntp-exec-log nil
  "Log of past NNTP commands and messages.  It is only recorded if
nntp-exec-log-p is non-nil.")
(defvar nntp-dot nil
  "Non-nil if expecting NNTP input to be \".\" terminated.")
(defconst nntp-dot-commands
  '(("head") ("body") ("article") ("help") ("newgroups") ("list") ("newnews"))
  "List of NNTP commands which are \".\" terminated.")

(defconst nntp-bogus-group "411 Invalid group name."
  "NNTP bogus newsgroup message.")

(defconst nntp-no-connect
  (format "^502 %s NNTP server can't talk to you.  Goodbye.\r?\n\n%s"
	  gnews-spool-machine "Process nntp finished")
  "Non-connection message in nntp-buffer")
(defconst nntp-xfer-only
  "502 You only have permission to transfer, sorry.\r?"
  "Transfer-only connection message.")

(defconst nntp-server-version (format "NNTP server version")
  "Where the NNTP version can be found.")

(defun nntp-start (msg)
  "Start the Gnews NNTP stream.\n
Non-nil argument MSG--a string--means inform the user of the connection.\n
MSG is prepended to the \"connecting to <your-nntp-server>\" message."
  (setq nntp (condition-case q
		 (prog2
		     (if msg (message "%sconnecting to %s..." msg
				      gnews-spool-machine))
		     (open-network-stream nntp-process
					  nntp-buffer
					  gnews-spool-machine
					  nntp-service)
		   (if msg (message "%sconnecting to %s...done" msg
				    gnews-spool-machine)))
	       (error (setq mode-line-format default-mode-line-format) 
		      (switch-to-buffer gnews-buffer-return)
		      (signal (car q) (cdr q))))
	nntp-exec-comm ""
	nntp-exec-args ""
	nntp-exec-force nil
	nntp-exec-value t)
  (set-buffer nntp-buffer)
  (setq nntp-dot nil		; for the start up message
	buffer-read-only nil)
  (erase-buffer)
  (process-kill-without-query nntp)
  (set-process-filter nntp 'nntp-filter))

(defun nntp-exec (clear finish comm &rest args)
  "(nntp-exec CLEAR FINISH COMM ARGS...) sends the command COMM,
with arguments ARGS... to the NNTP process.  It returns the value from
nntp-clean (ie, nil on any kind of failure).\n
The argument CLEAR, if non-nil, means clear nntp-buffer first.\n
Interactively, the entire NNTP command, with its arguments, is prompted
for.  Unless there's a prefix argument, the internal buffer is cleared.\n
The argument FINISH, if non-nil, means wait until the NNTP process is
completed.  Interactively FINISH is always t."
  (interactive (list (not current-prefix-arg) t
		     (read-from-minibuffer "NNTP command: ") nil))
  (if (interactive-p)
      (setq args (progn
		   (string-match "\\<[^ ]*\\>" comm)
		   (if (/= (length comm) (match-end 0))
		       (list (substring comm (1+ (match-end 0))))))
	    comm (substring comm (match-beginning 0) (match-end 0))))
  (cond ((stringp clear)
	 (signal 'wrong-type-argument (list 'clear-flag clear)))
	((stringp finish)
	 (signal 'wrong-type-argument (list 'finish-flag finish)))
	(t
	 (setq args (mapconcat 'identity args " "))))
  (if (and (not nntp-exec-force)		; WARNING: this fails if
	   (not (eq major-mode 'index-mode))	; I repeat a command that
	   (string= comm nntp-exec-comm)	; uses a default argument
	   (string= args nntp-exec-args)	; that is silently changed.
	   (not (string= comm "last"))
	   (not (string= comm "next")))
      nntp-exec-value
    (setq nntp-exec-comm comm nntp-exec-args args)
    (if clear (nntp-clear nntp-buffer))
    (setq nntp-dot (assoc comm nntp-dot-commands)
	  nntp-eot nil)
    (send-string nntp comm)
    (send-string nntp " ")
    (send-string nntp args)
    (send-string nntp "\n")
    (if nntp-exec-verbose (message "NNTP: %s %s" comm args))
    (prog1
	(if finish (nntp-finish))
      (if (interactive-p) (message nntp-info)))))

(defun nntp-finish ()
  "Finish the currently running NNTP command."
  (while (and (nntp-run-p)
	      (not nntp-eot))
    (gnews-accept-process-output nntp))
  (if (nntp-run-p)
      (prog1
	  (setq nntp-exec-value (nntp-clean nntp-buffer))
	(if nntp-exec-verbose (message nntp-info))
	(if nntp-exec-log-p
	    (setq nntp-exec-log (cons (cons (concat nntp-exec-comm
						    " " nntp-exec-args)
					    nntp-info)
				      nntp-exec-log))))
    (or (string= "quit" nntp-exec-comm)		; natural death
	(news-quit (y-or-n-p "Connection died: save the current roster? ")))))

(defun gnews-accept-process-output (proc)
  ;; On older Apollo's, make this defun an fset in the source,
  ;; as the byte-compiled version has timing errors:
  ;; (fset 'gnews-accept-process-output '(lambda (proc) "Like a-p-o, ..." ...))
  "Like accept-process-output, but ignore \"select errors\"."
  (condition-case gnews-error
      (accept-process-output proc)
    (error (if (equal gnews-error '(error "select error: Invalid argument"))
	       nil
	     (signal (car gnews-error) (cdr gnews-error))))))

(defun nntp-filter (proc string)
  "The filter for the NNTP process."
  (save-excursion
    (set-buffer nntp-buffer)
    (goto-char (point-max))
    (insert string)
    (goto-char 1)
    (if (looking-at "^[45]") (setq nntp-dot))	; it's an error
    (goto-char (point-max))
    (forward-line -1)
    (if nntp-dot
	(if (looking-at "^\\.\r?\n")
	    (progn (delete-region (point) (point-max))
		   (setq nntp-eot t)))
      (if (looking-at "^.*\r?\n")
	  (setq nntp-eot t)))
    (if (looking-at nntp-xfer-only)
	(with-output-to-temp-buffer "*gnews*warning*"
	  (message "Article retrieval by Message-ID only: %s"
		   "hit space to continue")
	  (ding)
	  (read-char)))
    (if (and (looking-at nntp-no-connect)
	     (not (eq (process-status nntp) 'open)))
	(progn (setq nntp-eot 'quit)
	       (error (concat (apply 'buffer-substring
				     (mapcar 'marker-position
					     (list (nth 2 (match-data))
						   (nth 3 (match-data)))))
			      ": host not found"))))
    (goto-char 1)
    (if (looking-at "^20[01]")			; initial connection
	(progn
	  (setq nntp-connect-message (buffer-string)
		nntp-version (or nntp-version (nntp-version))
		n-reply-allowed (null
				  (string-match "^201" nntp-connect-message)))
	  (erase-buffer)))
    (while (re-search-forward "\r$" nil t)
      (delete-region (match-beginning 0) (point)))))

(defun nntp-clear (buf)
  "Delete everything in the buffer BUFFER."
  (setq nntp-eot nil)
  (save-excursion
    (set-buffer buf)
    (erase-buffer)))

(defun nntp-clean (buf)
  "Remove unsightly trash from BUFFER; return nil if BUFFER is empty
or otherwise undesirable."
  (setq nntp-eot nil)
  (save-excursion
    (prog1
	(progn
	  (set-buffer buf)
	  (goto-char 1)
	  (setq nntp-info (buffer-substring 1 (gnews-eol)))
	  (not (or (= 1 (point-max))			 ; empty buffer
		   (string-match "^[45]" nntp-info)	 ; error message
		   (string-match "<0>" nntp-info))))	 ; bogus article
      (gnews-delete-line)
      (nntp-undot))))

(defun nntp-undot (&optional buf)
  "Remove leading periods from current position of BUF.  Return the number of
periods removed.  If optional argument BUF is nil, use the current buffer."
  (let ((dots 0))
    (if buf (set-buffer buf))
    (while (re-search-forward "^\\." nil t)
      (delete-region (match-beginning 0) (match-end 0))
      (setq dots (1+ dots))
      (forward-line 1))
    dots))

(defun nntp-run-p ()
  "Return t if the NNTP process is running, nil otherwise."
  (and (boundp 'nntp)
       (processp nntp)
       (eq (process-status nntp) 'open)))

(defun nntp-version ()
  "Return the NNTP version number."
  (interactive)
  (or nntp-version
      (if nntp-service
	  (let* ((i (string-match nntp-server-version nntp-connect-message))
		 (j (length nntp-server-version))
		 (k (string-match "[^ \t]"
				  (substring nntp-connect-message (+ i j))))
		 (l (string-match "[ \t]"
				  (substring nntp-connect-message (+ i j k))))
		 (v (substring nntp-connect-message (+ i j k) (+ i j k l))))
	    (if (interactive-p) (gnews-message "NNTP version %s" v) v))
	(if (interactive-p) (gnews-message "NNTP=Spool") "spool"))))

;;; Fast (detached?) indexing

(defun nntp-index-start ()
  "Start the Gnews NNTP stream for indexing."
  (setq nntp-index (open-network-stream nntp-process
					nntp-index-buffer
					gnews-spool-machine
					nntp-service))
  (set-buffer nntp-index-buffer)
  (setq buffer-read-only)
  (erase-buffer)
  (process-kill-without-query nntp-index)
  (set-process-filter nntp-index 'nntp-index-filter))

(defun nntp-index-run-p ()
  "Return t if the nntp-index process is running."
  (and (boundp 'nntp-index)
       (processp nntp-index)
       (eq (process-status nntp-index) 'open)))

(defun nntp-index-filter (proc string)
  "Filter for the nntp-index process."
  (setq nntp-index-p t)
  (set-buffer nntp-index-buffer)
  (setq article-field-list (list nil)
	nntp-index-done nil)
  (goto-char (point-max))
  (insert string)
  (goto-char 1)
  (while (not (eobp))
    (if (looking-at "^\\(20[01]\\|21[15]\\|423\\).*\r?$")
	(gnews-delete-line)
      (forward-line 1)))
  (goto-char 1)
  (let* ((hook-kill-continue t)
	 (hook hook-kill-per)
	 (h (if (boundp 'index-headers) (mapcar 'ignore index-headers)))
	 p q n i f g z junk)
    (while (and (not nntp-index-done)
		(re-search-forward "^221 \\([0-9]+\\) " nil t)
		(setq p (gnews-bol)
		      n (read (gnews-match 1)))
		(re-search-forward "^\\.\r?$" nil t)
		(setq q (gnews-eol)))
      (setq i index-headers z h)
      (while z				; h gets the headers
	(goto-char p)
	(setcar z (if (re-search-forward
			 (concat "^" (car i) ": *\\([^\r]*\\)") q t)
		      (gnews-match 1) ""))
	(setq i (cdr i) z (cdr z)))
      (setq z (cdr article-field-list))
      (while z				; a-f-l gets alist cdr's ""'ed
	(setcdr (car z) "")
	(setq z (cdr z)))
      (save-excursion
	(save-restriction
	  (narrow-to-region p q)
	  (goto-char p)
	  (forward-line 1)
	  (while (not (eobp))
	    (if (looking-at "^\\([^:]*\\): *\\([^\r]*\\)\r?$")
		(progn (setq f (gnews-match 1)
			     g (gnews-match 2))
		       (if (setq z (assoc f article-field-list))
			   (setcdr z g)
			 (nconc article-field-list (list (cons f g))))))
	    (forward-line 1))))
      (while (and hook hook-kill-continue (not junk))
	(setq junk (hook-kill-do (car hook) t)
	      hook (cdr hook)))
      (if (and junk (not index-show-kills))
	  (if (setq nntp-index-done (= n nntp-index-final))
	      (save-excursion
		(set-buffer index-buffer)
		(setq buffer-read-only)
		(goto-char (point-max))
		(if (not (bobp)) (delete-char -1))
		(setq buffer-read-only t)
		(setq junk nil)
		(index-done-do)))
	(save-excursion
	  (set-buffer index-buffer)
	  (setq buffer-read-only)
	  (setq nntp-index-done (= n nntp-index-final))
	  (goto-char (point-max))
	  (if (string< "" (mapconcat 'identity h ""))
	      (insert (format "%5d" n) (if junk "k" " ") " "
		      (index-line n index-format h index-filter index-sizes)
		      (if nntp-index-done "" "\n")))
	  (setq buffer-read-only t)
	  (if nntp-index-done (index-done-do))
	  (setq junk nil)
	  (set-buffer nntp-index-buffer)))
      (delete-region p q))))

(defun index-done-do ()
  (index-beginning-of-buffer)
  (setq buffer-read-only nil
	nntp-index-p nil)
  (if index-sort-do (index-sort))
  (index-beginning-of-buffer)
  (mapcar '(lambda (x) (insert (format "%5dm %s\n" (car x) (cdr x))))
	  (cdr index-perm-marks))
  (setq buffer-read-only t
	gnews-top-level-restore top-level
	top-level '(gnews-top-level))
  (if (or (eq index-ding t)
	  (and (numberp index-ding)
	       (< index-ding (count-lines 1 (point-max)))))
      (error "indexing...done")
    (throw 'top-level t)))

(defun gnews-top-level ()
  (message "indexing...done")
  (setq top-level gnews-top-level-restore)
  (if command-line-processed nil
    (normal-top-level)))

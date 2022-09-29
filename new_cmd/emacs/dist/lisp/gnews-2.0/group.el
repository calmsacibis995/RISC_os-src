;;; group.el: group-mode commands
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

;;; The commands that take article ranges as a prefix.

(mapcar '(lambda (c) (put c 'group-range-prefix 'range))
	'(group-junk group-mark group-mark-later group-mark-permanent
		     group-junk-local group-digit group-pipe group-save
		     group-pattern-forward-new group-pattern-forward
		     group-pattern-backward-new group-pattern-backward
		     group-cancel))

(mapcar '(lambda (c) (put c 'group-range-prefix 'prefix))
	'(group-catchup group-follow group-follow-yank group-reply
			group-reply-yank))

;;; the actual commands

(defun group-default ()
  "Run the default command, ie, the value of group-default-command"
  (interactive)
  (call-interactively group-default-command)
  (gnews-flush))

(defun group-next (arg)
  "Go to the next article in the current newsgroup.\n
If an index is present, \"next\" is defined as the next article
in the index-buffer."
  (interactive "p")
  (if (= article-current 0) (group-trace-return))
  (if (article-done) (article-junk))
  (setq group-default-command 'group-next-unread
	group-prompt-default group-prompt-normal)
  (if (boundp 'index-pop)
      (let ((b (current-buffer)))
	(set-buffer index-buffer)
	(if (index-eobp)
	    (progn
	      (set-buffer b)
	      (message "End of index buffer" (ding)))
	  (forward-line arg)
	  (cond ((catch 'article-nil (article-get (index-article) nil t))
		 (article-junk-local)
		 (group-next 1)))))
    (setq article-previous article-current)
    (if (<= article-current article-final)
	(let ((i 0))
	  (while (< i arg)
	    (setq article-current (article+1 t) i (1+ i)))))
    (if (< article-final article-current)
	(group-last)
      (cond ((catch 'article-nil (article-get article-current))
	     (article-junk-local)
	     (group-next 1)))))
  (gnews-flush))

(defun group-next-unread (&optional nojunk)
  "Go to the next unread article in the current newsgroup.\n
If an index is present, \"next\" is defined as the next article
in the index-buffer.\n
In Lisp code, non-nil optional argument NOJUNK means do not junk the
current article."
  (interactive)
  (if (= article-current 0) (group-trace-return))
  (if (and article-same-subject (interactive-p) (not (article-done)))
      (group-next-same-subject article-field-same-no)
    (if (and (not nojunk) (<= article-current article-final))
	(article-junk))
    (setq group-default-command 'group-next-unread
	  group-prompt-default group-prompt-normal
	  article-same-subject nil)
    (while				; We have to manuever here to avoid
	(eq t (setq article-status	; recursive throws from other groups.
		    (catch 'article-nil	; We catch junkings/cancellations/etc.
		      (if (if (boundp 'index-pop)
			      (let ((b (current-buffer)))
				(set-buffer index-buffer)
				(condition-case nil
				    (index-forward 1)
				  (error (set-buffer b)
					 (throw 'article-nil 'i)))
				(prog1
				    (article-current-set (index-article))
				  (set-buffer b)))
			    (article-current-set (article+1))
			    (<= article-current article-final))
			  (article-get article-current hook-kill-per t)
			(group-end-wrap))	; loop around
		      'y))))			; so return a success
    (cond ((eq article-status 'q)
	   (news-next-unread-maybe t))
	  ((eq article-status 'i)
	   (group-next-but-index-done t))
	  ((eq article-status 'c)
	   (group-next-but-no-wrap))))
  (gnews-flush))

(defun group-end-wrap ()
  "Function to call when wrapping around the end."
  (article-current-set article-first)
  (nntp-exec t t "stat" article-first)
  (article-current-set (article+1))
  (if (<= article-current article-final)
      (article-get article-current hook-kill-per t)
    (throw 'article-nil 'q)))

(defun group-next-same-subject (arg)
;;; rewrite some day, eh?  This stinks.
  "Search for next unread article matching subject.  With prefix argument
ARG, search for next unread article matching ARG'th header field given in
index-headers (1-based).  ARG=0 means turn off subject-search mode."
  (interactive "p")
  (if (= article-current 0) (group-trace-return))
  (if (zerop arg) (group-next-unread))
  (if (<= article-current article-final) (article-junk))
  (setq group-default-command 'group-next-same-subject
	group-default-prompt group-same-subject-prompt
	article-field-same-no arg
	article-field-same (nth (1- arg) index-headers)
	article-same-subject (reply-re-1
			       (article-field article-field-same)))
  (while
      (eq t (setq article-status	 ; Matt Crawford
		  (catch 'article-nil
		    (article-current-set
		     (let ((i (amark-next-unread article-current amark)))
		       (catch 'set-art
			 (while (<= i article-final)
			   (if (string= article-same-subject
					(nth article-field-same-no
					     (index-prepare
					       i index-headers)))
			       (throw 'set-art i))
			   (setq i (amark-next-unread i amark)))
			 (throw 'set-art (amark-next-unread
					   article-first amark)))))
		    (if (<= article-current article-final)
			(let ((nntp-exec-force t))
			  (article-get article-current gnews-hook-per))
		      (throw 'article-nil 'q))
		    'y))))
  (cond ((eq article-status 'q)
	 (news-next-unread-maybe t))
	((eq article-status 'i)
	 (group-next-but-index-done t))
	((eq article-status 'c)
	 (group-next-but-no-wrap)))
  (gnews-flush))

(defun group-previous (arg)
  "Go to the previous article in the current newsgroup.\n
If an index is present, \"previous\" is defined as the previous
article in the index-buffer."
  (interactive "p")
  (if (= article-current 0) (group-trace-return))
  (setq group-default-command 'group-next-unread
	group-prompt-default group-prompt-normal)
  (if (boundp 'index-pop)
      (let ((b (current-buffer)))
	(set-buffer index-buffer)
	(if (index-bobp)
	    (progn
	      (set-buffer b)
	      (message "Start of index buffer" (ding)))
	  (forward-line (- arg))
	  (cond ((catch 'article-nil (article-get (index-article) nil t))
		 (article-junk-local)
		 (group-previous 1)))))
    (setq article-previous article-current)
    (let ((i 0))
      (if (< article-final article-current)
	  (progn
	    (nntp-exec t t "stat" article-final)
	    (setq i (1+ i)))
	(if (article-done) (article-junk)))
      (while (< i arg)
	(setq article-current (article-1 t) i (1+ i))))
    (if (< article-final article-current)	 ; unlikely, but...
	(setq article-current article-final))
    (cond ((< article-current article-first)
	   (message "no previous article" (ding)))
	  ((= article-current article-previous)
	   (message "no previous article" (ding)))
	  ((cond ((catch 'article-nil (article-get article-current))
		  (article-junk-local)
		  (group-previous 1))))))
  (gnews-flush))

(defun group-previous-unread ()
  "Go to the previous unread article in the current newsgroup.\n
If an index is present, \"previous\" is defined as the previous
article in the index-buffer."
  (interactive)
  (if (= article-current 0) (group-trace-return))
  (let ((end (< article-final article-current))
	(art article-current)
	(prev (and (not (boundp 'index-pop))
		   (amark-previous-unread article-current amark))))
    (if end nil		; if end, the resetting of article-current is below
      (article-junk)
      (if prev (article-current-set prev)))
    (if (and prev (zerop prev))
	(setq article-status 'q)
      (while
	  (eq t (setq article-status
		      (catch 'article-nil
			(if (if (boundp 'index-pop)
				(let ((b (current-buffer)))
				  (set-buffer index-buffer)
				  (condition-case nil
				      (index-backward 1)
				    (error (set-buffer b)
					   (throw 'article-nil 'i)))
				  (prog1
				      (article-current-set (index-article))
				    (set-buffer b)))
			      (article-current-set (if end
						       (amark-previous-unread
							 (1+ article-final)
							 amark)
						     (article-1)))
			      (setq end)
			      (<= article-first article-current))
			    (article-get article-current hook-kill-per t)
			  ;; wraparound--is this correct?
			  (article-current-set (amark-previous-unread
						 (1+ article-final)
						 amark))
			  (nntp-exec t t "stat" article-current)
			  (if (<= article-first article-current)
			      (article-get article-current hook-kill-per)
			    (throw 'article-nil 'q)))
			'y)))))			; so return a success
    (cond ((eq article-status 'q)
	   (message "no previous unread message" (ding))
	   (setq article-current art))
	  ((eq article-status 'i)
	   (group-next-but-index-done nil))))
  (gnews-flush))

(defun group-last-seen ()
  "Toggle to the previous article seen."
  (interactive)
  (if (= article-current 0)
      (group-trace-return)
    (let* ((ah (gnadr article-history))
	   (gh (gnadr ah))
	   (nh (car ah)))
      (if (string= group-current gh)
	  (cond ((catch 'article-nil (article-get nh nil t))
		 (article-junk-local)
		 (message "article %d was just cancelled" nh)))
	(message "just entered newsgroup" (ding))))
    (gnews-flush)))

(defun group-previous-same-subject ()
  (interactive)
  (message "unimplemented")
  (if (= article-current 0) (group-trace-return))
  (gnews-flush))

(defun group-back ()
  "Back up one screen."
  (interactive)
  (if (< article-final article-current)
      (ding)
    (let (gnews-rot13-p) (article-mode))
    (condition-case nil
	(article-back)
      (error))
    (if (article-done) (group-mode)))
  (gnews-hilite)
  (gnews-flush))

(defun group-back-half ()
  "Back up half a screen."
  (interactive)
  (if (< article-final article-current)
      (ding)
    (let (gnews-rot13-p) (article-mode))
    (condition-case nil
	(article-back-half)
      (error))
    (if (article-done) (group-mode)))
  (gnews-hilite)
  (gnews-flush))

(defun group-quit ()
  "Quit the current newsgroup."
  (interactive)
  (if (boundp 'index-pop)
      (let ((iw (get-buffer-window index-buffer)))
	(if (and index-pop iw (not (one-window-p)))
	    (delete-window iw))
	(set-window-configuration index-return-configuration)
	(bury-buffer index-buffer)))
  (group-quit-intern)
  (news-mode)
  (if (interactive-p)
      (progn (setq news-default-command 'news-next-unread)
	     (message "quit to top level--what next %s ? " news-prompt-next)))
  (gnews-flush))

(defun group-quit-restore ()
  "Quite the current newsgroup, restoring .gnewsrc to its previous status."
  (interactive)
  (setq amark amark-entry)
  (group-quit))

(defun group-quit-emergency ()
  "Quit the newsreader efficiently, trying to save what can be saved.\n
This is meant for when Gnews gets Emacs Lisp errors on trying to quit a
newsgroup.  As far as I know, this only occurs when tinkering with the code."
  (interactive)
  (group-roster-write gnews-rc-file-new)
  (news-mode)
  (news-quit t))

(defun group-first ()
  "Go to the first non-expired article in the newsgroup."
  (interactive)
  (cond ((catch 'article-nil (article-get article-first nil t))
	 (setq article-current article-first)
	 (group-next 1))))

(defun group-last ()
  "Go to the pseudo-article at the end of the newsgroup."
  (interactive)
  (setq buffer-read-only)
  (erase-buffer)
  (setq buffer-read-only t)
  (group-mode)
  (setq gnews-mode-string (concat group-current " $/" article-final)
	gnews-rot13-p nil
	article-field-list (list nil (cons "Newsgroups" group-current))
	article-grab-point nil)
  (article-current-set (1+ article-final))
  (message "end of group")
  (set-buffer-modified-p t)		; AR
  (run-hooks 'group-last-hook)
  (gnews-flush))

(defun group-junk (art-list)
  "Junk the current article, that is, mark it as read.\n
In Lisp code, ART-LIST is an amark of articles to junk."
  (interactive (list (list article-current)))
  (setq art-list (umark-set-last article-final art-list))
  (amark-loop art-no art-list
    (article-junk nil art-no))
  (if (interactive-p)
      (message "%d: junked" article-current)
    (if art-list (message "junked")))
  (gnews-flush))

(defun group-junk-local (art-list)
  "Junk the current article within the current newsgroup only.\n
In Lisp code, ART-LIST is an amark of articles to junk."
  (interactive (list (list article-current)))
  (setq art-list (umark-set-last article-final art-list))
  (amark-loop art-no art-list
    (article-junk t art-no))
  (if art-list (message "junked locally") (ding))
  (gnews-flush))

(defun group-mark (art-list)
  "Mark the current article as unread (in this newsgroup only).\n
In Lisp code, ART-LIST is an amark of articles to mark."
  (interactive (list (list article-current)))
  (setq art-list (umark-set-last article-final art-list))
  (amark-loop art-no art-list
    (setq amark (amark-list-delete art-no amark))
    (setq article-junkable 'mark))
  (if (interactive-p) (if art-list (message "marked") (ding)))
  (gnews-flush))

(defun group-mark-later (art-list)
  "Mark the current article as unread (in this newsgroup only), but
only after exiting the newsgroup.\n
In Lisp code, ART-LIST is an amark of articles to mark."
  (interactive (list (list article-current)))
  (setq art-list (umark-set-last article-final art-list))
  (amark-loop art-no art-list
    (if (memq art-no group-mark-later-list) nil
      (nconc group-mark-later-list (list art-no))))
  (if (interactive-p) (if art-list (message "marked for later") (ding)))
  (gnews-flush))

(defun group-mark-permanent (art-list)
  "Mark the current article as permanently unread (in this newsgroup only).\n
This means it will show up with an \"m\" label, available for the usual
perusal, in the index until the article disappears on its own.  The article
will otherwise be treated as read.\n
In Lisp code, ART-LIST is an amark of articles to mark."
  (interactive (list (list article-current)))
  (setq art-list (umark-set-last article-final art-list))
  (amark-loop art-no art-list
    (hook-kill-add group-current
      (read (concat "(pre nil setq index-perm-marks "
		    "(append index-perm-marks (list (cons "
		    art-no " "
		    (prin1-to-string
		      (index-line art-no index-format
				  (cdr (index-prepare art-no index-headers))
				  index-filter index-sizes))
		    "))))"))))
  (if (interactive-p) (if art-list (message "marked permanently") (ding)))
  (gnews-flush))

(defun group-kill (pfx)
  "Kill this subject of discussion.  With prefix argument PFX, the
killing is local to the current newsgroup."
  (interactive "P")
  (if (< article-final article-current)
      (ding)
    (if (or group-kill-auto (y-or-n-p "Kill this subject? "))
	(let ((hk (list (list "Subject"
			      (regexp-quote
				(reply-re-0
				  (article-field "Subject")))
			      (if pfx 'article-junk-local 'article-junk)))))
	  (if hook-kill-per
	      (nconc hook-kill-per hk)
	    (setq hook-kill-per hk))
	  (article-junk pfx)
	  (message "killed")
	  (group-default))))
  (gnews-flush))

(defun group-kill-permanent ()
  "Kill this subject of discussion permanently.\n
The variable article-auto-junk-locally, if non-nil (at kill time),
makes such kills local."
  (interactive)
  (if (< article-final article-current)
      (ding)
    (if (or group-kill-auto (y-or-n-p "Kill this subject permanently? "))
	(let ((hk (list "Subject"
			(regexp-quote (reply-re-0 (article-field "Subject")))
			(if article-auto-junk-locally
			    'article-junk-local 'article-junk))))
	  (if hook-kill-per
	      (nconc hook-kill-per (list hk))
	    (setq hook-kill-per (list hk)))
	  (hook-kill-add (hook-kill-group group-current) hk)
	  (message "%s: killed permanently"
		   (reply-re-0 (article-field "Subject")))
	  (group-default))))
  (gnews-flush))

(defun group-catchup (pfx arg)
  "Catch up in the current newsgroup.\n
The two arguments, PFX and ARG--passed interactively as the literal and
numeric prefix argument respectively--control how many newsgroups to
leave unread at the end of the newsgroup:
 * If PFX is null, catch up all the articles.
 * If PFX is zero, catch up through the current article.       
 * If PFX is non-zero numeric, catch up all but the last ARG articles.
 * If PFX is a list, catch up all but the last 10*log_4(ARG) articles.
Note that log_4(ARG) is interactively a count of \\[universal-argument] 's.\n
Cross-posted articles will not be marked read in their other newsgroups."
  (interactive "P\np")
  (if (< (roster-catchup group-current t pfx arg) article-final)
      (cond ((catch 'article-nil (article-get article-current hook-kill-per))
	     (article-junk-local)
	     (group-next-unread t)))
    (if (boundp 'index-pop)
	(progn
	  (bury-buffer index-buffer)
	  (if index-pop
	      (let ((iw (get-buffer-window index-buffer)))
		(if iw (progn (delete-window iw)))))))
    (group-quit-intern)
    (news-mode)
    (news-next-unread-maybe))
  (gnews-flush))

(defun group-unsubscribe ()
  "Unsubscribe to current newsgroup.  It also junks the current article,
unless it was marked."
  (interactive)
  (if (roster-unsubscribe group-current)
      (progn
	(if (and (not (eq article-junkable 'mark))
		 (<= article-current article-final))
	    (article-junk))
	(group-quit-intern)
	(news-mode)
	(news-next-unread-maybe))))

(defun group-save (art-list)
  "Append current article to a file.  The variable gnews-save-style
controls the default output style, which see.\n
In Lisp code, ART-LIST is an amark of article numbers to save."
  (interactive (list (list article-current)))
  ;; not yet loopable: art-list is ignored
  (if gnews-slashify (gnews-mkdir (file-name-directory group-last-save)))
  (if (interactive-p) (setq gnews-prefix (concat article-current)))
  (if article-grab-point (article-forward-intern group-save-junk))
  (if (commandp gnews-save-style)	; must expect, reset group-last-save
      (call-interactively gnews-save-style)
    (let ((save (if gnews-save-style
		    (file-name-nondirectory group-last-save)
		  (gnews-save-name group-current gnews-prefix)))
	  (dir (file-name-directory group-last-save)))
      (setq group-last-save (expand-file-name
			      (read-file-name
				(format "Save to file: (default %s) " save)
				(file-name-directory group-last-save)
				(file-name-nondirectory save)) ;EHL
			      dir)))
    (write-region 1 (article-max) group-last-save t))
  (gnews-flush))

(defun group-pipe (pfx command)
  "Send article through a pipe--prefix argument means just use article body."
  (interactive "P\nsshell command: |")
  ;; not yet loopable
  (if article-grab-point (article-forward-intern group-pipe-junk))
  (let (art art-list)
    (if (interactive-p)
	(setq art-list (list article-current))
      (setq art-list pfx
	    pfx gnews-last-prefix
	    command (read-minibuffer "shell command: ")))
    (shell-command-on-region
      (if pfx (article-min) 1) (article-max) command))
  (gnews-flush))

(defun group-trace (arg)
  "Locate the article referenced by the current one.  With a positive
prefix argument ARG, look back ARG many references.  With a 0 prefix
argument, go to the first article in the references."
  (interactive "p")
  (if (<= 0 arg)
      (let ((ref (article-field "References")))
	(if (string= ref "")
	    (message "Original article" (ding))
	  (article-get-msg-id
	   (gnews-string-as-buffer ref nil
	     (if (zerop arg)
		 (progn
		   (beginning-of-line)
		   (re-search-forward "<[^>]*>" nil 1))
	       (end-of-line)
	       (re-search-backward "<[^>]*>" nil 1 arg))
	     (gnews-match 0)))))
    (message "argument must be zero or positive" (ding)))
  (gnews-flush))

(defun group-trace-return ()
  "Return to the parent article in a trace back sequence."
  (interactive)
  (if (= 0 article-current)
      (cond ((catch 'article-nil (article-get article-trace nil t))
	     (article-junk-local)
	     (gnews-message "whoops--%d was just cancelled" article-trace)))
    (message "Not in a trace back sequence" (ding)))
  (gnews-flush))

(defun group-pattern-forward (art-list)
  "Repeat the last forward pattern search among the articles in ART-LIST.
If no pattern search has been tried yet within the current newsgroup,
prompt for a header and regexp.\n
Interactively, the search begins at the next article.\n
A prefix argument, if present, means do not search over junked articles.\n
To change the current header/pattern, use \
\\<group-mode-map>\\[group-pattern-forward-new]."
  (interactive (list (list (amark-cons (1+ article-current) article-final))))
  (if group-pattern-redo
      (group-pattern-forward-new
	art-list group-pattern group-pattern-field group-pattern-command)
    (setq group-pattern-redo t)
    (call-interactively 'group-pattern-forward-new)))

(defun group-pattern-forward-new (art-list patt head comm)
  "Search forwards for an article, among the articles in ART-LIST, in the
current newsgroup, for regexp PATTern in header field HEAD.  [[When found,
execute command COMM--not yet implemented.]]\n
Interactively, the search begins at the next article and continues through
to the end of the newsgroup.  The regexp is entered at the first prompt, the
header at the second prompt [[, and the command at the third prompt]].\n
A prefix argument, if present, means do not search over junked articles."
  (interactive
    (list
      (list (amark-cons (1+ article-current) article-final))
      (read-from-minibuffer
	(concat group-query-pf "pattern: ") group-pattern)
      (completing-read (concat group-query-pf "header: ")
		       article-field-list nil nil group-pattern-field)
      nil))
  (setq group-pattern patt
	group-pattern-field head
	group-pattern-command comm
	group-default-command 'group-pattern-forward
	group-prompt-default group-prompt-pf)
  (let* ((msg (message "Searching..."))
	 (all (not current-prefix-arg))
	 (art (catch 'article-patt
		(amark-loop art-no art-list
		  (if (or all (not (amark-member art-no amark)))
		      (let ((hook-kill-art art-no))
			(hook-kill-do (list group-pattern-field group-pattern
					     'throw ''article-patt art-no))
			(if (zerop (% art-no 50)) ; kind of crude...
			    (message (setq msg (concat msg art-no "..."))))))
		  nil))))
    (if art (article-get-slow art nil t)
      (message "%s: pattern not found%s"
	       group-pattern (if all "" " (among unread articles)") (ding))))
  (gnews-flush))

(defun group-pattern-backward (art-list)
  "Repeat the last backward pattern search among the articles in ART-LIST.
If no pattern search has been tried yet within the current newsgroup, prompt
for a header and regexp.\n
Interactively, the search begins at the previous article.\n
A prefix argument, if present, means do not search over junked articles.\n
To change the current header/pattern, use \
\\<group-mode-map>\\[group-pattern-backward-new]."
  (interactive (list (list (amark-cons article-first (1- article-current)))))
  (if group-pattern-redo
      (group-pattern-backward-new
	art-list group-pattern group-pattern-field group-pattern-command)
    (setq group-pattern-redo t)
    (call-interactively 'group-pattern-backward-new)))

(defun group-pattern-backward-new (art-list patt head comm)
  "Search backwards for an article, among the articles in ART-LIST, in the
current newsgroup, for regexp PATTern in header field HEAD.  [[When found,
execute command COMM--not yet implemented.]]\n
Interactively, the search begins at the previous article and continues back
to the start of the newsgroup.  The regexp is entered at the first prompt,
the header at the second prompt [[, and the command at the third prompt]].\n
A prefix argument, if present, means do not search over junked articles."
  (interactive
    (list
      (list (amark-cons article-first (1- article-current)))
      (read-from-minibuffer
	(concat group-query-pb "pattern: ") group-pattern)
      (completing-read (concat group-query-pb "header: ")
		       article-field-list nil nil group-pattern-field)
      nil))
  (setq group-pattern patt
	group-pattern-field head
	group-pattern-command comm
	group-default-command 'group-pattern-backward
	group-prompt-default group-prompt-pb)
  (let* ((msg (message "Searching..."))
	 (all (not current-prefix-arg))
	 (art (catch 'article-patt
		(amark-pool art-no art-list
		  (if (or all (not (amark-member art-no amark)))
		      (let ((hook-kill-art art-no))
			(hook-kill-do (list group-pattern-field group-pattern
					     'throw ''article-patt art-no))
			(if (zerop (% art-no 50)) ; kind of crude...
			    (message (setq msg (concat msg art-no "..."))))))
		  nil))))
    (if art (article-get-slow art nil t)
      (message "%s: pattern not found%s"
	       group-pattern (if all "" " (among unread articles)"))))
  (gnews-flush))

(defun group-digit (d)
  "Provide a digit argument for a command.\n
If the 'group-range-prefix property of the eventual command is nil,
the formed numeric argument is passed to the command as is.\n
If the property is 'range, the digits, along with any -,^,$ are
passed to command as an amark for accessing articles by number.\n
If the property is 'prefix, then the first value is passed as a
prefix argument."
  (interactive (list (this-command-keys)))
  ;; A recursive call used for nil command (= set to article)
  ;; since we don't want to introduce an artifical key-binding.
  (if (interactive-p)
      (let* ((prefix (read-from-minibuffer
		       "article(s): " (concat d) group-range-keymap))
	     (command  (lookup-key (current-local-map)
				   (char-to-string gnews-minibuf-last-char)))
	     (style (get command 'group-range-prefix)))
	(setq prefix (if (eq style 'range)
			 (gnews-amarkify prefix article-first article-final)
		       (read prefix)))
	(if (eq style 'prefix)
	    (funcall command prefix prefix)
	  (funcall command prefix)))
    (let ((n (amark-car (car prefix)))
	  (art-no (read gnews-prefix)))
      (cond ((<= art-no article-get-really-line)
	     (article-line art-no))
	    ((null n)
	     (message "article %s is out of range (%d-%d)" gnews-prefix
		      article-first article-final (ding)))
	    ((catch 'article-nil (article-get n nil t))
	     (article-junk-local)
	     (message "article %d does not exist" n)))))
  (gnews-flush))

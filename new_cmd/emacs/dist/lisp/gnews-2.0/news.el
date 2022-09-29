;;; news.el: news-mode commands for Gnews
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

(defun news-default ()
  "Run the default news command, as given by the value of
the variable news-default-command"
  (interactive)
  (call-interactively news-default-command))

(defun news-yes ()
  "Select the current newsgroup"
  (interactive)
  (if group-current
      (news-goto group-current)
    (news-end t))
  (gnews-flush))

(defun news-next ()
  "Proffer to read news from the next subscribed group for which news
has arrived in, whether or not new news has arrived."
  (interactive)
  (if group-current
      (news-next-intern
	t (cdr (memq (assoc group-current group-roster) group-roster)))
    (news-end t))
  (gnews-flush))

(defun news-next-unread ()
  "Proffer to read news from the next subscribed group for which news
has arrived in."
  (interactive)
  (if group-current
      (news-next-intern
	nil (cdr (memq (assoc group-current group-roster) group-roster)))
    (news-first-unread))
  (gnews-flush))

(defun news-previous ()
  "Proffer to read news from the next subscribed group back, whether or
not new news has arrived."
  (interactive)
  (if group-current
      (news-next-intern
	t (cdr (memq (assoc group-current group-roster)
		     (reverse group-roster))))
    (news-next-intern t (reverse group-roster)))
  (gnews-flush))

(defun news-previous-unread ()
  "Proffer to read news from the next subscribed group back for which
news has arrived in."
  (interactive)
  (if group-current
      (news-next-intern
	nil (cdr (memq (assoc group-current group-roster)
		       (reverse group-roster))))
    (news-next-intern nil (reverse group-roster)))
  (gnews-flush))

(defun news-first ()
  "Select the first newsgroup."
  (interactive)
  (setq news-seen)
  (news-next-intern t group-roster)
  (gnews-flush))

(defun news-first-unread ()
  "Select the first newsgroup with unread news"
  (interactive)
  (setq news-seen)
  (news-next-intern nil group-roster)
  (gnews-flush))

(defun news-last-seen ()
  "Return to the previous newsgroup mentioned"
  (interactive)
  (group-warn-delete-window t)
  (if group-previous
      (let ((g (assoc group-previous group-roster)))
	(setq group-previous group-current
	      group-set-current ""		; force a reset
	      news-default-command 'news-yes
	      news-next-message "")
	(group-new-news g)
	(group-proffer t g (group-get-info g)))
    (message "no previous newsgroup" (ding)))
  (gnews-flush))

(defun news-end (&optional err)
  "Select the pseudo-newsgroup at the end of the group list.\n
Optional argument ERR means ding the user."
  (interactive)
  (setq group-previous group-current
	group-current nil)
  (cond (news-seen
	 (setq news-default-command 'news-first-unread
	       news-next-message "")
	 (gnews-message "more %s ? " news-prompt-first))
	(t
	 (setq news-default-command 'news-quit)
	 (gnews-message "more %s ? " news-prompt-quit)))
  (if err (ding))
  (gnews-flush))

(defun news-goto (group &optional nosub dot)
  "Select newsgroup GROUP from .gnewsrc--resubscribes automatically.
In Lisp code, non-nil optional argument NOSUB blocks resubscription,
and non-nil DOT suspends typeahead flushing.\n
Interactively, name completion and abbreviations are available.  Name
completion is the standard Emacs feature.  Abbreviations are special
to Gnews, and are controlled by the following keys:
\\{gnews-abbrev-keymap}
Abbreviations can be added, deleted, and listed with\\<news-mode-map> \
\\[gnews-abbrev-add], \\[gnews-abbrev-delete], and \\[gnews-abbrev-list]\n
If the first character typed is \"\\[news-index]\", the group will be entered
in index-mode.  If the first character typed is an \"\\[news-at]\", the named
group will be set to but not entered."
  (interactive
   (list
    (group-name-read "goto newsgroup: " group-roster 'news-restrictions) nil))
  (group-warn-delete-window t)
  (if (interactive-p)
      (let ((n (aref news-mode-map group-read-mood)))
	(cond ((eq n 'news-index)		; =
	       (group-set group)
	       (news-index nil))
	      ((eq n 'news-at)			; @
	       (message "at %s" (group-set group)))
	      (t
	       (group-get group nosub dot))))
    (group-get group nosub dot))
  (if (interactive-p) (gnews-flush)))		; caller decides for itself

(defun news-visit (group)
  "Visit newsgroup GROUP from .gnewsrc without resubscribing."
  (interactive
   (list
    (group-name-read "visit newsgroup: " group-roster 'news-restrictions)))
  (if (interactive-p)
      (let ((n (aref news-mode-map group-read-mood)))
	(cond ((eq n 'news-index)		; =
	       (group-set group)
	       (news-index nil))
	      ((eq n 'news-at)			; @
	       (message "at %s" (group-set group)))
	      (t
	       (group-get group t))))
    (group-get group t))
  (gnews-flush))

(defun news-general-goto (group)
  "Select newsgroup GROUP, not necessarily in .gnewsrc."
  ;; unlike the analogous news-{goto,visit}, this is only interactive
  (interactive
   (list (group-name-read
	  "Goto Newsgroup: " (roster-all) 'news-restrictions)))
  (let ((gi (assoc group group-roster))
	(n (aref news-mode-map group-read-mood)))
    (if (nntp-exec t t "group" group)
	(if (or gi (group-proffer-new group))
	    (cond ((eq n 'news-index)		; =
		   (group-set group)
		   (news-index nil))		; doesn't work ??
		  ((eq n 'news-at)		; @
		   (message "at %s" (group-set group)))
		  (t				; have to think
		   (group-get group))))		; about this one
      (message "Newsgroup %s does not exist!" group)))
  (gnews-flush))

(defun news-new ()
  "Add recently arrived newsgroups to your .gnewsrc.  Upon being
prompted, use file-name completion to identify them.\n
Use \\[keyboard-quit] to quit."
  (interactive)
  (message "getting list of new newsgroups...")
  (if roster-new nil
    (setq roster-new
	  (delq nil (mapcar
		      (function
			(lambda (g)
			  (if (or (assoc (car g) group-roster)
				  (string-match
				    (concat "^" (regexp-quote (car g)))
				    roster-old))
			      nil g)))
		      (roster-all)))))
  (message "getting list of new newsgroups...done")
  (news-add roster-new "No more new newsgroups" 'news-all)
  (gnews-flush))

(defun news-add (new msg pred)
  "Add newsgroups from the list NEW; when done, show in the
minibuffer the message MSG.  PRED is a newsgroup name restriction
filter: only names that PRED returns non-nil on are offered."
  (condition-case nil
      (progn
	(while new
	  (let* ((add (group-name-read "Add newsgroup: " new pred))
		 (aft (group-name-read (format "place %s after: " add)
				       group-roster 'news-all))
		 (tail (memq (assoc aft group-roster) group-roster)))
	    (setcdr tail (cons (list add t) (cdr tail)))
	    (setq gnews-rc-dangle t new (delq (assoc add new) new))))
	(message msg)
	(sit-for 1))
    (quit))
  (news-at))

(defun news-delete (group)
  "Remove newsgroup GROUP from .gnewsrc file."
  (interactive
   (list
    (group-name-read "delete newsgroup: " group-roster 'news-restrictions)))
  (delq (assoc group group-roster) group-roster)
  (setq gnews-rc-dangle t)
  (gnews-flush))

(defun news-pattern-forward (patt)
  "Proffer to read news from the next subscribed-to newsgroup whose name
matches the regexp PATTern."
  ;; suppress when at $-end
  (interactive (list (read-string "Regexp forward: " news-pattern)))
  (setq news-pattern patt)
  (let ((ng (car (delq nil
		       (mapcar '(lambda (g)
				  (if (string-match patt (car g)) g))
			       (cdr (memq (assoc group-current group-roster)
					  group-roster)))))))
    (if ng
	(group-proffer-must (car ng) ng)
      (message "%s: pattern not found" patt (ding))))
  (gnews-flush))

(defun news-pattern-backward (patt)
  "Proffer to read news from the next subscribed-to newsgroup back whose
name matches the regexp PATTern."
  ;; fix when at $-end
  (interactive (list (read-string "Regexp backward: " news-pattern)))
  (setq news-pattern patt)
  (let* ((gr (reverse group-roster))
	 (ng (car (delq nil
			(mapcar '(lambda (g)
				   (if (string-match patt (car g)) g))
				(cdr (memq (assoc group-current gr) gr)))))))
    (if ng
	(group-proffer-must (car ng) ng)
      (message "%s: pattern not found" patt (ding))))
  (gnews-flush))

(defun news-unsubscribe ()
  "Unsubscribe from the current newsgroup."
  (interactive)
  (if (not group-current)
      (news-end t)
    (group-warn-delete-window t)
    (roster-unsubscribe group-current)
    (setq group-entry-command last-command)
    (news-next-unread-maybe)))

(defun news-at ()
  "Identify the current newsgroup."
  (interactive)
  (if (not group-current)
      (news-end t)
    (group-proffer-must group-current)
    (gnews-flush)))

(defun news-list ()
  "Switch to a roster of the entire newsgroup list."
  (interactive)
  (message "unimplemented")
  (gnews-flush))

(defun news-list-rc (pfx)
  "Switch to newsgroup roster, based on your .gnewsrc.  Prefix argument
PFX means include the unsubscribed groups.\n
>>Currently too primitive for actual usage.<<"
  (interactive "P")
  (roster-display pfx group-roster)
  (gnews-flush))

(defun news-move (group after)
  "Move newsgroup GROUP inside the group-roster to after newsgroup AFTER.
Interactively, move the current newsgroup's location to after the
prompted-for newsgroup."
  (interactive
   (list group-current (if group-current
			   (group-name-read
			     (concat "place " group-current " after: ")
			     group-roster
			     'news-all))))
  (if (not group-current)
      (news-end t)
    (setq gnews-rc-dangle t)
    (let* ((g (assoc group group-roster))
	   (f (assoc after group-roster))
	   (gr (delq g group-roster))
	   (hd (reverse (memq f (reverse gr))))
	   (tl (cdr (memq f gr))))
      (setq group-roster (nconc hd (list g) tl)))))

(defun news-catchup (pfx arg)
  "Catch up in the proffered newsgroup.\n
The two arguments, PFX and ARG--passed interactively as the literal and
numeric prefix argument respectively--control how many newsgroups to
leave unread at the end of the newsgroup:
 * If PFX is null, catch up all the articles.
 * If PFX is numeric, catch up all but the last ARG articles.
 * If PFX is a list, catch up all but the last 10*log_4(ARG) articles.
Note that log_4(ARG) is interactively a count of \\[universal-argument] 's.\n
Cross-posted articles will not be marked read in their other newsgroups."
  (interactive "P\np")
  (group-warn-delete-window t)
  (if (not group-current)
      (news-end t)
    (group-set group-current)
    (if (< (roster-catchup group-current t pfx arg) article-final)
	(news-goto group-current)
      (if (boundp 'index-pop)
	  (progn (bury-buffer index-buffer)
		 (makunbound 'index-pop)))
      (if (not (eq major-mode 'news-mode)) (news-mode))
      (setq group-entry-command last-command)
      (news-next-unread-maybe))
    (gnews-flush)))

(defun news-mark ()
  "Unread news.  All articles in the current newsgroup are marked as unread."
  (interactive)
  (if (not group-current)
      (news-end t)
    (if (y-or-n-p (format "mark %s unread? " group-current))
	(let* ((bg (1- article-first))
	       (gm (if (< 0 bg) (amark-cons 1 bg))))
	  (setcdr (assoc group-current group-roster) (if gm (list t gm) '(t)))
	  (setq amark (if gm (list gm)))
	  (group-roster-write gnews-rc-file-new)))
    (gnews-flush)))

(defun news-only-match (pattern)
  "Restrict future newsgroup profferings to those matching PATTERN."
  (interactive "sRestrict profferings to [regexp]: ")
  (fset 'news-restrictions
	(if (string= "" pattern)
	    'news-all
	  (` (lambda (g)
	       (, (documentation 'news-all))
	       (string-match (, pattern) (car g))))))
  (if (interactive-p)
      (message (news-restrictions-message pattern)))
  (gnews-flush))

(defun news-add-match (pattern)
  "Add newsgroups, not present in your .gnewsrc, matching the regular
expression PATTERN.  Use file-name completion to get their names, and
\\[keyboard-quit] to quit.\n
Upon finishing, future profferings will be restricted to PATTERN."
  (interactive "sAdd newsgroups matching [regexp]: ")
  (news-only-match pattern)
  (news-add (roster-all)
	    (news-restrictions-message pattern)
	    '(lambda (g)
	       (and (news-restrictions g)
		    (not (assoc (car g) group-roster))))))

(defun news-restrictions-message (s)
  (if (string= "" s)
      "Restrictions removed"
    (format "Restrictions %s in effect" s)))

(defun news-immediate ()
  "Enter the current newsgroup at the first unread article, but do not
actually set to the article.\n
Typeahead is never suppressed by this command."
  (interactive)
  (if group-current
      (news-goto group-current nil t)
    (news-end t)))

;;; .gnewsrc functions

(defun news-quit (arg)
  "Quit news, saving the internal news data for next time.\n
In Lisp code, nil ARG means restore the .gnewsrc."
  (interactive (list t))
  (group-warn-delete-window t)
  (if gnews-rc-dangle (group-roster-write gnews-rc-file-new))
  (if gnews-hook-dangle (gnews-hook-write gnews-hook-file))
  (if news-new-noted (setq roster-new (list (car roster-new))))
  (gnews-time-write gnews-time-file)
  (setq mode-line-format default-mode-line-format) 
  (set-buffer-modified-p t)
  (if (nntp-run-p) (nntp-exec t t "quit"))
  (if (nntp-index-run-p) (send-string nntp-index "quit\n"))
  (if (processp nntp) (delete-process nntp))
  (if (processp nntp-index) (delete-process nntp-index))
  (copy-file (if arg gnews-rc-file-new gnews-rc-file-old) gnews-rc-file t)
  (mapcar '(lambda (b)
	     (if (bufferp b)
		 (if gnews-buffer-clean (kill-buffer b) (bury-buffer b))))
	  (gnews-buffer-list))
  (run-hooks 'gnews-quit-hook)
  (gnews-flush)
  (if (boundp 'gnews-configuration-return)
      (set-window-configuration gnews-configuration-return)
    (switch-to-buffer "*scratch*"))
  (sit-for 0))

(defun news-quit-restore ()
  "Quit, but leave .gnewsrc file alone.  What would have been saved
under \\[news-quit] appears in .gnewsrc.new."
  (interactive)
  (news-quit nil))

(defun news-rc-restore ()
  "Convert current group roster status into a .newsrc file.  The command
queries before overwriting an existing .newsrc file."
  (interactive)
  (gnews-to-news-rc (concat gnews-dot-dir ".newsrc"))
  (gnews-flush))

(defun news-restart ()
  "Checkpoint everything, and restart the NNTP internals."
  (interactive)
  (group-warn-delete-window t)
  (if gnews-rc-dangle (group-roster-write gnews-rc-file-new))
  (if gnews-hook-dangle (gnews-hook-write gnews-hook-file))
  (if news-new-noted (setq roster-new (list (car roster-new))))
  (gnews-time-write gnews-time-file)
  (if (nntp-run-p) (nntp-exec t t "quit"))
  (if (nntp-index-run-p) (send-string nntp-index "quit\n"))
  (if (processp nntp) (delete-process nntp))
  (if (processp nntp-index) (delete-process nntp-index))
  (nntp-start "Re")
  (nntp-index-start)
  (message "just a moment...")
  (roster-string-set)
  (cond (group-current
	 (setq group-set-current ""
	       news-default-command (if (string= news-prompt-return
						 news-prompt-yes)
					'news-yes 'news-next))
	 (group-proffer-must group-current))
	(t
	 (setq news-seen t)
	 (news-end))))

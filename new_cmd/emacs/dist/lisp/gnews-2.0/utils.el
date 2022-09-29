;;; utils.el: basic utilities for Gnews
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

;;; .gnewsrc.* handlers

(defun gnews-from-news-rc ()
  "Convert .newsrc file to .gnewsrc format"
  (gnews-string-as-buffer "" nil
    (insert-file (concat gnews-dot-dir ".newsrc"))
    (message ".newsrc to .gnewsrc conversion...")
    (goto-char 1)
    (if (looking-at "^options ")
	(progn
	  (setq gnews-rn-options (buffer-substring 1 (gnews-eol)))
	  (gnews-delete-line)))
    (while (re-search-forward "^\\([^:!\n ]+\\)$" nil t)
      (replace-match "\\1: 0"))
    (goto-char 1)
    (while (re-search-forward "^\\([^:!]*\\)\\(.\\) *\\(.*\\)$" nil t)
      (replace-match "   (\"\\1\" \\2 \\3)" t nil))
    (goto-char 1)
    (while (search-forward ":" nil t)
      (replace-match "t"))
    (goto-char 1)
    (while (search-forward "!" nil t)
      (replace-match "nil"))
    (goto-char 1)
    (while (search-forward "," nil t)
      (replace-match " "))
    (goto-char 1)
    (while (re-search-forward "\\([0-9]+\\)-\\([0-9]+\\)" nil t)
      (replace-match "(\\1 . \\2)"))
    (goto-char 1)
    (insert "(setq\t\t\t\t;\n group-roster\n '(\n")
    (goto-char (point-max))
    (insert "   ))\n")
    (eval-current-buffer)
    (write-file gnews-rc-file)
    (write-file gnews-rc-file-new))
  (message ".newsrc to .gnewsrc conversion...done"))

(defun gnews-to-news-rc (file)
  "Convert group-roster to .newsrc format, and write to FILE."
  (interactive (list (concat gnews-dot-dir ".newsrc")))
  (let ((max-lisp-eval-depth 3000))
    ;; Until gnews-replace is written non-recursively.
    (if (or (not (file-exists-p file))
	    (yes-or-no-p "Overwrite existing .newsrc? "))
	(gnews-string-as-buffer
	  (apply 'concat
		 (mapcar '(lambda (g)
			    (insert (car g) (if (gnadr g) ": " "! ")
				    (if (gnddr g)
					(gnews-replace "[()]" ""
					  (gnews-replace ")? (?" ","
					    (gnews-replace " \\. " "-"
					      (prin1-to-string (gnddr g)))))
				      "")
				    ?\n)
			    (let ((ll (count-lines 1 (point))))
			      (if (zerop (% ll 10))
				  (progn (message "%d groups ..." ll) ""))))
			 group-roster)) nil
	  (goto-char 1)
	  (if (boundp 'gnews-rn-options) (insert gnews-rn-options ?\n))
	  (write-region 1 (point-max) file nil 0)
	  (message "%s written" file))))
  (gnews-flush))

(defun group-roster-write (file)
  "Write out group-roster to FILE."
  (let ((b (current-buffer))
	(fb (find-file-noselect file))
	(gp (car group-roster))
	(r (cdr group-roster))
	version-control)
    (set-buffer fb)
    (erase-buffer)
    (insert "(setq\t\t\t\t; -*- Emacs-Lisp -*-\n group-roster\n '(")
    (while gp
      (insert "\n   ")
      (prin1 gp fb)
      (setq gp (car r) r (cdr r)))
    (insert "\n   ))\n")
    (write-region 1 (point-max) buffer-file-name nil 0)
    (set-buffer-modified-p nil)
    (switch-to-buffer b)
    (kill-buffer fb)
    (setq gnews-rc-dangle)))

(defun gnews-hook-write (file)
  "Write out hook-kill-all and gnews-abbrev to FILE."
  (gnews-string-as-buffer "" nil
    (insert "(setq\t\t\t\t; -*- Emacs-Lisp -*-\n hook-kill-all\n '(nil")
    (mapcar '(lambda (g)
	       (let ((gh (cdr (assoc g hook-kill-all))))
		 (if (car gh)
		     (progn
		       (insert "\n   (" (prin1-to-string g))
		       (mapcar '(lambda (x)
				  (insert "\n    " (prin1-to-string x)))
			       gh)
		       (insert "\n    )")))))
	    (mapcar 'car (cdr hook-kill-all)))
    (insert "\n   ))\n\f\n(setq\n gnews-abbrev\n '(")
    (mapcar '(lambda (x) (insert "\n   " (prin1-to-string x))) gnews-abbrev)
    (insert "\n   ))\n")
    (if (boundp 'gnews-rn-options)
	(insert "\f\n(setq gnews-rn-options \"" gnews-rn-options "\")\n"))
    (write-region 1 (point-max) file nil 0))
  (setq gnews-hook-dangle))

(defun gnews-time-write (file)
  "Write out roster-new to FILE."
  (gnews-string-as-buffer "" nil
    (insert "(setq\t\t\t\t; -*- Emacs-Lisp -*-\n roster-new\n '(("
	    (prin1-to-string (gnaar roster-new)) " . "
	    (prin1-to-string (gndar roster-new)) ")")
    (mapcar '(lambda (x) (insert "\n   " (prin1-to-string x)))
	    (cdr roster-new))
    (insert "\n   ))\n")
  (write-region 1 (point-max) file nil 0))
  (setq news-new-noted))

(defun roster-new-check ()
  (let ((buff (set-buffer (find-file-noselect gnews-list-file))))
    (setq roster-old (buffer-string)
	  roster-old-count (count-lines 1 (point-max)))
    (erase-buffer)
    (insert roster-string)
    (setq roster-new-count (count-lines 1 (point-max))
	  roster-new-p (and (< 0 roster-old-count)
			    (< roster-old-count roster-new-count)))
    (write-region 1 (point-max) buffer-file-name nil 0)
    (set-buffer-modified-p nil)
    (switch-to-buffer news-buffer)
    (kill-buffer buff)
    roster-new-p))

(defun gnews-load (file &optional mok nom nos)
  "Safely load FILE, with optional arguments as in load, which see.
On error, abort NNTP and throw the user into FILE.  [[On quit, abort
NNTP and get out of Gnews.]]"
  (condition-case q
      (load file mok nom nos)
    (error (if (processp nntp)
	       (progn (delete-process nntp)
		      (delete-process nntp-index)))
	   (bury-buffer news-buffer)
	   (find-file file)
	   (signal (car q) (cdr q)))
;;; (quit (nntp-exec t t "quit")		; doesn't work?
;;;	  (bury-buffer news-buffer))		; so why not?
    ))

(defun gnews-hook-load ()
  "Load the .gnewsrc.hook file."
  (if (file-exists-p gnews-hook-file)
      (gnews-load gnews-hook-file nil t t)
    (setq gnews-hook-dangle t
	  hook-kill-all (list nil)
	  gnews-abbrev (list nil)))
  ;; Some gnews-hook => hook-kill-all rename conversions.
  (if (boundp 'gnews-hook-list)
      (progn (setq hook-kill-all gnews-hook-list
		   gnews-hook-dangle t)
	     (makunbound 'gnews-hook-list)
	     (switch-to-buffer gnews-warn-buffer)
	     (insert "** WARNING **\n\n\
I have renamed numerous internal variables, so as to make most things
cleaner.  The variable that contains your hook-kills will be automatically
fixed--the fact that it is out-of-date is what triggered this warning in
the first place.

You will have to fix a few other internal variables that you might use
somewhere.  Fortunately, the command to do this has been written for you.
It is named `M-x gnews-rename', and is in `Help.el'.  You do not have to
run this immediately: if you don't certain of your customizations may
fail to take effect, nothing worse.

You use gnews-rename by quitting out of Gnews, then visiting your `.emacs'
file, or whatever file(s) contain your basic Gnews user-variable settings,
and then running `M-x gnews-rename'.  Also, you may need to run this while
visiting your `.gnewsrc.hook' file, if you have any non-Gnews-generated
hooks.

This message will be saved in `.gnewsrc.warn'.  Hit any key to continue.")
	     (read-char)
	     (write-region 1 (point-max) (concat gnews-rc-file ".warn"))
	     (switch-to-buffer news-buffer)))
  (if (and (boundp 'gnews-hook-kill-alist) (not hook-kill-alist))
      (setq hook-kill-alist gnews-hook-kill-alist)))

;;; I suppose a warning message is appropriate.  However, if the hook
;;; file is gone, I don't see what good this does.  And if it has been
;;; moved around, a copy will, perhaps, still be around, so for now, I
;;; do nothing.  As it is, the .hook file needs some sort of backup.

;;; Also, this currently is how a new .gnewsrc.hook is created.

;;; article headers

(defun article-header-clean (art-flag)
  "Remove unsightly headers.  Non-nil ART-FLAG means update
article-field-list."
  (goto-char 1)
  (if art-flag
      (progn
	(setq article-field-list (list nil))
	(while (not (looking-at "^[ \t]*$"))	; fix those moronic
	  (if (looking-at "^[ \t]+")		; multiline headers
	      (delete-indentation))
	  (forward-line 1))))
  (goto-char 1)					; now we do business
  (let (field value intern-field)
    (while (re-search-forward "^\\([^:]*\\): *\\(.*\\)$" (article-min) t)
      (setq field (gnews-match 1)
	    value (gnews-match 2))
      (delete-region (match-beginning 1) (1+ (match-end 2)))
      (setq value (article-header-tweak field value))
      (if (or article-header-all-p
	      (and (string< "" value)
		   (setq intern-field (intern field))
		   (if (memq intern-field article-header-ignore)
		       (memq intern-field article-header-show)
		     t)))
	  (insert field ": " value "\n"))
      (if art-flag (nconc article-field-list (list (cons field value))))))
  (goto-char 1)
  (setq article-formfeed-p t)
  (run-hooks 'article-header-hook)
  (goto-char 1))

(defun article-header-tweak (field string)
  "Given header FIELD with value STRING, return a new value for the header.\n
This can be used for fine tweaking of header displays, eg, removing
extraneous \"Re:\"s, converting GMT time to local time, etc."
  string)

(defun article-field (&rest headers)
  "As a function call, return the first non-empty field associated
with the HEADERS, eg (article-field \"Reply-To\" \"From\") returns
the Reply-To: field if present, else the From: field, or failing
even that, a null string.\n
Interactively, insert the prompted-for field.\n
Note: this function is case-sensitive."
  (interactive (list (completing-read "Header: " article-field-list)))
  (if (interactive-p)
      (insert (article-field-intern (car headers)))
    (let ((f (article-field-intern (car headers))))
      (while (and headers f (zerop (length f)))
	(setq headers (cdr headers)
	      f (article-field-intern (car headers))))
      (or f ""))))

(defun article-field-intern (header)
  "Return the field associated with HEADER, or \"\" if non-existent.
This function refers to digest headers when in the Digest minor mode.\n
Compare with article-field-raw."
  (if (gnews-digest-p)
      (article-digest-field-raw header)
    (article-field-raw header)))

(defun article-field-raw (header)
  "Return the field associated with HEADER, or \"\" if non-existent.
This always refers to the headers of the raw article, and never to the
headers within a digest.\n
Compare with article-field-intern."
  (or (cdr (assoc header article-field-list)) ""))

;;; Identifying the next newsgroup to proffer

(defun news-next-intern (flag gl)
  "Internal function for identify next newsgroup to identify.  FLAG means
proffer from all subscribed-to newsgroups when non-nil, and from those
with new news only when FLAG is nil.\n
GL is the newsgroup roster to search through, in order."
  (setq news-default-command 'news-yes)
  (let ((g (car gl)) q rst)
    (group-warn-delete-window t)
    (condition-case q
	(progn
	  (while (and g
		      (not q)
		      (not (and (setq rst (news-restrictions g))
				(gnadr g)
				(or flag
				    (and (roster-string-new-news g)
					 group-not-bogus
					 (group-new-news g))))))
	    (setq news-next-message (if flag ""
				      (concat
					(if (< (length news-next-message) 20)
					    news-next-message "")
					(if rst "." ""))))
	    (message news-next-message)
	    (setq gl (cdr gl) g (car gl)))
	  (cond (q)
		((null g) (news-end))
		(t
		 (setq news-next-message "")
		 (group-set (car g))
		 (group-proffer flag g (group-get-info g)))))
      (quit (setq news-next-message ""
		  news-default-command 'news-next-unread
		  news-prompt-return news-prompt-next)
	    (group-set (car g))
	    (gnews-message "interrupted at %s--%s ? "
			   group-current news-prompt-next))))
  (gnews-flush))

(defun roster-string-new-news (g)
  "Check if roster-string gives quick new news information about
group entry G.  Return t if it looks like new news."
  (let* ((gp (car g))
	 (i (string-match (concat "^" (regexp-quote gp)
				  " \\([0-9]+\\) \\([0-9]+\\) \\([ymn]\\)")
			  roster-string))
	 (gg (gnddr g)))
    (and (setq group-not-bogus i)
	 (setq article-final (read (substring roster-string
					      (match-beginning 1)
					      (match-end 1)))
	       article-first (read (substring roster-string
					      (match-beginning 2)
					      (match-end 2))))
	 (setq group-not-empty (< 0 article-final))
	 (if gg (< (amark-cdr (car gg)) article-final) t))))

(defvar group-0-reset-warn nil
  "*If non-nil, warn about newsgroups reset to 0 articles.")

(defun group-new-news (g &optional nowarn)
  "Return determination of existence of unread news for group entry G.
Optional second argument NOWARN non-nil means don't display any warning
messages."
  (if (nntp-exec t t "group" (car g))
      (progn
	(if (string-match (concat " " (regexp-quote (car g)) "$")
			  nntp-info)
	    nil				; we got the correct nntp-info
	  (let ((nntp-exec-force t))	; timing error???--let's retry
	    (nntp-exec t t "group" (car g))))
	(if (string= nntp-info nntp-bogus-group)
	    nil				; no new news: it's bogus
	  (let* ((cc (read-from-string nntp-info 4))
		 (ii (read-from-string nntp-info (cdr cc)))
		 (ll (read-from-string nntp-info (cdr ii)))
		 (tt (if (and (gnddr g) (gnaddr g)) (car (reverse g)) 0)))
	    (setq article-first (car ii)
		  article-final (car ll))
	    (cond ((and (not nowarn)
			(< (1+ article-final) (amark-cdr tt))
			(or (< 0 article-final) group-0-reset-warn))
		   (with-output-to-temp-buffer gnews-warn-buffer
		     (use-local-map news-mode-map)
		     (if (boundp 'group-warning-options) nil
		       (setq group-warning-options
			     (append
			       (list (substring news-prompt-next 1 2))
			       (gnews-map '(lambda (dk f)
					     (if (eq (key-binding dk) f) dk
					       (gnews-subst-command-keys
						 (format "\\[%s]"
							 (symbol-name f)))))
				 '("u" "c" "m" "D")
				 '(news-unsubscribe news-catchup
						    news-mark news-delete)))))
		     (princ
		       (concat
			 "\nWarning: " (car g) " has been ?reset to "
			 article-final " from " (amark-cdr tt)
			 "\n\nYour options include:\n\n\t"
			 (nth 0 group-warning-options)
			 " (to skip to the next group)\n\t"
			 (nth 1 group-warning-options)
			 " (to unsubscribe the problem away)\n\t"
			 (nth 2 group-warning-options)
			 " (to mark everything read)\n\t"
			 (nth 3 group-warning-options)
			 " (to mark everything unread)\n\t"
			 (nth 4 group-warning-options)
			 " (to delete the newsgroup)\n")))
		   (gnews-message "what next [%s] ? "
				  (apply 'concat group-warning-options))
		   (setq group-warn-p t)
		   (group-set (car g) t))
		  (t
		   (and (< 0 article-final)
			(or (< article-first (amark-car tt))
			    (> article-final (amark-cdr tt)))))))))))

(defun group-get-info (g &optional quick)
  "Get the basic information about newsgroup .gnewsrc entry G.  Returns a
list containing subscription bit, G's amark and the count of unread
articles.  Returns nil if G is bogus.\n
If optional second argument QUICK is non-nil, an internal NNTP call will
not be done."
  (if (or quick (nntp-exec t t "group" (car g)))
      (let* ((s (gnadr g))
	     (m (gnddr g))
	     (z (amark-list-init article-first m))
	     (c (if (zerop article-final) 0
		  (apply '- article-final 0 (mapcar 'amark-size m)))))
	(list s m c))))

(defun group-warn-delete-window (delete)
  "Delete the warning buffer if DELETE is non-nil."
  (if delete
      (progn
	(delete-windows-on (get-buffer gnews-warn-buffer))
	(setq group-warn-p))))

(defun group-bogus-warn (group)
  "Give an error message when trying to enter a bogus newsgroup."
  (message "%s: <bogus>" group (ding)))

;;; Newsgroup information getting/setting

(defun group-set (group &optional informed)
  "Set the current news GROUP.  Basic group-dependent variables are set.
The newsgroup is not actually entered.\n
Optional flag INFORMED non-nil means do not call group-new-news to set
certain basics.\n"
  (group-warn-delete-window (not group-warn-p))
  (if (string= group group-set-current) group	 ; already up-to-date
    (let ((g (assoc group group-roster)))
      (if (not informed) (group-new-news g))
      (gnews-map 'set
	'(amark amark article-count)
	(group-get-info g t))
      (setq group-bogus (string= nntp-info nntp-bogus-group))
      (if (article-exists-p article-first)
	  nil					 ; everything's OK
	(setq article-current article-first
	      article-first (article+1 t)	 ; skip over dead stuff
	      article-current article-first))
      (article-current-set (max (if (car amark)
				    (1+ (amark-cdr (car amark)))
				  0)
				article-first))
      (amark-list-init article-current amark)
      (setq article-same-subject (and article-same-subject-trigger
				      (<= article-same-subject-trigger
					  article-count))
	    amark-entry amark
	    group-previous (or group-current group-previous)
	    group-current group
	    group-checkpoint nil
	    group-mark-later-list (list nil)
	    article-digest-maybe nil
	    index-current nil
	    index-header-field nil
	    group-set-current group)
      (setcdr (cdr g) amark)
      group)))

(defun group-get (group &optional nosub dot)
  "Switch to named news GROUP.  Resubscribes unless optional second argu-
ment NOSUB is non-nil.  Goes to the first available article, or the end
of the newsgroup if the group is caught up in, unless third argument DOT
is non-nil, in which case no article setting is done."
  (setq group-current (group-set group)
	index-return-configuration (current-window-configuration))
  (if (not nosub)
      (let ((sub (cdr (assoc group group-roster))))
	(setcar sub (or (car sub) t))))
  (setq news-default-command 'news-yes)
  (if group-bogus
      (group-bogus-warn group)
    (group-mode)
    (hook-kill-set group)
    (setq group-pattern ""
	  group-pattern-field "Subject"
	  group-pattern-command nil
	  group-entry-command (if (rassq this-command gnews-abbrev-keymap)
				  group-entry-command this-command)
	  group-pattern-redo nil
	  group-last-save (concat gnews-news-dir
				  (if (eq gnews-slashify t)
				      (gnews-replace "\\." "/" group-current)
				    group-current)
				  (if gnews-slashify "/")))
    (if hook-kill-pre-ok			; a locking mechanism
	(progn
	  (setq index-perm-marks (list nil))
	  (mapcar 'hook-kill-do hook-kill-pre)
	  (setq hook-kill-pre-ok nil)))
    (if (or dot (boundp 'index-pop)) nil
      (while (and (<= article-current article-final)
		  (not (article-exists-p article-current)))
	(amark-list-insert article-current amark)
	(setq article-current (1+ article-current)))
      (if (<= article-current article-final)
	  (cond ((catch 'article-nil (article-get
				       article-current hook-kill-per t))
		 (article-junk-local)
		 (group-next-unread t)))
	(group-last)))))

(defun group-proffer (flag g gi)
  "Ask if user wishes to read newsgroup, requiring new news if FLAG is nil.
Returns with list of interesting data if yes.  G is list of newsgroup info
from the .gnewsrc file, and GI is the list of info from group-get-info."
  (if (or flag (< 0 (nth 2 gi)))
      (if (string= nntp-info nntp-bogus-group)
	  (gnews-message "%s <bogus> %s ? "
	    (setq group-bogus t
		  group-current (car g))
	    (setq news-prompt-return news-prompt-next))
	(gnews-message "%s {%d} %s ? "
	  (setq group-bogus nil
		news-seen t
		group-current (car g))
	  (setq group-proffer-count (nth 2 gi))
	  (setq news-prompt-return news-prompt-yes)))))

(defun group-proffer-must (group &optional gn)
  "Proffer to read news from GROUP.  Optional second argument GN is the
.gnewsrc entry for GROUP."
  (if (not gn) (setq gn (assoc group group-roster)))
  (group-set group)
  (group-proffer t gn (group-get-info gn)))

(defun group-proffer-new (group)
  "Ask if user wishes to read newsgroup GROUP not in .gnewsrc."
  (let* ((f (group-name-read
	      (format "place %s after: " group) group-roster 'news-all))
	 (g (assoc f group-roster))
	 (n (list group t))
	 (hd (reverse (memq g (reverse group-roster))))
	 (tl (cdr (memq g group-roster))))
    (setq news-seen t
	  group-roster (nconc hd (list n) tl))
    (group-roster-write gnews-rc-file-new)
    (group-get group)))

(defun gnews-mod-p (group)
  "Return non-nil if GROUP is moderated."
  (if (string-match (concat "^" group "[ \t]+[0-9]+[ \t]+[0-9]+[ \t]+"
			    "\\([ymn]\\)")
		    roster-string)
      (string-match (substring roster-string
			       (match-beginning 1)
			       (match-end 1))
		    "mn")))

;;; newsgroup exiting internals

(defun group-amark-set (group om)
  "Set GROUP's amark to AMARK."
  (setcdr (cdr (assoc group group-roster)) (setq amark om)))

(defun group-quit-intern ()
  "The essential internals of newsgroup quitting."
  (if (cdr group-mark-later-list)	; things to mark for later
      (progn
	(group-mark (cdr group-mark-later-list))
	(group-amark-set group-current amark)
	(setq group-mark-later-list (list nil))))
  (setcdr (cdr (assoc group-current group-roster)) amark)
  (mapcar 'hook-kill-do hook-kill-post)
  (setq hook-kill-post nil
	hook-kill-pre-ok t)		; unlock the pre-hooks for re-use
  (if (boundp 'index-pop)
      (progn				; hide knowledge of the index buffer
	(set-window-configuration index-return-configuration)
	(makunbound 'index-pop)))
  (if group-checkpoint (group-roster-write gnews-rc-file-new)))

(defun news-next-unread-maybe (&optional quit)
  "Run 'news-next-unread if the current newsgroup was entered with
one of the usual entry commands appropriate for .gnewsrc in-sequence
profferings; otherwise merely print the quit to top level prompt.\n
If optional argument QUIT is non-nil, run a group-quit first."
  (if quit (group-quit))
  (cond ((memq group-entry-command
	       '(gnews news-yes news-default news-next-unread news-index
		       news-catchup))
	 (news-next-unread))
	((memq group-entry-command
	       '(news-previous-unread))
	 (news-previous-unread))
	(t
	 (setq news-default-command 'news-next-unread)
	 (message "quit to top level--what next %s ? " news-prompt-next))))

(defun group-next-but-index-done (forward)
  "What to do when the index-buffer is filled and you go to the next
article in group-mode.  Argument FORWARD non-nil if going forward,
nil if going backward."
  (if (and (= (length amark) 1)
	   (= (amark-cdr (car amark)) article-final))
      (news-next-unread-maybe t)
    (save-excursion
      (set-buffer index-buffer)
      (goto-char (if forward 1 (point-max)))
      (if (funcall (if forward 're-search-forward 're-search-backward)
		   "^ *[0-9]+\\([ i]\\)" nil t)
	  (if (string= " " (gnews-match 1))
	      (cond ((catch 'article-nil (article-get (index-article) nil t))
		     (article-junk-local)
		     (group-next-but-index-done forward)))
	    (group-catchup nil 1))
	(news-next-unread-maybe t)))))

(defun group-next-but-no-wrap ()
  "What to do when no wrapping is wanted at the $-end of a newsgroup."
  (if (and (= (length amark) 1)
	   (= (amark-cdr (car amark)) article-final))
      (news-next-unread-maybe t)
    (group-catchup nil 1)))

;;; initial article display

(defun article-display-init (&optional dotsokay rot13)
  "Display the article sitting in nntp-buffer.  If optional argument
DOTSOKAY is non-nil, don't play games with leading periods."
  (let (p q)
    (setq article-grab-point-old nil)
    (if (null article-grab-point)
	(progn (set-buffer nntp-buffer)
	       (setq article-grab-point (point-max))))
    (if article-subject-hilite
	(progn (set-buffer nntp-buffer)
	       (goto-char 1)
	       (re-search-forward "^Subject: ")
	       (setq p (match-end 0) q (gnews-eol))))
    (if article-formfeed
	(progn (goto-char 1)
	       (forward-paragraph 1)
	       (if (re-search-forward article-formfeed article-grab-point t)
		   (setq article-grab-point (match-end 0)))))
    (switch-to-buffer news-buffer)
    (setq buffer-read-only)
    (erase-buffer)
    (if article-subject-hilite
	(progn
	  (insert-buffer-substring nntp-buffer 1 p)
	  (setq buffer-read-only t)	; no flashing "*"s
	  (sit-for 0)
	  (setq inverse-video (not inverse-video)
		buffer-read-only)
	  (insert-buffer-substring nntp-buffer p q)
	  (setq buffer-read-only t)	; no flashing "*"s
	  (sit-for 0)
	  (setq inverse-video (not inverse-video)
		buffer-read-only)
	  (insert-buffer-substring nntp-buffer q article-grab-point))
      (insert-buffer-substring nntp-buffer 1 article-grab-point))
    (goto-char 1)
    (setq article-count-off-but-ok nil)
    (if dotsokay nil
      (setq article-grab-point (- article-grab-point (nntp-undot))))
    (setq buffer-read-only t)
    (goto-char 1)
    (set-buffer-modified-p nil)
    (if gnews-rot13-p
	(let ((min (article-min)) buffer-read-only)
	  (save-excursion
	    (goto-char min)
	    (gnews-rot13 min article-grab-point))))
    (message "")
    (article-%-clear)
    (let (article-subject-hilite)
      (article-mode))
    (article-%-compute)))

(defun article-effective-init-display ()
  "Set the effective article-display-count to nil if the current article
has article-big lines are more.\n
If non-nil, also make sure that the effective article-display-count is at
least two more the number of displayed headers."
  (let ((l (string-to-int (article-field "Lines"))))
    (cond ((= 0 l) nil)
	   (article-display-count
	    (if (< l article-big)
		(max article-display-count
		     (+ 2 (save-excursion
			    (goto-char 1)
			    (forward-paragraph 1)
			    (count-lines 1 (point))))))))))

;;; basic article predicates

(defun article-min ()
  "Return smallest value of point in the body of the article."
  (save-excursion
    (goto-char 1)
    (forward-paragraph)
    (forward-line 1)
    (point)))

(defun article-max ()
  "Return largest value of point in the body of the article."
  (point-max))

(defun article-done ()
  "Returns t if the end of the article is visible, nil otherwise"
  (and (<= article-current article-final)
       (or (null article-%)
	   article-count-off-but-ok
	   (< 99 (read article-%)))
       (< 0 article-current)))

(defun article-forward-intern (junk &optional ff digest)
  "Get more of the article--if JUNK non-nil, mark as junkable.\n
If optional second argument FF is non-nil, it is treated as a
formfeed regexp, blocking display and scrolling at the next
formfeed, if present.  If FF is nil, the rest of the article
will be filled in.\n
If FF is a string and the third argument DIGEST is a string,
then always grab to the next formfeed even if it is not visible,
and use the string as a regexp that matches text after FF, to
confirm that FF was a genuine digest separator."
  (setq article-junkable (or article-junkable junk))
  (if (and article-grab-point
	   (or (not ff)
	       digest
	       (pos-visible-in-window-p article-grab-point)))
      (save-excursion
	(if article-grab-point
	    (setq article-grab-point-old article-grab-point))
	(setq buffer-read-only)
        (set-buffer nntp-buffer)
        (let* ((pmax (point-max))
	       (pm (if (and ff (not digest))
		       (save-excursion
			 (goto-char article-grab-point)
			 (forward-line (screen-height))
			 (point))
		     pmax))
	       (start (min article-grab-point pm))
	       (rsf (goto-char start))
	       (rsfp nil)
	       (end (if (and ff
			     (setq rsf (re-search-forward ff nil t))
			     (setq rsfp (point))
			     (or (not digest)
				 (while (not (or (eobp) (looking-at digest)))
				   (forward-line 1)
				   (setq rsf (re-search-forward ff nil t)
					 rsfp (point)))
				 rsf))
			rsfp pm))
	       string)
	  (setq article-formfeed-p (or article-formfeed-p rsf))
	  (set-buffer news-buffer)
	  (if (< start end)
	      (save-excursion
		(goto-char (setq news-start (point-max)))
		(if (not (and article-formfeed-p gnews-rot13-p))
		    (insert-buffer-substring nntp-buffer start end)
		  (set-buffer nntp-buffer)
		  (setq string (buffer-substring start end))
		  (set-buffer news-buffer)
		  (gnews-rot13-insert string))
		(setq article-grab-point (if ff end))))
	  (if (= end pmax) (setq article-grab-point)))
	(setq buffer-read-only t)
	(article-%-compute))))

(defun article-exists-p (j)
  "Return t if article J exists in the current newsgroup, and reset
the internal NNTP article number to J if it does."
  (nntp-exec t t "stat" j))

(defun article+1 (&optional raw)
  "Return the next available article number.  If it is necessary to skip
over non-existent articles, then also close up dead space in the current
amark.\n
If the optional argument RAW is non-nil, the amark will be ignored
when identifying the next available article."
  (let (i j sm0 nen)
    (if (and (< article-current article-final)
	     (or (setq nen (nntp-exec t t "next"))
		 (setq sm0 (string-match "<0>" nntp-info)))
	     (string< "" nntp-info))
	(progn
	  (setq i (car (read-from-string nntp-info 4))
		j (if raw i (amark-next-unread (1- i) amark)))
	  (amark-block-mark article-current i amark)
	  (cond (raw (if (or (not nen) sm0) (article+1 t) i))
		((< article-final j) j)
		((article-exists-p j) j)
		(t (article+1))))
      (1+ article-final))))
  
(defun article-1 (&optional raw)
  "Return the previous available article number.  If it is necessary to skip
over non-existent articles, then also close up dead space in the current
amark.\n
If optional argument RAW is non-nil, the amark will be ignored when
identifying the previous available article."
  (let (i j sm0 nel)
    (if (and (or (setq nel (nntp-exec t t "last"))
		 (setq sm0 (string-match "<0>" nntp-info)))
	     (string< "" nntp-info))
	(progn
	  (setq i (car (read-from-string nntp-info 4))
		j (if raw i (amark-previous-unread (1+ i) amark)))
	  (amark-block-mark article-current i amark)
	  (cond (raw (if (or (not nel) sm0) (article-1 t) i))
		((< j article-first) j)
		((article-exists-p j) j)
		(t (article-1))))
      article-first)))			; this is wrong, but what isn't ?

(defun article-current-set (art)
  "Set the current article number to ART."
  (setq article-previous article-current
	article-current art))

;;; newsgroup roster generation

(defun roster-all ()
  "Return list of all newsgroups known to this site."
  (or roster
      (gnews-string-as-buffer roster-string nil
	(setq roster (list nil))
	(while (not (eobp))
	  (let* ((info (buffer-substring (gnews-bol) (gnews-eol)))
		 (sp (string-match " " info))
		 (gg (substring info 0 sp))
		 (ff (read-from-string info sp))
		 (ii (read-from-string info (cdr ff))))
	    (nconc roster (list (list gg (car ii) (car ff)))))
	  (forward-line 1))
	(setq roster (cdr roster)))))

(defun roster-string-set ()
  (save-excursion
    (nntp-exec t t "list")
    (set-buffer nntp-buffer)
    (setq roster-string (buffer-string))))

(defun roster-new-set (msg-y msg-n urgent)
  "Get the new groups waiting in the nntp-buffer, and append to
roster-new.  If new groups are found, display message MSG-Y (if
non-nil).  A %d, if present, is formatted as the number of new
newsgroups.  If no new groups are found, display message MSG-N
\(if non-nil\).  If third argument URGENT is non-nil, then the
yes message will be accompanied by a ding; moreover, the yes
message will be generated if there are still \"old\" new groups
that the user hasn't considered yet."
  (set-buffer nntp-buffer)
  (goto-char 1)
  (if (looking-at "^2") (forward-line 1))
  (if (eobp)
      (cond ((and urgent (cdr roster-new))
	     (ding)
	     (if msg-y (message msg-y (1- (length roster-new)))))
	    (msg-n
	     (message msg-n)))
    (while (not (eobp))
      (let ((gn (buffer-substring (gnews-bol) (gnews-eol))))
	(if (and (not (assoc gn roster-new))
		 (not (assoc gn group-roster)))
	    (nconc roster-new (list (list gn nil)))))
      (forward-line 1))
    (if urgent (ding))
    (if msg-y (message msg-y (1- (length roster-new))))))

;;; rot13 function

(setq gnews-rot13-map (make-keymap))

(let ((i 0))
  (while (< i 128)
    (aset gnews-rot13-map i (cond ((< i ?A) i)
				  ((< i ?N) (+ i 13))
				  ((<= i ?Z) (- i 13))
				  ((< i ?a) i)
				  ((< i ?n) (+ i 13))
				  ((<= i ?z) (- i 13))
				  (t i)))
    (setq i (1+ i))))

(defun gnews-rot13-insert (str)
  "Insert the rot13 version of STR."
  (let ((i 0) (m (length str)))
    (while (< i m)
      (insert (aref gnews-rot13-map (aref str i)))
      (setq i (1+ i)))))

(defun gnews-rot13-string (str)
  "Return the rot13 version of STR."
  (mapconcat 'char-to-string
	     (mapcar '(lambda (c) (aref gnews-rot13-map c)) str)
	     ""))

(defun gnews-rot13 (beg end &optional adjust)
  "Caesar alphabet shift the region by 13, better known as \"rot13\".\n
In Lisp code, the region is given by arguments BEG and END."
  ;; Bug: what if beg is before the currently visible portion?
  (interactive "*r")
  (save-excursion
    (if (pos-visible-in-window-p end)
	(let ((str (buffer-substring beg end)))
	  (delete-region beg end)
	  (gnews-rot13-insert str))
      (let* ((mid (save-excursion
		    (move-to-window-line -1) (end-of-line) (point)))
	     (vis (buffer-substring beg mid))	; visible portion of region
	     (inv (buffer-substring mid end)))	; invisible portion of region
	(delete-region beg end)
	(gnews-rot13-insert vis)
	(sit-for 0)				; rot13 flash for the reader
	(gnews-rot13-insert inv))))
  (if adjust (recenter article-formfeed-top)))

;;; history mechanism

(defvar article-history nil)

(defun article-history-append (art gp msgid)
  (if (and article-history (string= msgid (gnddr (car article-history))))
      nil
    (setq article-history (cons (cons art (cons gp msgid))
				article-history))))

;;; Spool.el--substitute functions for reading off a news spool
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

;;; Extensively revised/fixed/tested by Hal R Peterson (hrp@hall.cray.com)

(provide 'gnews-spool)

;;; First, the basics get redefined.  This must be loaded *after* Init.el

(fset 'nntp-start 'gnews-spool-start)
(fset 'nntp-exec 'gnews-spool-exec)
(fset 'article-get 'article-get-slow)
(fset 'nntp-index-exec 'gnews-spool-index-exec)
(fset 'nntp-index-start (function (lambda () t)))
(fset 'nntp-run-p (function (lambda () nil)))

;;; This won't work with fast indexing, of course, which violates the
;;; rule of filtering every NNTP command through a single function.
;;; A fast indexing for spool code is in the works.

;@@ IMPORTANT REMARK: I am unable to fully test this code.  Caveat user.
;@@ Possible site dependencies are indicated below with ;@@ comments.

;;; gnews-spool-* preliminaries

(defvar n-reply-allowed t
  "*Non-nil if posting is permitted.")

(defvar gnews-server-article nil
  "Current article in pseudo-server.")
(defvar gnews-server-group nil
  "Current group in pseudo-server.  Not yet in use.")

(defvar gnews-spool-active-file "/usr/lib/news/active")
(defvar gnews-spool-history-file "/usr/lib/news/history")
(defvar gnews-spool-newsdir "/usr/spool/news/")

(defun gnews-spool-info (&rest args)
  "Set the nntp-info variable."
  (setq nntp-info (mapconcat 'identity args " ")))

(defun gnews-spool-dir (gp)
  "Return the directory where newsgroup GP's articles are found."
  (concat gnews-spool-newsdir (gnews-replace "\\." "/" gp) "/"))

(defun gnews-spool-art (gp art)
  "Return the name of the file containing newsgroup GP, article # ART."
  (concat gnews-spool-newsdir (gnews-replace "\\." "/" gp) "/" art))

;@@ The regexp for matching the date and time may be different on your site.
;@@ For example, maybe "[ \t]+../../..[ \t]+..:..[ \t]+" is what you need.

(defun gnews-spool-regexp (msg-id)
  "Return the regexp that matches MSG-ID in the history file, and also
brackets off the newsgroup and article number as match #1 and match #2."
  (concat "^" (regexp-quote msg-id)  	  ; Message-ID
	  "[ \t]../../..[ \t]..:..[ \t]"  ; date/time.
	  "\\([^/]*\\)/\\([0-9]*\\) "))	  ; group/###

;;; gnews-spool-* commands and internals

(defun gnews-spool-start (msg)
  "Initialize gnews-spool buffers."
  (if msg (message "%sreading news spool from %s..." msg gnews-spool-machine))
  (setq gnews-spool-active (find-file-noselect gnews-spool-active-file))
  (gnews-spool-info (if n-reply-allowed "200" "201") news-path)
  (fset 'nntp-run-p (function (lambda () t)))
  (if msg (message "%sreading news spool from %s...done"
		   msg gnews-spool-machine)))

(defun gnews-spool-exec (clear finish comm &rest args)
  "NNTP commands interpreted directly off a news spool."
  (interactive
    (list (not current-prefix-arg)
	  (read-from-minibuffer "NNTP command: ")
	  nil))
  (if (interactive-p)
      (setq args (progn
		   (string-match "\\<[^ ]*\\>" comm)
		   (if (/= (length comm) (match-end 0))
		       (list (substring comm (1+ (match-end 0))))))
	    comm (substring comm (match-beginning 0) (match-end 0)))
    (if (stringp clear) (error "Uh, you forgot the clear flag, eh?")))
  (if clear (nntp-clear nntp-buffer))
  (let* ((b (current-buffer))
	 (a1prime (car args))
	 (a1 (concat a1prime))
	 (a2 (concat (gnadr args)))
	 (art (if a1prime
		  a1
		(or gnews-server-article "-1"))))
    (prog2
	(set-buffer nntp-buffer)
	(cond ((string= comm "group")
	       (gnews-spool-exec-group a1))
	      ((string= comm "article")
	       (gnews-spool-exec-art art 'art))
	      ((string= comm "head")
	       (gnews-spool-exec-art art 'head))
	      ((string= comm "body")
	       (gnews-spool-exec-art art 'body))
	      ((string= comm "stat")
	       (gnews-spool-exec-art art 'stat))
	      ((string= comm "next")
	       (gnews-spool-exec-motion t))
	      ((string= comm "last")
	       (gnews-spool-exec-motion nil))
	      ((string= comm "list")
	       (gnews-spool-exec-list))
	      ((string= comm "newgroups")
	       (gnews-spool-exec-newgroups (concat a1 a2)))
	      ((string= comm "help")
	       (gnews-spool-exec-help))
	      ((string= comm "quit")
	       (gnews-spool-exec-quit)))
      (set-buffer b))))

(defun gnews-spool-exec-group (gp)
  "Fake an NNTP group command."
  (let ((dir (gnews-spool-dir gp)) c f l)
    (if (and (file-readable-p dir)
	     (car (file-attributes dir)))
	(gnews-string-as-buffer "" nil
	  (setq gnews-server-article nil)
	  (call-process "ls" nil t nil dir)
	  (goto-char 1)
	  (insert "(setq gnews-spool-group-list (gnews-spool-preen '(")
	  (goto-char (point-max))
	  (insert ")))")
	  (eval-current-buffer)
	  (if (null gnews-spool-group-list)
	      (gnews-spool-info "211 0 0 0" gp)	; nothing there
	    (sort gnews-spool-group-list '<)
	    (setq gnews-spool-group-tsil (reverse gnews-spool-group-list))
	    (setq c (length gnews-spool-group-list))
	    (setq f (car gnews-spool-group-list))
	    (setq l (car gnews-spool-group-tsil))
	    (gnews-spool-info "211" c f l gp)
	    t))
      (gnews-spool-info "411 Invalid group name.")
      nil)))

(defun gnews-spool-preen (grouplist)
  "Remove all subgroups from GROUPLIST, a list of articles in a group."
  ;; First remove the leading subgroups.
  (while (and grouplist (not (integerp (car grouplist))))
    (setq grouplist (cdr grouplist)))
  ;; Now remove the embedded and trailing subgroups.
  (let ((preened-list grouplist))
    (while (cdr grouplist)
      (if (not (integerp (gnadr grouplist)))
	  (setcdr grouplist (gnddr grouplist))
	(setq grouplist (cdr grouplist))))
    preened-list))

(defvar gnews-spool-history-lookup-prog "grep"
  "External program to run when looking up a Message-ID.")

(defun gnews-spool-history-lookup-args (msg-id)
  "List of arguments to pass to gnews-spool-history-lookup-prog."
  (list (regexp-quote msg-id) gnews-spool-history-file))

(defun gnews-spool-exec-art (art-no part)
  "Fake an NNTP article/head/body/stat command."
  (let (file msg-id)
    (if (and (cond ((string-match "^[0-9]+$" art-no)
		    (setq file (gnews-spool-art group-current art-no))
		    (if (let ((attributes (file-attributes file)))
			  (and attributes (< 0 (nth 7 attributes))))
			(if (memq part '(body stat))
			    ;; Set the Message-ID by hand
			    (setq msg-id (gnews-string-as-buffer "" nil
					   (call-process "sed" file t t
							 "/^Message-ID:/q")
					   (forward-line -1)
					   (forward-char 12)
					   (buffer-substring
					    (point) (gnews-eol))))
			  t)))
		   ((string-match "^<.*>$" art-no)
		    (gnews-string-as-buffer "" nil
		      (apply 'call-process
			     gnews-spool-history-lookup-prog nil t nil 
			     (gnews-spool-history-lookup-args art-no))
		      (beginning-of-buffer)
		      (if (re-search-forward (gnews-spool-regexp msg-id)
					     nil t)
			  (setq art-no (gnews-match 2)
				file (gnews-spool-art (gnews-match 1) art-no))
			(setq art-no "0"
			      file "/meese/sucks/raw/eggs/film/at/11")))
		    (set-buffer nntp-buffer)))
	     (file-readable-p file))
	(progn
	  (setq gnews-server-article art-no)
	  (cond ((eq part 'art)
		 (insert-file file))
		((eq part 'head)
		 (call-process "sed" file t t "/^$/q")
		 (goto-char (point-max))
		 (delete-char -1))
		((eq part 'body)
		 (call-process "sed" file t t "1,/^$/d")))
	  (gnews-spool-info (cond ((eq part 'art) "220")
				  ((eq part 'head) "221")
				  ((eq part 'body) "222")
				  ((eq part 'stat) "223"))
			    art-no
			    msg-id
			    "Article retrieved;"
			    (cond ((eq part 'art) "head and body follow.")
				  ((eq part 'head) "head follows.")
				  ((eq part 'stat) "request text separately.")
				  ((eq part 'body) "body follows.")))
	  t)
      (gnews-spool-info "423 Invalid article number:" art)
      nil)))

;;; ugly.  There must be a cleaner way.

(defun gnews-spool-exec-motion (pfx)
  "Fake an NNTP next/last command."
  (let* ((art-no (and gnews-server-article
		      (car (read-from-string gnews-server-article))))
	 (art-list (cdr (memq art-no
			      (if pfx
				  gnews-spool-group-list
				gnews-spool-group-tsil))))
	 (next-art-no (car art-list))
	 (next-art (concat next-art-no))
	 (result (if (null art-list) nil
		   (gnews-spool-exec-art next-art 'stat))))
    (while (and (not (null art-list))
		(null result))
      (setq art-list (cdr art-list)
	    next-art (concat (car art-list))
	    result (if (null art-list)
		       nil
		     (gnews-spool-exec-art next-art 'stat))))
    (if (null art-list)
	(gnews-spool-info "421 No "
			  (if pfx "next" "previous")
			  " article to retrieve")
      (setq gnews-server-article next-art))
    result))

(defun gnews-spool-exec-list ()
  "Fake an NNTP list command."
  (insert-file gnews-spool-active-file)
  (gnews-spool-info "215 Newsgroups in form \"group high low y/n\".")
  t)

(defun gnews-spool-exec-newgroups (ymd hms &optional gmt) t)

(defun gnews-spool-exec-help ()
  "Waste time creatively."
  (if (boundp 'gnews-meese-is-a-bowbity-bowb-bowb) (load "meese.el"))
  (gnews-spool-info "100 This server accepts the following commands:")
  t)

(defun gnews-spool-exec-quit ()
  "Fake an NNTP quit command."
  (fset 'nntp-run-p '(lambda () nil))
  (setq gnews-server-article nil
	gnews-server-group nil)
  (gnews-spool-info "205" news-path "closing connection.  Goodbye.")
  t)

;;; from jr@bbn.com (John Robinson):

(defun gnews-spool-index-exec (comm &rest args)
  "NNTP commands for indexing interpreted directly off a news spool."
  (interactive
    (list (not current-prefix-arg)
	  (read-from-minibuffer "NNTP command: ")
	  nil))
  (if (interactive-p)
      (setq args (progn
		   (string-match "\\<[^ ]*\\>" comm)
		   (if (/= (length comm) (match-end 0))
		       (list (substring comm (1+ (match-end 0))))))
	    comm (substring comm (match-beginning 0) (match-end 0))))
  (let* ((b (current-buffer))
	 (a1prime (car args))
	 (a1 (concat a1prime))
	 (a2 (gnadr args))
	 (art (if a1prime a1 (or gnews-server-article "-1"))))
    (prog2
	(set-buffer nntp-buffer)
	(cond ((string= comm "group")
	       (gnews-spool-exec-group a1))
	      ((string= comm "head")
	       (gnews-spool-exec-art art 'head)))
      (set-buffer b))))


;;; fast indexing: set news-index-fast to t.

(fset 'news-index-fast 'gnews-spool-index-fast)

(defun gnews-spool-index-fast (pfx &optional nosub in-group)
  "Display an index of the proffered newsgroup."
  (interactive "P")
  (setq index-pop index-pop-up
	nntp-index-done nil)
  (or in-group (news-goto group-current nosub))
  (set-buffer nntp-index-buffer)
  (erase-buffer)
  (gnews-buffer index-pop index-buffer)
  (setq buffer-read-only)
  (erase-buffer)
  (setq buffer-read-only t)
  (message "indexing...")
  (nntp-exec t t "group" group-current)
  (setq nntp-index-final (if (amark-member article-final amark)
			     (amark-previous-unread article-final amark)
			   article-final)
	gnews-spool-index-files (list "-")
	gnews-s-i-f gnews-spool-index-files)
  (amark-loop art-no (list (cons article-current article-final))
    (if (and (or index-show-kills (not (amark-member art-no amark)))
	     (memq art-no gnews-spool-group-list))
	(progn
	  (setcdr gnews-s-i-f (list (concat (gnews-spool-dir group-current)
					    art-no)))
	  (setq gnews-s-i-f (cdr gnews-s-i-f)))))
  (setq nntp-index
	(start-process "gnews-spool-index"
		       nntp-index-buffer
		       "/bin/sh"
		       "-c"
		       (concat "for i in "
			       (mapconcat 'identity
					  gnews-spool-index-files
					  " ")
			       ";do echo :${i}:"
			       ";sed -n \"1,/^$/p\" $i"
			       ";done")))
  (set-process-filter nntp-index 'gnews-spool-index-filter)
  (index-mode)
  (setq index-x-menu nil)
  (if index-sort-do (index-sort))
  (setq buffer-read-only)
  (goto-char 1)
  (mapcar '(lambda (x) (insert (format "%5dm %s\n" (car x) (cdr x))))
	  (cdr index-perm-marks))
  (setq buffer-read-only t)
  (setq index-final article-current)
  (article-current-set index-final))

(defun gnews-spool-index-filter (proc string)
  "Filter for fast spool indexing." 
  (set-buffer nntp-index-buffer)
  (setq article-field-list (list nil)
	nntp-index-done nil)
  (goto-char (point-max))
  (insert string)
  (goto-char 1)
  (let* ((hook-kill-continue t)
	 (hook hook-kill-per)
	 (h (mapcar 'ignore index-headers))
	 (rgxp (concat "^:" (gnews-spool-dir group-current) "\\([0-9]+\\):"))
	 p q n i f g z junk)
    (while (and (not nntp-index-done)
		(re-search-forward rgxp nil t)
		(setq p (gnews-bol)
		      n (read (buffer-substring
				(match-beginning 1) (match-end 1))))
		(re-search-forward "^$" nil t)
		(not (eobp))
		(setq q (gnews-eol)))
      (setq i index-headers z h)
      (while z				; h gets the headers
	(goto-char p)
	(setcar z (if (re-search-forward
			(concat "^" (car i) ": *\\(.*\\)") q t)
		      (buffer-substring
			(match-beginning 1) (match-end 1))
		    ""))
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
	    (if (looking-at "^\\([^:]*\\): *\\(.*\\)$")
		(progn (setq f (buffer-substring
				 (match-beginning 1) (match-end 1))
			     g (buffer-substring
				 (match-beginning 2) (match-end 2)))
		       (if (setq z (assoc f article-field-list))
			   (setcdr z g)
			 (nconc article-field-list (list (cons f g))))))
	    (forward-line 1))))
      (while (and hook hook-kill-continue (not junk))
	(setq junk (hook-kill-do (car hook) t)
	      hook (cdr hook)))
      (delete-region p q)
      (if (and junk (not index-show-kills))
	  (if (setq nntp-index-done (= n nntp-index-final))
	      (save-excursion
		(set-buffer index-buffer)
		(setq buffer-read-only)
		(goto-char (point-max))
		(if (not (bobp)) (delete-char -1))
		(setq buffer-read-only t)
		(index-beginning-of-buffer)
		(let (debug-on-error) (error "indexing...done"))))
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
	  (if nntp-index-done
	      (let (debug-on-error)
		(index-beginning-of-buffer)
		(error "indexing...done")))
	  (set-buffer nntp-index-buffer))))))


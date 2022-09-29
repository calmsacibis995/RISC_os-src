;;; index.el: index-mode commands for Gnews
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

;;; If it isn't obvious, the first four index control variables below
;;; are to be read in parallel, ie, the formats etc correspond in order
;;; to that of index-headers.

(defvar index-headers '("Subject" "From")
  "*Header fields worth searching for quickly.  The first one should
normally be \"Subject\", as that is the field subject-search mode keys
on by default.")
(defvar index-format "%s  %s"
  "*Format for displaying index.")
(defvar index-sizes '(40 29)
  "*Maximum size when displaying--nil means no limit, 0 means no display.")
(defvar index-filter '(identity reply-domain)
  "*Filters to prettify subject fields before displaying.")

(defvar index-pop-up nil
  "*If non-nil, pop into index-buffer, if nil, switch into it")
(defvar index-junk-do nil
  "*If non-nil, remove junked headers automatically, if nil, label them
with a \"j\"")
(defvar index-sort-do nil "*If non-nil, sort index-buffer on entry")
(defvar index-junkers "jk"
  "*A string of which letters indicate junked articles in index-mode.")
(defvar index-kill-auto nil
  "*Non-nil means kill the current subject if an index-kill-* command is
invoked.  Nil means query first.")

(defvar index-display-count nil
  "*If non-nil, it is the number of lines to display between \"More ?\"s.")
(defvar index-display-all nil
  "*If nil, fill in only one window's worth [or the amount given by
index-display-count] of indexing upon entry.")
(defvar index-show-kills t
  "*If non-nil, junked and killed articles are showed (\"j\"-ed and \"k\"-ed),
upon entry into a new index.")

(defvar index-ding t
  "*If t, ding when finished fast indexing.  If a number, ding when finished
fast indexing if the number of articles is greater than this number.")

(defvar index-search-string "")


;;; index-mode

(if index-mode-map nil
  (setq index-mode-map (make-sparse-keymap))
  (gnews-key-bind index-mode-map
		  '(("f".index-buffer-switch)
		    ("o".index-other-window)
		    ("=".index-article-get)
		    ("n".index-forward)
		    ("p".index-backward)
		    ("N".index-forward-all)
		    ("P".index-backward-all)
		    (" ".index-scroll-up)
		    ("\^?".index-scroll-down)
		    ("?".index-search-backward)
		    ("/".index-search-forward)
		    ("i".index-ignore)
		    ("j".index-junk)
		    ("J".index-junk-local)
		    ("\ej".index-junk-region)
		    ("k".index-kill)
		    ("K".index-kill-permanent)
		    ("m".index-mark)
		    ("\em".index-mark-region)
		    ("c".index-catchup)
		    ("u".index-unsubscribe)
		    ("q".index-quit)
		    ("S".index-sort)
		    ("[".index-expand-backward)		; do!
		    ("]".index-expand-forward)
		    ("{".index-expand-backward-all)	; do!
		    ("}".index-expand-forward-all)
		    ("U".index-undo)
		    ("x".index-junkers-erase)
		    ("s".index-save)			; do!
		    ("a".universal-argument)
		    ("<".index-beginning-of-buffer)
		    (">".index-end-of-buffer)
		    ("\^w".gnews-bug-report)
		    ("!".shell)
		    ("h".describe-mode)
		    ("H".gnews-describe-mode)))
  (mapcar '(lambda (x)
	     (define-key index-mode-map (concat x) 'digit-argument))
	  '(0 1 2 3 4 5 6 7 8 9 "-")))

(defun index-mode ()
  "Index-mode is used by Gnews to view indices to newsgroups.\n
Commands are:
\\{index-mode-map}.
The index display can be fine-tuned by the following variables:
index-headers, index-format, index-sizes, index-filter.  The first
variable lists which headers to display, the second the formats to use,
the third gives a maximum column size (nil == no max), and the last is
a filter to run each header through before displaying.\n
See also the variables index-pop-up, index-junk-do, index-sort-do,
index-show-kills, and the function index-sort-function."
  (interactive)
  (setq major-mode 'index-mode
	mode-name "Index"
	gnews-mode-string (concat group-current " =/" article-final)
	gnews-read-p nil
	gnews-hook-p nil
	gnews-rot13-p nil
	gnews-digest-p nil)
  (use-local-map index-mode-map)
  (gnews-set-mode-line)
  (run-hooks 'index-hook))

;;; index support functions

(defun index-prepare (n fields)
  "With article N and list of article FIELDS (a b c ...), return the
list (N N's_field_a N's_field_b N's_field_c ...).  Also, update the
variable index-current."
;;; ugliness in implementation--assumes second argument is constant--hence
;;; it is redundant and should use a global variable--but that is kind of
;;; ugly too
  (or (assq n index-current)
      (save-excursion
	(set-buffer nntp-buffer)
	(let ((i (append (list n)
			 (if (nntp-exec t t "head" n)
			     (progn
			       (article-header-clean t)
			       (mapcar '(lambda (f)
					  (save-excursion
					    (goto-char 1)
					    (re-search-forward
					      (concat "^"
						      (regexp-quote f)
						      ": *\\(.*\\)$")
					      nil t)
					    (gnews-match 1)))
				       fields))
			   (make-list (length fields) ""))))) ; null fields
	  (setq index-current (append (list i) index-current))
	  i))))

(defun index-line (art-no fmt headers filter sizes)
  "Return the string to display in the index for article ART-NO, using
format FMT, mapping the list of HEADERS through the list of FILTER fun-
ctions, truncating each item to the list of SIZES."
  (apply 'format fmt
	 (gnews-map (function
		      (lambda (f s h)
			(if s
			    (substring (concat
					 (funcall f h) (make-string s ? ))
				       0 s)
			  (funcall f h))))
	   filter sizes headers)))

;;; index commands

(defvar news-index-fast t
  "*Use the fast indexing code.")

(defun news-index (pfx &optional nosub in-group)
  "Display an index of the proffered newsgroup.  With a prefix argument,
do not clear the *index* buffer upon entry, but just set to the buffer."
  (interactive "P")
  (cond (group-bogus
	 (group-bogus-warn group-current))
	((not group-current)
	 (news-end t))
	(t
	 (if (< 100 article-count)	; lots of articles, & maybe holes
	     (let ((i 0))		; five should suffice for closing
	       (while (< i 5) (article+1) (setq i (1+ i)))  ; major holes
	       (setq amark (amark-list-delete article-first amark))))
	 (setq amark-index amark)
	 (if (not in-group)
	     (setq index-return-configuration (current-window-configuration)))
	 (if news-index-fast
	     (news-index-fast pfx nosub in-group)
	   (setq index-pop index-pop-up)
	   (or in-group (news-goto group-current nosub))
	   (set-buffer index-buffer)
	   (index-mode)
	   (if pfx nil
	     (setq buffer-read-only)
	     (erase-buffer)
	     (gnews-buffer index-pop index-buffer)
	     (setq index-x-menu nil)
	     (mapcar '(lambda (x)
			(insert (format "%5dm %s\n" (car x) (cdr x))))
		     (cdr index-perm-marks))
	     (setq index-final article-current)
	     (while (and (index-expand-forward nil index-show-kills)
			 (or index-display-all (y-or-n-p "More ? "))))
	     (recenter 0)
	     (if index-sort-do
		 (index-sort)
	       (index-forward 0))
	     (setq buffer-read-only t))
	   (index-beginning-of-buffer)
	   (if (= 1 (point-max))
	       (progn
		 (group-quit)
		 (news-next-unread))
	     (article-current-set (index-article))
	     (message "")
	     (gnews-flush))))))

(defun news-index-fast (pfx &optional nosub in-group)
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
  (sit-for 0)
  (message "indexing...")
  (send-string nntp-index (concat "group " group-current "\n"))
  (setq nntp-index-final (if (amark-member article-final amark)
			     (amark-previous-unread article-final amark)
			   article-final))
  (amark-loop art-no (list (cons article-current article-final))
    (if (amark-member art-no amark) nil		; already junked
      (send-string nntp-index (concat "head " art-no "\n"))))
  (index-mode)
  (setq index-x-menu nil)
  (goto-char 1)
  (setq index-final article-current)
  (article-current-set index-final))

(defun group-index (&optional art-list)
  "Switch over to the index-buffer.  [[With article ranges, make an index
of those articles.]]"
  (interactive)
  (if (interactive-p)
      (if (boundp 'index-pop)
	  (progn		; this must go in non-interactive case
	    (article-junk)
	    (set-buffer index-buffer)
	    (let ((p (point)))
	      (goto-char 1)
	      (if (re-search-forward (concat "^ *" article-current) nil t)
		  (index-junk-line nil)
		(goto-char p)
		(beginning-of-line)))
	    (index-mode)
	    (if (and (not (eq article-junkable 'mark))
		     (= (index-article) article-current)
		     (not (index-flag-p)))
		(index-junk-line nil))
	    (if index-junk-do
		(index-junkers-erase)
	      (condition-case ()	; ignore end-of-buffer "errors"
		  (index-forward 1)
		(error)))
	    (gnews-buffer index-pop index-buffer)
	    (if (index-first-line)
		(beginning-of-line)
	      (group-quit)
	      (news-next-unread)))
	(news-index nil nil t))		; missing index buffer
    (message "unimplemented"))		; the non-interactive case
  (gnews-flush))

(defvar index-hook-commands '(news-default)
  "The list of commands that pre-hook indexing is triggered by.
See index-if.")

(defun index-if (&optional count commands)
  "Index the current newsgroup.  For use within pre-hooks.\n
Optional argument COUNT if numeric means the indexing will not be done
if there are fewer unread articles than COUNT.  A nil value is treated
as one.\n
Argument COMMANDS is a list of the commands that permit automatic
indexing.  A nil list is treated as the list index-hook-commands,
with default value '(news-default).\n
Thus, the minimal pre-hook (pre nil index-if) means to always index
automatically upon entry when the group is entered with a `SPC',
but otherwise set to the group without indexing."
  (setq count (or count 1)
	commands (or commands index-hook-commands))
  (cond ((and (memq group-entry-command commands)
	      (<= count group-proffer-count))
	 (setq amark-index amark
	       index-return-configuration (current-window-configuration))
	 (news-index-fast nil nil t))))

;;; index support functions

(defun index-junkers-erase ()
  "Remove \"j\" lines from the index-buffer."
  (interactive)
  (save-excursion
    (goto-char 1)
    (setq buffer-read-only)
    (while (not (eobp))
      (forward-char 5)
      (cond ((memq (following-char) (mapcar 'identity index-junkers))
	     (beginning-of-line)
	     (gnews-delete-line))
	    (t
	     (forward-line 1))))
    (goto-char (point-max))
    (if (and (bolp) (not (bobp))) (delete-backward-char 1))
    (setq buffer-read-only t)))

(defun index-article ()
  "Get article number for the current index line."
  (save-excursion 
    (set-buffer index-buffer)
    (beginning-of-line)
    (re-search-forward " *\\([0-9]+\\)")
    (read (gnews-match 1))))

(defun index-subject ()
  "Get the subject for the current index line."
  (buffer-substring (+ 7 (gnews-bol)) (+ 39 (gnews-bol))))

(defun index-flag-p ()
  "Return non-nil if looking at a flagged line.\n
Automatically flags and junks index-kill'ed topics."
  (or (looking-at "^ *[0-9]+[a-z]")
      (let* ((ikp index-kill-per)
	     (hk (car ikp))
	     (h (car hk))
	     (k (cdr hk))
	     match)
	(while (and hk (not match))
	  (setq match (string-match
			(concat "\\(Re:[ \t]*\\)" h)
			(substring (index-subject) 0 32))
		ikp (cdr ikp) hk (car ikp) h (car hk) k (cdr hk)))
	(if match
	    (progn
	      (article-junk k (index-article))
	      (index-flag ?k)))
	match)))

(defun index-flag (flag &optional no-over)
  "FLAG this line (FLAG is a character).  Optional NO-OVER argument means
do not overwrite any existing flag."
  (let (buffer-read-only)
    (re-search-forward "\\([0-9]\\)\\([^0-9]\\)")
    (if (if no-over (string= (gnews-match 2) " ") t)
	(replace-match (concat "\\1" (char-to-string flag))))
    (beginning-of-line)))

(defun index-junk-line (delete)
  "Junk a line in the index-buffer.  If DELETE is non-nil, the line is
actually deleted, otherwise it is marked with a \"j\""
  (if (eq article-junkable 'mark) nil
    (setq buffer-read-only)
    (beginning-of-line)
    (if delete
	(progn (gnews-delete-line)
	       (if (eobp)
		   (if (bobp)
		       (index-quit)
		     (delete-backward-char 1)
		     (beginning-of-line))))
      (index-flag ?j))
    (setq buffer-read-only t)))

(defun index-first-line ()
  "Return (point) of first unjunked line of index-buffer, otherwise nil."
  (save-excursion
    (goto-char 1)
    (if (re-search-forward "^ *[0-9]+ " nil t) (gnews-bol))))

(defun index-eobp ()
  "Return t if point is at the end of the index buffer, nil if not."
  (save-excursion (forward-line 1) (eobp)))

(defun index-bobp ()
  "Return t if point is at the beginning of the index buffer, nil if not."
  (save-excursion (beginning-of-line) (bobp)))

;;; index sorting

(defun index-sort-function (x y)
  "Index sort comparison function.  See index-sort for details."
  (let ((a (cdr x))
	(b (cdr y))
	(i (read (substring (car x) 0 5)))
	(j (read (substring (car y) 0 5))))
    (or (string< a b) (if (string= a b) (< i j)))))

(defun index-sort-prep (h)
  "Prepare current line for index sorting.  See index-sort for details."
  (cons h (downcase (substring (concat (reply-re-0 (substring h 7))
				       (make-string 32 ? ))
			       0 32))))

(defun index-sort ()
  "Sort the index buffer by subject headers.\n
To get a different sort then the default, you need only modify up to
two internal functions, index-sort-prep and maybe index-sort-function.\n
index-sort-prep takes one argument, a string containing the contents
of a line in the index buffer.  The function returns a dotted pair of
the form (DISPLAY-LINE . PRIMARY-KEY).  DISPLAY-LINE refers to what is
actually displayed after sorting--the only restriction is that the first
six characters are reserved for article number and junking symbols.
Normally it will be the same as the argument.  PRIMARY-KEY is a rubric:
the given index-sort-function uses it as such.  They are separated out
to avoid recomputation in sorting.\n
The default index-sort-function compares two dotted pairs, returning t if
either the first pair's PRIMARY-KEY is string< than the second pair's, or
else if the two compare equal under string=, but the article number of the
first pair's DISPLAY-LINE is less than the second's.  Otherwise it returns
nil.\n
One possibility would be where you wish to order articles by date.  The
first thing to do would be to add a third field--see index-headers--and
put it, say, past column 80, with truncate-lines set to t in index-mode.
You could then remove it from DISPLAY-LINE."
  (interactive)
  (setq buffer-read-only)
  (message "sorting...")
  (let* ((l (list nil)) (ll l))
    (goto-char (point-max))
    (if (not (bolp)) (insert ?\n))
    (goto-char 1)
    (while (not (eobp))
      (nconc ll (list (index-sort-prep
			(buffer-substring
			  (point) (progn (forward-line 1) (point))))))
      (setq ll (cdr ll)))
    (setq ll (sort (cdr l) 'index-sort-function))
    (erase-buffer)
    (while ll
      (insert (gnaar ll))
      (setq ll (cdr ll))))
  (message "sorting...done")
  (delete-backward-char 1)
  (goto-char 1)
  (index-forward 0)
  (setq buffer-read-only t)
  (gnews-flush))

;;; index-mode

(defun index-forward (arg)
  "Move forward ARG lines, ignoring the junked ones."
  (interactive "p")
  (beginning-of-line)
  (cond ((< arg 0)
	 (index-backward (- arg)))
	((= arg 0)
	 (while (index-flag-p)
	   (forward-line 1)))
	(t
	 (while (and (not (index-eobp)) (< 0 arg))
	   (next-line 1)
	   (while (index-flag-p)
	     (forward-line 1))
	   (setq arg (1- arg)))))
  (beginning-of-line)
  (let ((latj (index-flag-p)))
    (if (or latj (< 0 arg))
	(progn
	  (if latj (index-backward 1))
	  (error "last available article in the newsgroup"))))
  (gnews-flush))

(defun index-forward-all (arg)
  "Move forward ARG lines."
  (interactive "p")
  (if (index-eobp) (error "end of index"))
  (forward-line arg)
  (beginning-of-line))

(defun index-backward (arg)
  "Move backward ARG lines, ignoring the junked ones."
  (interactive "p")
  (beginning-of-line)
  (if (< arg 0) (index-forward (- arg)))
  (while (and (not (index-bobp)) (< 0 arg))
    (previous-line 1)
    (while (and (index-flag-p) (not (index-bobp)))
      (forward-line -1))
    (setq arg (1- arg)))
  (if (index-bobp)
      (if (index-flag-p)
	  (if (interactive-p)
	      (progn (index-forward 1) (error "first available article"))
	    (error "no available articles"))))
  (gnews-flush))

(defun index-backward-all (arg)
  "Move forward ARG lines."
  (interactive "p")
  (if (index-bobp) (error "start of index"))
  (forward-line (- arg))
  (beginning-of-line))

(defun index-scroll-up (pfx arg)
  "Scroll up in the index buffer."
  (interactive "P\np")
  (scroll-up (if pfx arg))
  (gnews-flush))

(defun index-scroll-down (pfx arg)
  "Scroll down in the index buffer."
  (interactive "P\np")
  (scroll-down (if pfx arg))
  (gnews-flush))

(defun index-search-forward (pfx)
  "Search forward for same subject of last read article.  With prefix,
prompt for string."
  (interactive "P")
  (let* ((pp (point))
	 (s (if pfx
		(setq index-search-string (read-from-minibuffer
					   "Search for: "
					   index-search-string))
	      (let* ((str (cdr (assoc "Subject" article-field-list)))
		     (len (min 20 (length str))))
		(substring str 0 len))))
	 (r (search-forward s nil t))
	 (p (progn (beginning-of-line) (point))))
    (cond ((= p pp)
	   (forward-line 1)
	   (if (search-forward s nil t)
	       (beginning-of-line)
	     (error "not found")))
	  (r
	   (beginning-of-line))
	  (t
	   (error "not found"))))
  (gnews-flush))

(defun index-search-backward (pfx)
  "Search backward for same subject of last read article.  With prefix
prompt for string."
  (interactive "P")
  (let* ((s (if pfx
		(setq index-search-string (read-from-minibuffer
					   "Search for: "
					   index-search-string))
	      (let* ((str (cdr (assoc "Subject" article-field-list)))
		     (len (min 20 (length str))))
		(substring str 0 len)))))
    (if (search-backward s nil t)
	(beginning-of-line)
      (error "not found")))
  (gnews-flush))

(defun index-article-get ()
  "Switch or pop, as indicated by the value of index-pop, to the
indicated article"
  (interactive)
  (if index-pop (index-other-window) (index-buffer-switch)))

(defun index-buffer-switch ()
  "Switch from index buffer to the indicated article."
  (interactive)
  (setq index-pop nil)
  (let ((art (index-article)))
    (switch-to-buffer news-buffer)
    (cond ((catch 'article-nil (article-get art nil t))
	   (article-junk-local)
	   (message "article %d was just cancelled" art))))
  (gnews-flush))

(defun index-other-window ()
  "Display the indicated article in alternate window."
  (interactive)
  (setq index-pop (or index-pop-up t))
  (let ((art (index-article)))
    (pop-to-buffer news-buffer)
    (cond ((catch 'article-nil (article-get art nil t))
	   (article-junk-local)
	   (message "article %d was just cancelled" art))))
  (gnews-flush))

(defun index-undo (arg)
  "Undo in the index-buffer.\n
Note that this is not yet a genuine Gnews undo, only an undo on the
contents of the index-buffer."
  (interactive "p")
  (setq buffer-read-only)
  (undo arg)
  (setq buffer-read-only t)
  (beginning-of-line)
  (save-excursion
    (let (goal-column)
      (goto-char 1)
      (while (not (eobp))
	(if (looking-at (concat "^ *[0-9]+[" index-junkers "]"))
	    (amark-list-insert (index-article) amark-index))
	(forward-line 1))))
  (group-amark-set group-current amark-index)
  (gnews-flush))

(defun index-ignore ()
  "Move forward to the next subject; mark this topic as \"ignored\".
That is, the article is not junked, but the index motion commands
will skip over it."
  (interactive)
  (beginning-of-line)
  (let ((sub (reply-re-0 (index-subject)))
	goal-column)
    (setq buffer-read-only)
    (index-flag ?i t)
    (forward-line 1)
    (while (and (not (eobp))
		(looking-at (concat "\\(^.....\\). \\(\\([Rr][Ee]: *\\)?"
				    (regexp-quote sub) "\\)")))
      (index-flag ?i t)
      ;(replace-match "\\1i \\2")
      (forward-line 1))
    (setq buffer-read-only t))
  (beginning-of-line))

(defun index-junk (pfx)
  "Junk current index article.  With prefix argument or index-junk-do
non-nil, delete the line also."
  (interactive "P")
  (setq article-junkable t)
  (article-junk nil (index-article))
  (index-junk-line (or pfx index-junk-do))
  (condition-case ()
      (index-forward 1)
    (error))
  (gnews-flush))

(defun index-junk-local (pfx)
  "Junk current index article in current newsgroup only.  With prefix
argument or index-junk-do non-nil, delete the line also."
  (interactive "P")
  (setq article-junkable t)
  (article-junk t (index-article))
  (index-junk-line (or pfx index-junk-do))
  (condition-case ()
      (index-forward 1)
    (error))
  (gnews-flush))

(defun index-mark ()
  "Mark the current index article as unread."
  (interactive)
  (beginning-of-line)
  (if (re-search-forward "\\( *[0-9]+\\)[a-z]" (+ 8 (point)) t)
      (let ((art-no (gnews-match 1)))
	(setq buffer-read-only)
	(replace-match (concat art-no " "))
	(setq amark (amark-list-unread (read art-no) amark)
	      buffer-read-only t)
	(beginning-of-line)))
  (gnews-flush))

(defun index-mark-region (pfx a b)
  "Mark the articles in the current region as unread.  (Permanently if
there's a prefix argument--see group-mark-permanent.)"
  (interactive "P\nr")
  (let ((al (list nil)))
    (save-excursion
      (goto-char a)
      (beginning-of-line)
      (while (<= (point) b)
	(if pfx
	    (nconc al (list (index-article)))
	  index-mark)
	(forward-line 1))
      (if pfx (group-mark-permanent al))))
  (message "region marked")
  (gnews-flush))

(defun index-junk-region (pfx a b)
  "Junk the articles in the current region as read.  (Locally if
there's a prefix argument.)"
  (interactive "P\nr")
  (let ((al (list nil)))
    (save-excursion
      (goto-char a)
      (while (<= (point) b)
	(beginning-of-line)
	(article-junk pfx (index-article))
	(index-junk-line index-junk-do)
	(sit-for 0)
	(condition-case ()
	    (index-forward 1)
	  (error)))))
  (message "region junked")
  (gnews-flush))

(defun index-kill (pfx)
  "Kill this subject of discussion.  With prefix argument PFX, the
killing is local to the current newsgroup."
  (interactive "P")
  (if (or index-kill-auto (y-or-n-p "Kill this subject? "))
      (let* ((subj (reply-space-off (reply-re-0 (index-subject))))
	     (hk-subj (regexp-quote subj))
	     (hk (list (list "Subject" hk-subj
			     (if pfx 'article-junk-local 'article-junk)))))
	(setq index-kill-per (cons (cons hk-subj pfx) index-kill-per))
	(if hook-kill-per
	    (nconc hook-kill-per hk)
	  (setq hook-kill-per hk))
	(article-junk pfx (index-article))
	(index-flag ?k)
	(message "%s: killed" subj)
	(index-forward 1)))
  (gnews-flush))

(defun index-kill-permanent (pfx)
  "Kill this subject of discussion permanently.  With prefix argument PFX, 
the killing is local to the current newsgroup."
  (interactive "P")
  (if (or index-kill-auto (y-or-n-p "Kill this subject? "))
      (let* ((subj (reply-space-off (reply-re-0 (index-subject))))
	     (hk-subj (regexp-quote subj))
	     (hk  (list "Subject" hk-subj
			(if pfx 'article-junk-local 'article-junk))))
	(setq index-kill-per (cons (cons hk-subj pfx) index-kill-per))
	(if hook-kill-per
	    (nconc hook-kill-per (list hk))
	  (setq hook-kill-per (list hk)))
	(hook-kill-add (hook-kill-group group-current) hk)
	(article-junk pfx (index-article))
	(index-flag ?k)
	(message "%s: killed permanently" subj)
	(index-forward 1)))
  (gnews-flush))

(defun index-catchup (pfx arg)
  "Catch up in current newsgroup."
  (interactive "P\np")
  (if (< (roster-catchup group-current t pfx arg) article-final)
      (progn
	(goto-char 1)
	(index-forward 1)
	(condition-case nil
	    (index-backward 1)
	  (error))
	(gnews-flush))
    (set-buffer news-buffer)
    (group-quit-intern)
    (switch-to-buffer news-buffer)
    (news-mode)
    (news-next-unread-maybe))
  (gnews-flush))

(defun index-unsubscribe ()
  "Unsubscribe from the current newsgroup."
  (interactive)
  (if (roster-unsubscribe group-current)
      (progn
	(set-buffer news-buffer)
	(delete-windows-on index-buffer)
	(group-quit-intern)
	(news-mode)
	(switch-to-buffer news-buffer)
	(news-next-unread-maybe)))
  (gnews-flush))

(defun index-quit ()
  "Quit the current newsgroup."
  (interactive)
  (set-buffer news-buffer)
  (call-interactively 'group-quit))

(defun index-beginning-of-buffer ()
  "Move to the beginning of the index buffer."
  (interactive)
  (goto-char 1)
  (beginning-of-line)
  (gnews-flush))

(defun index-end-of-buffer ()
  "Move to the end of the index buffer."
  (interactive)
  (goto-char (point-max))
  (beginning-of-line)
  (gnews-flush))

(defun index-expand-forward (arg &optional all)
  "Expand the index buffer forward.  By default one extra window's worth
of articles only is provided; with a prefix argument, the rest of the
newsgroup is indexed.  This function assumes kills are in effect--use the
\"\\[index-expand-forward-all]\" command to override them."
  (interactive "P")
  (setq buffer-read-only)
  (index-end-of-buffer)
  (if (looking-at "^ *[0-9]") (next-line 1))
  (article-current-set index-final)
  (unwind-protect
      (condition-case ()
	  (save-excursion
	    (let ((w (or index-display-count (1- (window-height))))
		  (standard-output index-buffer)
		  (filters (append '(identity) index-filter))
		  (sizes (append '(nil) index-sizes))
		  headers hook junk)
	      (while (and (<= article-current article-final)
			  (or arg (< 0 w)))
		(setq headers (index-prepare article-current index-headers)
		      hook hook-kill-per
		      junk nil)
		(if (string= (mapconcat 'identity (cdr headers) "") "")
		    nil	    ; a null article
		  (while (and hook (not junk))
		    (setq junk (hook-kill-do (car hook) t)
			  hook (cdr hook)))
		  (if (and junk (not all))
		      (hook-kill-junk-message article-current hook)
		    (insert (index-line
			      article-current
			      (concat
				"%5d"
				(cond ((and all (amark-member
						  article-current amark))
				       "j")
				      ((and all junk) "k")
				      (t " "))
				" " index-format "\n")
			      headers
			      filters
			      sizes))
		    (setq w (1- w)))
		  (sit-for 0))
		(article-current-set (article+1 all)))
	      (if (not (bobp)) (delete-backward-char 1)))
	    (< article-current article-final))	 ; normal return value
	(quit (goto-char (point-max))
	      (if (and (bolp) (not (bobp))) (delete-backward-char 1))
	      (beginning-of-line)
	      (article-current-set (1+ (index-article)))
	      nil))				 ; quit return value
    (setq buffer-read-only t)
    (setq index-final article-current)		 ; for next time through
    (if (< 1 (point-max))
	(article-current-set (index-article))
      nil)					 ; what to do?? (cf news-index)
    (gnews-flush)))

(defun index-expand-forward-all (arg)
  "Expand the index-buffer forwards, getting all existing articles."
  (interactive "P")
  (index-expand-forward arg t))

(defun index-expand-backward ()
  "Expand the index-buffer backwards."
  (message "unimplemented"))

(defun index-expand-backward-all ()
  "Expand the index-buffer backwards, getting all existing articles."
  (message "unimplemented"))

(defun index-x-select ()
  "Select an article via a pop up X window.  (very primitive)"
  (interactive)
  (if (null index-x-menu)
      (progn
	(setq index-x-menu (list group-current))
	(let* ((ix index-current)
	       (i (car ix)))
	  (while ix
	    (let* ((title (reply-re-0 (gnadr i)))
		   (im (assoc title index-x-menu)))
	      (cond (im
		     (nconc im (list (cons (gnaddr i) (car i)))))
		    (t
		     (nconc index-x-menu
			    (list (list title (cons (gnaddr i) (car i))))))))
	    (setq ix (cdr ix) i (car ix))))))
  ;; the selection
  (x-proc-mouse-event)
  (let ((art (x-popup-menu x-mouse-pos index-x-menu)))
    (if art (article-get art nil t))))

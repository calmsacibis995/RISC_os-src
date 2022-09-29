;;; prims.el: primitive commands/macros for Gnews
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

(provide 'gnews-prims)

;;; some general macros

(defmacro gnaar (x) (` (car (car (, x)))))
(defmacro gnadr (x) (` (car (cdr (, x)))))
(defmacro gndar (x) (` (cdr (car (, x)))))
(defmacro gnddr (x) (` (cdr (cdr (, x)))))
(defmacro gnaddr (x) (` (car (cdr (cdr (, x))))))

(defmacro gnews-string-as-buffer (string val &rest body)
  "(gnews-string-as-buffer STRING VAL BODY...) executes BODY in a temporary
buffer whose contents are STRING.  The returned value depends on VAL: if
VAL equals 'b, the buffer from its beginning to (point) is returned, if it
equals 'e, the buffer from (point) to its end is returned, if VAL is
otherwise non-nil, the entire buffer is returned, and if VAL is nil, the
value of the last form in BODY is returned."
  (` (let ((buf (generate-new-buffer "*string*"))
	   (old (current-buffer)))
       (set-buffer buf)
       (insert (, string))
       (goto-char 1)
       (prog1 (let ((result (progn (,@ body))))
		(cond ((equal (, val) 'e)
		       (buffer-substring (point) (point-max)))
		      ((equal (, val) 'b)
		       (buffer-substring (point-min) (point)))
		      (t
		       (if (, val) (buffer-string) result))))
	 (kill-buffer buf)
	 (set-buffer old)))))

(put 'gnews-string-as-buffer 'lisp-indent-hook 'defun)

;;; some general primitives

(defun gnews-map (func &rest lists)
  "Apply FUNC elementwise across the list of LISTS, with nil truncation."
  (if (not (memq nil lists))			; (apply 'and lists)
      (cons (apply func (mapcar 'car lists))
	    (apply 'gnews-map func (mapcar 'cdr lists)))))

(put 'gnews-map 'lisp-indent-hook 1)

(defun gnews-last (l)
  "Return last item in a list L."
  (nth (length (cdr l)) l))

(defun gnews-bol ()
  "Return value of point at beginning of line."
  (save-excursion (beginning-of-line) (point)))

(defun gnews-eol ()
  "Return value of point at end of line."
  (save-excursion (end-of-line) (point)))

(defun gnews-arg-count (arg)
  "Compute [(log ARG)/(log 4)] (ie, it counts \\[universal-argument]'s)."
  (let ((dec 0))
    (while (< 1 arg)
      (setq dec (+ 1 dec))
      (setq arg (/ arg 4)))
    dec))

(defun gnews-delete-word ()
  "Delete to the end of the current word."
  (delete-region (point) (progn (forward-word 1) (point))))

(defun gnews-delete-line ()
  "Delete to the end of the current line."
  (delete-region (point) (progn (forward-line 1) (point))))

(defun gnews-delete-paragraph ()
  "Delete to the end of the current paragraph."
  (delete-region (point) (progn (forward-paragraph 1) (point))))

(defun gnews-mkdir (dir)
  "Create directory DIR if non-existent."
  (setq dir (expand-file-name dir))
  (if (not (file-exists-p dir))
      (let ((dirs (list nil)))
	(while (and (not (file-exists-p dir))
		    (not (string= dir "/")))	 ; is this always correct?
	  (setq dir (directory-file-name dir))	 ; ie, remove trailing /
	  (nconc dirs (list dir))
	  (setq dir (file-name-directory dir)))	 ; ie, remove trailing name
	(setq dirs (nreverse (cdr dirs)))
	(while (car dirs)
	  (call-process "mkdir" nil nil nil (car dirs))
	  (setq dirs (cdr dirs))))))

(defun reply-domain (s)
  "Remove the domains from an address."
  (let* ((i (string-match "[^@]+@..[^.]*\\(\\.\\)[^ \t]*\\([ \t]+\\|$\\)" s))
	 (ii (if i (match-beginning 1)))
	 (j (string-match "[^%]+%[^@.]*\\([@.]\\)[^ \t]*\\([ \t]+\\|$\\)" s))
	 (jj (if j (match-beginning 1)))
	 (k (if i (if j (min ii jj) ii) (if j jj)))
	 (head (substring s 0 k))
	 (tail (if k (substring s (match-end 0)))))
    (if k (concat head (if (string= tail "") "" " ") tail) s)))

(defun reply-re-1 (subj)
  "Return string SUBJ with exactly one \"Re:\" prefix."
  (concat "Re: " (reply-re-0 subj)))

(defun reply-re-0 (subj)
  "Return string SUBJ stripped of all \"Re:\" prefixes."
  (if (string= subj "") ""
    (progn (string-match "^ *\\(Re:[ \t]*\\)*" subj)
	   (substring subj (match-end 0)))))

(defun reply-space-off (subj)
  "Return string SUBJ stripped of trailing white space."
  (string-match "\\([ \t]*\\)$" subj)
  (substring subj 0 (match-beginning 1)))

(defun gnews-replace (regexp replace string)
  "Replace REGEXP with REPLACE in STRING."
  (if (string-match regexp string)
      (concat (substring string 0 (match-beginning 0))
	      replace
	      (gnews-replace regexp replace (substring string (match-end 0))))
    string))

(put 'gnews-replace 'lisp-indent-hook 2)

(defun gnews-copy-keymap (keymap)
  ;; unneeded for v18.51 and up...
  "Copy a sparse KEYMAP recursively, so that no subkey in the new map
can have a side effect on the original."
  (if (atom keymap)
      keymap
    (cons (if (consp (car keymap))
	      (gnews-copy-keymap (car keymap))
	    (car keymap))
	  (gnews-copy-keymap (cdr keymap)))))

(defun gnews-subst-command-keys (s)
  "Replace command names in string S with extra compact key descriptions;
like substitute-command-keys, only even more compact."
  (gnews-pfx-replace (substitute-command-keys s)))

(defun gnews-pfx-replace (s)
  "In string S, convert ESC x to ~X and C-x to ^X."
  (let ((e (string-match "ESC " s))
	(c (string-match "C-" s)))
    (cond ((and e (if c (< e c)))
	   (concat (substring s 0 e)
		   "~"
		   (upcase (substring s (+ 4 e) (+ 5 e)))
		   (gnews-pfx-replace (substring s (+ 5 e)))))
	  (c
	   (concat (substring s 0 c)
		   "^"
		   (upcase (substring s (+ 2 c) (+ 3 c)))
		   (gnews-pfx-replace (substring s (+ 3 c)))))
	  (t
	   s))))

(defun gnews-exec-1-pending ()
  "Execute the leading article-mode command in command input."
  (let ((s "") b c)
    (while (and (not (numberp c)) (input-pending-p))
      (setq s (concat s (char-to-string (read-char))))
      (setq b c c (lookup-key article-mode-map s)))
    (cond ((commandp b) (call-interactively b))
	  ((commandp c) (call-interactively c)))))

(defun gnews-buffer (pop buf)
  "If POP is non-nil, pop to BUF.  If nil, switch to BUF.\n
If POP is also numeric, adjust the window to POP many lines high."
  (funcall (if pop 'pop-to-buffer 'switch-to-buffer) buf)
  (if (numberp pop) (enlarge-window (- pop (window-height)))))

(defun gnews-buffer-list ()
  "Return a list of the currently active Gnews buffers."
  (let* ((bl (buffer-list))
	 (b (car bl))
	 gbl)
    (while bl
      (if (string-match "^\\*gnews\\*" (buffer-name b))
	  (setq gbl (cons b gbl)))
      (setq bl (cdr bl) b (car bl)))
    (if (boundp 'gnews-spool-active)
	(setq gbl (cons gnews-spool-active gbl)))
    gbl))

(defun gnews-comma-parse (s)
  "Given a string S of comma separated names, return an alist of
strings of the names."
  (if (zerop (length s)) nil
    (let ((n (string-match "," s)) g)
      (while n
	(setq g (cons (list (substring s 0 n)) g)
	      s (substring s (1+ n)) n (string-match "," s)))
      (cons (list (substring s 0 n)) g))))

(defun gnews-match (i)
  (buffer-substring (match-beginning i) (match-end i)))


;;; Stepping through amarks

(defmacro amark-loop (var om &rest body)
  "(amark-loop VAR OM BODY...) executes BODY, stepping forwards through
the range of article numbers spanned by OM, using VAR as the variable name
that refers, within BODY, to the article number."
  (` (let ((gm (, om)) art-pair art-last (, var))
       (while gm
	 (setq art-pair (car gm)
	       (, var) (amark-car art-pair)
	       art-last (amark-cdr art-pair))
	 (while (and (, var) (<= (, var) art-last))
	   (progn (,@ body))
	   (setq (, var) (1+ (, var))))
	 (setq gm (cdr gm))))))

(defmacro amark-pool (var om &rest body)
  "(amark-pool VAR OM BODY...) executes BODY, stepping backwards through
the range of article numbers spanned by OM, using VAR as the variable name
that refers, within BODY, to the article number."
  (` (let ((gm (reverse (, om))) art-pair art-first (, var))
       (while gm
	 (setq art-pair (car gm)
	       (, var) (amark-cdr art-pair)
	       art-first (amark-car art-pair))
	 (while (and (, var) (<= art-first (, var)))
	   (progn (,@ body))
	   (setq (, var) (1- (, var))))
	 (setq gm (cdr gm))))))

(put 'amark-loop 'lisp-indent-hook 2)
(put 'amark-pool 'lisp-indent-hook 2)

;;; keymap handling

(defun gnews-key-bind (keymap pairs)
  "Return KEYMAP with new key bindings lifted from the list of dotted
PAIRS.  The car of a member of PAIRS gives the key, and the corresponding
cdr gives the function.\n"
  (mapcar '(lambda (x) (define-key keymap (car x) (cdr x))) pairs))

(defvar news-mode-map nil "*Keymap for News mode")
(defvar group-mode-map nil "*Keymap for Group mode")
(defvar article-mode-map nil "*Keymap for Pager mode")
(defvar e-reply-mode-map nil "*Keymap for E-Reply mode")
(defvar n-reply-mode-map nil "*Keymap for N-Reply mode")
(defvar roster-mode-map nil "*Keymap for Roster mode")
(defvar index-mode-map nil "*Keymap for Index mode")
(defvar hook-kill-mode-map nil "*Keymap for Hook Kill mode")
(defvar gnews-edit-mode-map nil "*Keymap for Gnews Edit mode")

;;; Newsgroup name reading and abbreviations

(defun group-name-read (prompt roster pred)
  "Read in the name of a newsgroup, with abbreviations and completion.
Prompt with string PROMPT, and restrict names to those in the alist
ROSTER that match PREDicate.  Returns the newsgroup name, and as a side
effect, sets group-read-mood to the first character typed.\n
\\<news-mode-map>\
For newsgroup entry commands that use this function, an immediate
\"\\[news-index]\" is interpreted as a request to enter the named group via
index-mode, while an immediate \"\\[news-at]\" means the group is only to
be set to, not entered.\n
\\<gnews-abbrev-keymap>\
An immediate \"\\\[gnews-abbrev-expand]\" abbreviates the current \
newsgroup, \\<news-mode-map>\"\\[news-end]\" expands
to the user's last newsgroup, and \
\"\\[news-first-unread]\" to the user's first."
  (group-warn-delete-window t)
  (message prompt)
  (prog1
      (let ((minibuffer-completion-confirm group-name-confirm)
	    (minibuffer-completion-table roster)
	    (minibuffer-completion-predicate pred)
	    n a f)
	(setq group-read-mood (read-char)
	      n (aref news-mode-map group-read-mood)
	      a (cdr (assq group-read-mood gnews-abbrev-keymap))
	      group-entry-command this-command)
	;; This loses on multi-key def's (say from function keys).  Later.
	(cond ((eq n 'news-at)				 ; @
	       (read-from-minibuffer
		 (concat prompt "[set to] ") "" gnews-abbrev-keymap))
	      ((eq n 'news-index)			 ; =
	       (read-from-minibuffer
		 (concat prompt "[index] ") "" gnews-abbrev-keymap))
	      ((eq n 'news-end)				 ; $
	       (read-from-minibuffer
		 prompt (car (gnews-last group-roster)) gnews-abbrev-keymap))
	      ((eq n 'news-first-unread)		 ; ^
	       (let (minibuffer-completion-predicate)	 ; turn it off
		 (read-from-minibuffer
		   prompt (gnaar group-roster) gnews-abbrev-keymap)))
	      ((and group-current (eq a 'gnews-abbrev-expand))
	       (setq gnews-pre-abbrev "")		 ; .
	       (read-from-minibuffer
		 prompt group-current gnews-abbrev-keymap))
	      ((eq a 'minibuffer-completion-help)	 ; ?
	       (group-name-read-completion prompt roster pred))
	      ((or (eq a 'minibuffer-complete)		 ; TAB
		   (eq a 'minibuffer-complete-word))	 ; SPC
	       (setq f (try-completion "" roster pred))
	       (if (string< "" f)
		   (read-from-minibuffer prompt f gnews-abbrev-keymap)
		 (group-name-read-completion prompt roster pred)))
	      (t					 ; other
	       (read-from-minibuffer
		 prompt (char-to-string group-read-mood)
		 gnews-abbrev-keymap))))
    (let ((comp-win (get-buffer-window " *Completions*")))
      (if comp-win (progn (delete-window comp-win) (sit-for 0))))))

(defun group-name-read-completion (prompt roster pred)
  "Display a completion buffer in group-name-read."
  (with-output-to-temp-buffer " *Completions*"
    (display-completion-list
      (sort (apply 'append (mapcar '(lambda (g)			; must be a
				      (if (funcall pred g)	; bit tricky
					  (list (car g))))	; to squeeze
				   roster))			; out nils
	    'string<)))
  ;; The following line doesn't work.  So why not?
  (setq minibuffer-scroll-window (get-buffer-window " *Completions*"))
  (read-from-minibuffer prompt "" gnews-abbrev-keymap))

(defvar gnews-abbrev '(nil) "Abbreviations for newsgroup name expansion.")

(setq gnews-abbrev-keymap (gnews-copy-keymap minibuffer-local-completion-map))

(setq gnews-pre-abbrev nil)

(gnews-key-bind gnews-abbrev-keymap
		'((".".gnews-abbrev-expand)
		  ("\n".gnews-abbrev-expand-exit)
		  ("\r".gnews-abbrev-exit)
		  ("\\".gnews-abbrev-unexpand)))

;;; abbreviation/unabbreviation keymap commands

(defun gnews-abbrev-expand ()
  "Expand word before point, if it is an abbreviation.  The null
abbreviation is always for the name of the current newsgroup."
  (interactive)
  (let* ((p (save-excursion (forward-word -1) (point)))
	 (q (point))
	 (z (assoc (buffer-substring p q) gnews-abbrev)))
    (setq gnews-pre-abbrev (buffer-substring 1 (point-max)))
    (insert (if (string= gnews-pre-abbrev "")
		group-current
	      (concat (if z (progn (delete-region p q) (cdr z)) "") ".")))))

(defun gnews-abbrev-expand-exit ()
  "Expand word before point, and attempt a completion and exit."
  (interactive)
  (gnews-abbrev-expand)
  (setq this-command 'gnews-abbrev-expand)
  (backward-delete-char 1)
  (sit-for 0)
  (minibuffer-complete-and-exit))

(defun gnews-abbrev-exit ()
  "Standard completion and exit, without abbreviation expansion."
  (interactive)
  (setq this-command 'gnews-abbrev-expand
	gnews-pre-abbrev (buffer-substring 1 (point-max)))
  (minibuffer-complete-and-exit))

(defun gnews-abbrev-unexpand (arg)
  "Cancel last abbreviation expansion.  If the last minibuffer event
wasn't an abbreviation expansion, self-insert ARG times."
  (interactive "p")
  (cond ((or (eq last-command t)	; initial command
	     (eq last-command 'gnews-abbrev-expand))
	 (delete-region 1 (point-max))
	 (insert gnews-pre-abbrev))
	(t
	 (self-insert-command arg))))

;;; news-mode commands

(defun gnews-abbrev-add (abbr)
  "Add a newsgroup ABBReviation.\n
At the prompt, type the ABBReviation and hit return.  At the second
prompt, type what that string is an ABBReviation for."
  (interactive "sNewsgroup abbreviation: ")
  (gnews-abbrev-delete abbr)
  (setq gnews-hook-dangle t)
  (nconc gnews-abbrev
	 (list (cons abbr (read-from-minibuffer
			   (concat "\"" abbr "\" abbreviates: "))))))

(defun gnews-abbrev-delete (abbr)
  "Delete a newsgroup ABBReviation."
  (interactive "sDelete abbreviation: ")
  (setq gnews-hook-dangle t)
  (delq (assoc abbr gnews-abbrev) gnews-abbrev))

(defun gnews-abbrev-list ()
  "List all newsgroup ABBReviations."
  (interactive)
  (with-output-to-temp-buffer "*gnews*abbrev*"
    (display-completion-list
      (mapcar '(lambda (a)
		 (concat (car a) " "
			 (substring "  ~~~>  " (length (car a)))
			 (cdr a)))
	      (cdr gnews-abbrev))))
  (save-excursion
    (save-window-excursion
      (set-buffer "*gnews*abbrev*")
      (goto-char 1)
      (gnews-delete-line)
      (insert "Newsgroup abbreviations:\n\n"))))

;;; keymap for entering numeric article ranges in group-digit

(setq group-range-keymap
      (vconcat (make-vector ?\040 nil)
	       (make-vector ?\020 'gnews-minibuf-exit-char)
	       (make-vector ?\012 'self-insert-command)		; 0-9
	       (make-vector ?\106 'gnews-minibuf-exit-char)))

(aset group-range-keymap ?- 'self-insert-command)
(aset group-range-keymap ?, 'self-insert-command)
(aset group-range-keymap ?$ 'self-insert-command)
(aset group-range-keymap ?  'self-insert-command)
(aset group-range-keymap ?\n 'gnews-minibuf-exit-0)
(aset group-range-keymap ?\r 'gnews-minibuf-exit-0)
(aset group-range-keymap ?\^? 'backward-delete-char)
(aset group-range-keymap ?\^g 'abort-recursive-edit)

(defun gnews-minibuf-exit-char (pfx)
  "Exit the minibuffer, setting gnews-minibuf-last-char to the invoking
command character and gnews-last-prefix to the prefix PFX."
  (interactive "P")
  (setq gnews-minibuf-last-char last-command-char
	gnews-last-prefix pfx)
  (exit-minibuffer))

(defun gnews-minibuf-exit-0 (pfx)
  "Exit the minibuffer, setting gnews-minibuf-last-char to 0 and
gnews-last-prefix to the prefix PFX."
  (interactive "P")
  (setq gnews-minibuf-last-char ?0
        gnews-last-prefix pfx)
  (exit-minibuffer))

(defun gnews-amarkify (string init last)
  "Convert STRING, in \"^,-$ & digits\" format, to an amark bounded below
by INIT and above by LAST.\n
Sets gnews-prefix to STRING."
  (setq gnews-prefix string)		; for group-digit convenience
  (umark-set-init
    init
    (umark-set-last
      last
      (car (read-from-string
	     (gnews-string-as-buffer (concat "(" string ")") t
	       (while (search-forward "," nil t)
		 (replace-match " "))
	       (goto-char 1)
	       (if (search-forward "^" nil t)
		   (replace-match (concat init)))
	       (if (search-forward "$" nil t)
		   (replace-match (concat last)))
	       (goto-char 1)
	       (while (re-search-forward "\\([0-9]+\\)-\\([0-9]+\\)" nil t)
		 (replace-match "(\\1.\\2)"))))))))

;;; mode line handlers

(defun gnews-set-mode-line ()
  "Set the mode line.  (Note that the displayed percentage refers to the
bottom of the screen as in rn, and not the top.)"
  (setq mode-line-format '((gnews-hook-p "-%1*-" "---") " Gnews: "
			   (25 . gnews-mode-string) "  " global-mode-string
			   "  %[(" mode-name gnews-minor-mode-alist ")%]----"
			   (gnews-read-p (article-% ("" article-% "%1*")) "%p")
			   "%-"))
  (if (memq major-mode '(article-mode group-mode)) (gnews-hilite))
  (set-buffer-modified-p (not gnews-hook-p))
  (sit-for 0))

(defun article-%-clear ()
  "Clear the percentage figure"
  (setq article-% nil))

(defun article-%-compute ()
  "Compute percentage of article seen."
  (let ((lines (string-to-int (article-field "Lines"))))
    (if (= 0 lines)		; ie, zero or missing or malformed
	(setq lines (count-lines (article-min) (point-max))))
    (setq article-% (if (= 0 lines) "100"
		      (concat
			(/ (* 100
			      (count-lines (article-min)
					   (save-excursion
					     (move-to-window-line -1)
					     (forward-line 1)
					     (point))))
			   lines))))))

;;; Highlighting the subject

(defun gnews-hilite ()
  (if article-subject-hilite (gnews-hilite-field "Subject")))

(defun gnews-hilite-field (field)
  "Show FIELD in reverse of usual video."
  (save-excursion
    (goto-char 1)
    (if (and (re-search-forward (concat "^" field ": ") nil t)
	     (pos-visible-in-window-p (match-beginning 0)))
	(progn (sit-for 0)
	       (setq buffer-read-only)
	       (let* ((p (match-end 0))
		      (q (gnews-eol))
		      (s (buffer-substring p q))
		      (inverse-video (not inverse-video)))
		 (delete-region p q)
		 (setq buffer-read-only t)
		 (sit-for 0)
		 (setq buffer-read-only)
		 (insert s)
		 (setq buffer-read-only t)
		 (sit-for 0))))))

;;; time functions

(defun gnews-time ()
  "Return current time in ((YY MM DD).(HH MM SS)) format."
  ;; This assumes (current-time-string) returns
  ;; something like: "Sun Jan 10 00:00:00 1988"
  (let* ((months '(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec))
	 (time (current-time-string))
	 (mm (read-from-string time 4))
	 (dd (read-from-string time (cdr mm)))
	 (d (car dd))
	 (di (cdr dd))
	 (h (read (substring time (+ 0 di) (+ 2 di))))
	 (m (read (substring time (+ 3 di) (+ 5 di))))
	 (s (read (substring time (+ 6 di) (+ 8 di))))
	 (y (read (substring time (+ 11 di) (+ 13 di))))
	 (mo (- 13 (length (memq (car mm) months)))))
    (cons (list y mo d) (list h m s))))

(defun gnews-time-form (l)		; RLS
  "Convert a list L in the form (YY MM DD) into \"YYMMDD\"."
  (apply 'format "%02d%02d%02d" l))

(defun gnews-date-back (l n)
  "From a date L in (YY MM DD) format, return the list for N days earlier.
As in NNTP, we assume the 2-digit year YY is centered on 2000."
  (let* ((y (car l))
	 (m (gnadr l))
	 (p (1- m))
	 (d (gnaddr l))
	 (z '(31 28 31 30 31 30 31 31 30 31 30 31)))
    (while (<= d n)
      (setq n (- n d)
	    m (if (< 0 p) p (setq y (1- y)) 12)
	    p (1- m)
	    d (nth p z))
      (if (= y -1) (setq y 99))
      (if (and (= m 2) (zerop (% y 4)) (not (zerop y))) (setq d (1+ d))))
    (list y m (- d n))))

(defun gnews-date-forward (l n)
  "From a date L in (YY MM DD) format, return the list for N days later.
As in NNTP, we assume the 2-digit year YY is centered on 2000."
  (let* ((y (car l))
	 (m (gnadr l))
	 (p (1+ m))
	 (z '(31 28 31 30 31 30 31 31 30 31 30 31))
	 (d (gnaddr l))
	 (c (nth (1- m) z))
	 (e (+ n d)))
    (if (and (= m 2) (zerop (% y 4)) (not (zerop y))) (setq c (1+ c)))
    (while (< c e)
      (setq e (- e c)
	    m (if (< p 13) p (setq y (1+ y)) 1)
	    p (1+ m)
	    c (nth (1- m) z))
      (if (= y 100) (setq y 0))
      (if (and (= m 2) (zerop (% y 4)) (not (zerop y))) (setq c (1+ c))))
    (list y m e)))

;;; {a,u}mark-list handlers

;;; an amark is a list of numbers and dotted pairs of numbers in
;;; monotone order.  ((a.b) c d (e.f)), for example, corresponds
;;; to a-b,c,d,e-f notation in .newsrc files.

;;; Rewritten as macros by RLK:

(defmacro amark-car (c) (` (if (listp (, c)) (car (, c)) (, c))))
(defmacro amark-cdr (c) (` (if (listp (, c)) (cdr (, c)) (, c))))
(defmacro amark-cons (a b)
  (` (if (< (, a) (, b)) (cons (, a) (, b)) (if (= (, a) (, b)) (, a)))))
(defmacro amark-lcons (a b)
  (` (if (< (, a) (, b)) (list (cons (, a) (, b)))
       (if (= (, a) (, b)) (list (, a))))))

;;; amark-size must be a function, since it gets mapcar'ed.

(defun amark-size (c)
  "Return the number of articles spanned by the amark-cons cell C."
  (if c (1+ (- (amark-cdr c) (amark-car c))) 0))

;;; The following macros rely on amark-block-mark-intern for list de-
;;; structive rewriting of amarks.  Since a-b-m-i can't rewrite a nil
;;; list by side effects, these are macros to handle nils directly.

(defmacro amark-block-mark (j q om)
  "Macro for marking articles > J and < Q in amark OM as read, using
side effects when possible, setq otherwise."
  (` (if (null (, om)) (setq (, om) (amark-lcons (1+ (, j)) (1- (, q))))
       (amark-block-mark-intern (, j) (, q) (, om)))))

(defmacro amark-list-insert (j om)
  "Macro for marking article J read in amark OM, using side effects
when possible, setq otherwise."
  (` (if (null (, om)) (setq (, om) (list (, j)))
       (amark-block-mark-intern (1- (, j)) (1+ (, j)) (, om)))))

(defmacro amark-list-init (j om)
  "Macro for marking articles < J in amark OM marked as read, using
side effects when possible, setq otherwise."
  (` (if (null (, om)) (setq (, om) (amark-lcons 1 (1- (, j))))
       (amark-block-mark-intern 0 (, j) (, om))
       (let ((u (car (, om))) (v (cdr (, om))))		; leading 0?
	 (if (and u (zerop (amark-car u)))		; if so, fix
	     (if (zerop (amark-cdr u))
		 (setq (, om) v)
	       (setcar (, om) (amark-cons 1 (amark-cdr u)))))))))

;;; The basic amark manipulation function.  I hope there's no pressing
;;; need for a binary search to find the cut-and-paste points.

(defun amark-block-mark-intern (j q om)
  "Mark articles > J and < Q in amark OM as read, using side effects.\n
Do not use if OM is null."
  (let ((i (1- j)) (k (1+ j)) (p (1- q)) (r (1+ q)))
    (cond ((< p k)
	   om)
	  ((null (car om))
	   (setcar om (amark-cons k p))
	   (setcdr om nil)
	   om)
	  ((< q (amark-car (car om)))
	   (setcdr om (cons (car om) (cdr om)))
	   (setcar om (amark-cons k p))
	   om)
	  ((> j (amark-cdr (nth (1- (length om)) om)))
	   (nconc om (amark-lcons k p)))
	  (t
	   (let ((o om) (a (car om)) b aa bb oo nn ka pb)
	     (while (< (amark-cdr a) j)
	       (setq nn o o (cdr o) bb a a (car o)))
	     (setq oo o aa a b bb)
	     (while (and a (< (amark-car a) r))
	       (setq o (cdr o) b a a (car o)))
	     (setq ka (min k (amark-car aa)) pb (max p (amark-cdr b)))
	     (if (and a bb (< (amark-cdr bb) j) (< q (amark-car a)))
		 (nconc (setcdr nn (amark-lcons ka pb)) o)
	       (setcar oo (amark-cons ka pb))
	       (setcdr oo o)))
	   om))))

;;; should be rewritten destructively--no pressing need though.

(defun amark-list-delete (j om)
  "Delete J from amark OM, ie, mark J as unread."
  (let ((i (1- j)) (k (1+ j)))
    (apply 'append
	   (mapcar '(lambda (w)
		      (let ((u (amark-car w)) (v (amark-cdr w)))
			(cond ((= u j)
			       (amark-lcons k v))
			      ((= j v)
			       (amark-lcons u i))
			      ((< u j)
			       (if (< j v)
				   (list (amark-cons u i) (amark-cons k v))
				 (list w)))
			      (t
			       (list w)))))
		   om))))

(defun amark-first-value (om)
  "Return smallest value in amark OM."
  (amark-car (car om)))

(defun amark-last-value (om)
  "Return largest value in amark OM."
  (amark-cdr (nth (1- (length om)) om)))

(defun amark-member (j om)
  "Return t if J is marked read by amark M."
  (memq t (mapcar '(lambda (w)
		     (and (<= (amark-car w) j)
			  (<= j (amark-cdr w))))
		  om)))

;;; should be rewritten destructively--no pressing need though.

(defun amark-list-unread (j om)
  "Return amark with J removed from amark OM, if present--for umarks,
the first J only is removed."
  (let* ((i (1- j)) (k (1+ j)) (a (car om)) (b (cdr om))
	 (u (amark-car a)) (v (amark-cdr a)) d olu)
    (if (and u (not b) (= u j) (= j v)) (setq a nil))
    (while (and a (not olu))
      (cond ((= j u)
	     (setq olu (append d (amark-lcons k v) b)))
	    ((< j v)
	     (setq olu (append d (amark-lcons u i) (amark-lcons k v) b)))
	    ((= j v)
	     (setq olu (append d (amark-lcons u i) b)))
	    (t
	     (setq d (append d (list a)) a (car b) b (cdr b)
		   u (amark-car a) v (amark-cdr a)))))
    (or olu d)))

(defun amark-next-unread (j om)
  "Return smallest unread value > J in amark OM."
  (let* ((k (1+ j)) (a (car om)) (b (cdr om))
	 (u (amark-car a)) (v (amark-cdr a)) onu)
    (while (and a (not onu))
      (cond ((< k u)
	     (setq onu k))
	    ((<= k v)
	     (setq onu (1+ v)))
	    (t
	     (setq a (car b) b (cdr b)
		   u (amark-car a) v (amark-cdr a)))))
    (or onu k)))

(defun amark-previous-unread (j om)
  "Return largest unread value < J in amark OM.  If this is negative,
return 0 instead."
  (let* ((i (1- j)) (a (car om)) (b (cdr om))
	 (u (amark-car a)) (v (amark-cdr a)) opu)
    (while (and a (not opu))
      (cond ((< i u)
	     (setq opu i))
	    ((<= i v)
	     (setq opu (1- u)))
	    (t
	     (setq a (car b) b (cdr b)
		   u (amark-car a) v (amark-cdr a)))))
    (max 0 (or opu i))))

;;; umarks are unordered amarks.

(defun umark-set-init (j m)
  "Return the umark obtained by initial truncation.  Ie, given value J
and umark M, a new umark with with initial value >= J is obtained by
\"cutting M off\" at J."
  (delq nil
	(mapcar '(lambda (x)
		   (amark-cons (max j (amark-car x)) (amark-cdr x)))
		m)))

(defun umark-set-last (j m)
  "Return umark obtained by final truncation.  Ie, given value J and
umark M, a new umark with with final value <= J is obtained by \"cutting
M off\" at J."
  (delq nil
	(mapcar '(lambda (x)
		   (amark-cons (amark-car x) (min j (amark-cdr x))))
		m)))

;;; miscellaneous functions

(defun gnews-flush ()
  "Flush typeahead if we're supposed to."
  (if (and gnews-flush-typeahead
	   (not defining-kbd-macro)
	   (not executing-macro))
      (discard-input)))

(defun gnews-message (&rest msg)
  (apply 'message (nconc msg '(" "))))

(defun gnews-version (&optional arg)
  "Version number of this release of Gnews.  Optional prefix ARG means
give version number of underlying NNTP."
  (interactive "P")
  (cond (arg (if (interactive-p)
		 (message "NNTP version %s" nntp-version)
	       nntp-version))
	(t (if (interactive-p)
	       (message "Gnews version %s" gnews-version)
	     gnews-version))))

(defun article-msg-id-after-point ()			; AR
  "Return the message-ID located after point."
  (save-excursion
    (if (save-excursion
	  (and (search-backward "<"
				(save-excursion
				  (search-backward " " (point-min) t)
				  (point))
				t)
	       (looking-at "<[^ >]+>")))
	(gnews-replace "\n" "" (gnews-match 0))
      (if (re-search-forward "<[^ >]+>" nil t)
	  (gnews-replace "\n" ""
	    (buffer-substring (save-excursion (search-backward "<") (point))
			      (point)))))))

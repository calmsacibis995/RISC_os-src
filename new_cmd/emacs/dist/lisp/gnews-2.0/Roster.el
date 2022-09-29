;;; Roster.el: roster-mode commands for Gnews
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

;;; NOTE: roster-mode is only a stub of a mode.  I work on it now and then.

; >> Suggestions as to display formats and the like are most welcome. <<

;;; roster-mode primitives

(defvar roster-abstract-format "  {%s}")
(defvar roster-bogus "    * bogus *"
  "*String for indicating bogus newsgroups.")
(defvar roster-format (format roster-abstract-format "%d")
  "*Format for displaying roster")
(defvar roster-size 35
  "*Maximum size for newsgroup name.  nil means no maximum.")
(defvar roster-pop nil
  "*Non-nil means pop into roster-buffer, nil means switch into it")

(defvar roster-show-zero nil
  "*Show newsgroups with zero unread articles.")
(defvar roster-show-bogus nil
  "*Show newsgroups that are bogus.")

(if roster-mode-map nil
  (setq roster-mode-map (make-sparse-keymap))
  (gnews-key-bind roster-mode-map
		  '(("n".roster-forward)
		    ("p".roster-backward)
		    (" ".roster-scroll-up)
		    ("\t".roster-scroll-down)
		    ("/".roster-search)
		    ("u".roster-unsubscribe)
		    ("g".roster-group-get)
		    ("v".roster-group-visit)
		    ("c".roster-catchup)
		    ("q".roster-quit)
		    ("a".universal-argument)
		    ("<".roster-beginning-of-buffer)
		    (">".roster-end-of-buffer)
		    ("!".shell)
		    ("h".describe-mode)
		    ("H".gnews-describe-mode)))
  (mapcar '(lambda (x)
	     (define-key roster-mode-map (concat x) 'digit-argument))
	  '(0 1 2 3 4 5 6 7 8 9 "-")))

(defun roster-mode ()
  "Roster-mode is used by Gnews for viewing a roster to the newsgroups.\n
Commands are:
\\{roster-mode-map}"
  (interactive)
  (setq major-mode 'roster-mode
	mode-name "Roster"
	gnews-mode-string ""
	gnews-read-p nil
	gnews-hook-p nil)
  (use-local-map roster-mode-map)
  (gnews-set-mode-line)
  (run-hooks 'roster-hook))

;;; the roster display

(defun roster-display (unsub roster)
  "Display ROSTER.  If UNSUB is non-nil, more is displayed"
  (setq roster-roster roster)
  (gnews-buffer roster-pop roster-buffer)
  (setq buffer-read-only)
  (erase-buffer)				;temporary
  (sit-for 0)
  (roster-mode)
  (mapcar '(lambda (g) (if (or unsub (gnadr g)) (roster-display-group g)))
	  roster)
  (delete-backward-char 1)
  (setq buffer-read-only t)
  (if (roster-search group-current t) nil
    (roster-beginning-of-buffer))
  (gnews-flush))

(defun roster-display-group (group)
  (if (and (roster-string-new-news group)
	   group-not-bogus group-not-empty)
      (roster-display-group-nntp group)
    (let ((sub (gnadr group)))
      (if (if group-not-bogus roster-show-zero roster-show-bogus)
	  (progn
	    (insert (if sub "   " " ! ")
		    (if roster-size
			(substring (concat (car group)
					   (make-string roster-size ? ))
				   0 roster-size)
		      (car group))
		    " " (if group-not-bogus
			    (format roster-format 0)
			  roster-bogus)
		    ?\n)
	    (sit-for 0))))))

(defun roster-display-group-nntp (group)
  "Display the roster information about GROUP entry based on NNTP."
  (let* ((genuine (nntp-exec t t "group" (car group)))
	 (sub (gnadr group))
	 (count (if genuine
		    (let* ((cc (read-from-string nntp-info 4))
			   (ii (read-from-string nntp-info (cdr cc)))
			   (ll (read-from-string nntp-info (cdr ii)))
			   (i (car ii))
			   (l (car ll))
			   (m (gnddr group))
			   (z (amark-list-init i m))
			   (c (apply '- l 0 (mapcar 'amark-size m))))
		      (max 0 c)))))
    (if (if genuine
	    (or (< 0 count) roster-show-zero)
	  roster-show-bogus)
	(progn
	  (insert (if sub "   " " ! ")
		  (if roster-size
		      (substring (concat (car group)
					 (make-string roster-size ? ))
				 0 roster-size)
		    (car group))
		  " "
		  (if genuine
		      (format roster-format count)
		    roster-bogus)
		  ?\n)
	  (sit-for 0)))))

(defun roster-group-name ()
  "Return the name of the newsgroup on the current line"
  (save-excursion
    (beginning-of-line)
    (let ((p (+ 3 (point)))
	  (q (progn (forward-char 3)
		    (re-search-forward "[^ ] ")
		    (1- (point)))))
      (buffer-substring p q))))

(defun roster-redisplay-group (g n)
  (if (roster-search g t)
      (progn
	(re-search-forward "{[0-9]+}")
	(setq buffer-read-only)
	(replace-match (concat "{" n "}"))
	(setq buffer-read-only t)
	(beginning-of-line))))

;;; roster-mode commands

(defun roster-forward (arg)
  "Move forward ARG lines, ignoring the unsubscribed ones"
  (interactive "p")
  (beginning-of-line)
  (if (< arg 0) (roster-backward (- arg)))
  (while (and (not (eobp)) (< 0 arg))
    (next-line 1)
    (while (and (not (looking-at "  ")) (not (eobp)))
      (forward-line 1))
    (setq arg (1- arg)))
  (if (eobp)
      (progn
	(roster-backward 1)
	(error "last subscribed-to newsgroup")))
  (gnews-flush))

(defun roster-backward (arg)
  "Move backward ARG lines, ignoring the unsubscribed ones"
  (interactive "p")
  (beginning-of-line)
  (if (bobp)
      (error "beginning of buffer")
    (if (< arg 0) (roster-forward (- arg)))
    (while (and (not (bobp)) (< 0 arg))
      (previous-line 1)
      (while (and (not (looking-at "  ")) (not (bobp)))
	(forward-line -1))
      (setq arg (1- arg)))
    (if (and (bobp) (not (looking-at "  ")))
	(progn
	  (roster-forward 1)
	  (error "first subscribed-to newsgroup"))))
  (gnews-flush))

(defun roster-scroll-up (pfx arg)
  "Scroll up in the roster buffer"
  (interactive "P\np")
  (scroll-up (if pfx arg))
  (gnews-flush))

(defun roster-scroll-down (pfx arg)
  "Scroll up in the roster buffer"
  (interactive "P\np")
  (scroll-down (if pfx arg))
  (gnews-flush))

(defun roster-beginning-of-buffer ()
  "Move to the beginning of the roster buffer"
  (interactive)
  (goto-char 1)
  (beginning-of-line)
  (gnews-flush))

(defun roster-end-of-buffer ()
  "Move to the end of the roster buffer"
  (interactive)
  (goto-char (point-max))
  (beginning-of-line)
  (gnews-flush))

(defun roster-search (group &optional silent)
  "Search for GROUP.  Return t if found, nil if not."
  (interactive (list (group-name-read "Search for: " roster-roster 'news-all)))
  (let ((rg (concat "\\ " (regexp-quote group) "\\ ")))
    (if (re-search-forward rg nil t) (not (beginning-of-line))
      (if (re-search-backward rg nil t) (not (beginning-of-line))
	(or silent (message "%s: group not found" group))
	nil))))

(defun roster-group-get ()
  (interactive)
  (group-get (roster-group-name))
  (gnews-flush))

(defun roster-group-visit ()
  (interactive)
  (group-get (roster-group-name) t)
  (gnews-flush))

(defun roster-catchup (group ask pfx arg)
  "Catchup in the roster mode.\n
In Lisp code, roster-catchup is the basic catchup function, and takes
arguments GROUP naming the newsgroup to catchup, non-nil ASK meaning
to query the user to catch up, and PFX and ARG, the literal and numeric
prefix arguments.  roster-catchup returns the number of articles to
leave marked."
  (interactive (list (roster-group-name) t current-prefix-arg
		     (prefix-numeric-value current-prefix-arg)))
  (if (interactive-p) (group-set group))
  (if (if ask (y-or-n-p (format "catch up in %s? " group)) t)
      (let* ((p (cond ((null pfx) 0)
		      ((zerop arg)
		       (if (memq major-mode '(article-mode group-mode))
			   (- article-final article-current)
			 (error "Not in group-mode")))
		      ((numberp pfx) pfx)
		      ((listp pfx) (* 10 (gnews-arg-count arg)))
		      (t 0)))
	     (gp (assoc group group-roster))
	     (sub (gnadr gp))
	     (cu (- article-final p))
	     (gm (amark-cons 1 cu)))
	(message "")
	(setcdr gp (if gm (list sub gm) (list sub)))
	(setq amark (list gm))
	(article-current-set (1+ cu))
	(setq gnews-rc-dangle t)
	(gnews-flush)
	(if (interactive-p) (roster-redisplay-group group p))
	cu)
    (message "")
    (gnews-flush)
    0))

(defun roster-unsubscribe (group)
  "Unsubscribe from GROUP.  In roster mode, GROUP is the current one
whose line point is on."
  (interactive (roster-group-name))
  (if (y-or-n-p (concat "Unsubscribe from " group "? "))
      (progn
	(setcar (cdr (assoc group group-roster)) nil)
	(group-roster-write gnews-rc-file-new)
	(message "")
	(gnews-flush)
	t)
    (gnews-flush)
    nil))

(defun roster-quit ()
  (interactive)
  (switch-to-buffer news-buffer))

;;; heh heh heh

(defvar gnews-qbpgbe-name nil)

(defun gnews-qbpgbe (pfx)
  "Post high quality followups in high quality newsgroups."
  (interactive "P")
  (if (or (string-match "alk.biz" (article-field "Newsgroups"))
	  (string-match "lt.flam" (article-field "Newsgroups"))
	  (string-match "lt.birt" (article-field "Newsgroups")))
      (let* ((qbpgbe (mapconcat 'char-to-string
				(mapcar '(lambda (c) (aref gnews-rot13-map c))
					"qbpgbe")
				""))
	     (gnews-name (or gnews-qbpgbe-name
			     (concat "thats" (upcase qbpgbe) "toyoubuddy")))
	     (qbpgbe-buffer (concat "*" qbpgbe "*"))
	     (qbpgbe-func-1 (intern qbpgbe))
	     (qbpgbe-func-2 (intern (concat qbpgbe "-ret-or-read")))
	     s)
	(random t)
	(gnews-set 'gnews-advertise t)
	(if (not (boundp 'yow-vector)) (load-library "yow"))
	(if (and pfx (not (fboundp 'flame2))) (load-library "flame"))
	(if (null yow-vector) (setq yow-vector (snarf-yows)))
	(funcall qbpgbe-func-1)
	(erase-buffer)
	(message "")
	(group-follow-yank nil 1)
	(goto-char 1)
	(forward-paragraph 1)
	(while (and (not (input-pending-p)) (not (eobp)))
	  (setq s (buffer-substring
		    (point) (progn (forward-sentence 1) (point))))
	  (set-buffer qbpgbe-buffer)
	  (if (string-match "[^ \t\n]+" s)
	      (progn
		(insert-string s)
		(funcall qbpgbe-func-2 1)
		(cond (pfx
			(funcall qbpgbe-func-2 1)
			(flame2 (+ 1 (logand (random) 3)))))
		(funcall qbpgbe-func-2 1)))
	  (set-buffer reply-buffer-name))
	(goto-char 1)
	(re-search-forward "^Subject: ")
	(gnews-delete-line)
	(insert (let ((y "\n"))
		  (while (string-match "\n" y) (setq y (yow))) y))
	(insert ?\n)
	(re-search-forward "^Posting-Front-End: ")
	(end-of-line)
	(insert " goes to the " qbpgbe (if pfx " and gets angry" ""))
	(forward-paragraph 1)
	(delete-region (point) (point-max))
	(insert-buffer qbpgbe-buffer)
	(while (re-search-forward "\n\n\n+" nil t)
	  (replace-match "\n\n"))
	(goto-char 1)
	(while (re-search-forward "^  " nil t)
	  (replace-match reply-prefix))
	(reply-return 1))
    (message "You can't talk about PHIL DONAHUE in this newsgroup!!" (ding))))

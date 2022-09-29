;;; art.el: pager-mode commands for Gnews
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

(defun article-scroll-up (arg &optional ff digest)
  "Scroll window forward upward ARG lines, or nearly a full screen if no
ARG.  Catch end of buffer errors.\n
Optional second argument FF, if non-nil, blocks an incorrect second call
to article-forward-intern."
  (condition-case ()
      (progn
	(scroll-up arg)
	(if (and article-grab-point
		 (not (if ff (save-excursion (goto-char article-grab-point)
					     (beginning-of-line)
					     (looking-at article-formfeed))))
		 (pos-visible-in-window-p article-grab-point))
	    (article-forward-intern nil ff digest)))
    (end-of-buffer (if (or gnews-edit-p
			   (= (read (article-field "Lines"))
			      (1+ (count-lines (article-min) (point-max)))))
		       (setq article-count-off-but-ok t)
		     (error "Truncated article")))))

(defun article-forward (&optional arg)
  "Scroll the current article forward one page.\n
In Lisp code (only), optional argument ARG means scroll forward that
many lines.  Interactively, use article-line, which see."
  (interactive (list nil))
  (let ((agp article-grab-point)
	(ff article-formfeed)
	(top article-formfeed-top)
	(digest article-formfeed-post))
    (if (and agp (pos-visible-in-window-p agp))
	(progn (article-forward-intern nil ff digest)
	       (if (pos-visible-in-window-p			 ; Check for
		     (save-excursion				 ; non-null
		       (goto-char agp)				 ; reading
		       (re-search-forward "[^ \n\t\f]" nil t)	 ; material:
		       (point)))				 ; if so, then
		   nil						 ; leave alone
		 (article-scroll-up arg ff digest))		 ; else scroll.
	       (if article-formfeed-top				 ; And then
		   (progn (goto-char agp)			 ; adjust the
			  (beginning-of-line)			 ; top of the
			  (if (looking-at ff) (recenter top))))) ; if needed
      (article-scroll-up arg ff digest))
    (article-%-compute)
    (if (article-done) (article-quit))
    (gnews-flush)))

(defun article-down ()
  "Scroll the current article down half a page."
  (interactive)
  (article-forward (/ (window-height) 2)))

(defun article-line (arg)
  "Scroll the current article up one line--with numeric argument ARG,
scroll that many lines."
  (interactive "p")
  (if (article-done)
      (ding)
    (article-forward arg)))

(defun article-quit ()
  "Quit the current article."
  (interactive)
  (article-forward-intern t)
  (if article-junkable (article-junk))
  (gnews-message (concat "End of article " article-current
			 "--what next " group-prompt-default
			 (if (eq article-current article-final) " $ " " ? ")))
  (group-mode)
  (gnews-flush)
  t)

(defun article-junk (&optional pfx art-no super)
  "Mark current article as junked.\n
In Lisp code, non-nil optional argument PFX (the prefix argument) means
do a local junk, and non-nil argument ART-NO is the article number to
junk.  Non-nil argument SUPER means due a superjunk: add this message-ID
to this newsgroup's hook-kills: temporary if SUPER is numeric, temporary
and permanent if t."
  (interactive "P")
  (setq hook-kill-continue)			; terminate hook processing
  (if (and (eq article-junkable 'mark) (not (interactive-p))) nil
    (if (and (not pfx) art-no)
	(let ((b (current-buffer)))
	  (save-excursion
	    (set-buffer nntp-buffer)
	    (nntp-exec t t "head" art-no)
	    (article-header-clean t))
	  (set-buffer b)))
    (setcdr (cdr (assoc group-current group-roster)) amark)
    (let ((xgp (list nil))
	  (xref (list nil))
	  (str (if (or pfx (string-match "<0>" nntp-info))
		   "" (if article-field-list
			  (article-field "Xref")	; from a read article
			(save-excursion			; from a per-hook junk
			  (set-buffer nntp-buffer)
			  (goto-char (point-max))
			  (if (re-search-backward "^Xref: \\(.*\\)" nil t)
			      (gnews-match 1)		; cross-posted
			    "")))))			; not cross-posted
	  (mid (if super
		   (if article-field-list
		       (article-field "Message-ID")
		     (save-excursion
		       (set-buffer nntp-buffer)
		       (goto-char 1)
		       (re-search-forward "^Message-ID: \\(.*\\)" nil t)
		       (gnews-match 1)))))
	  ss)
      (if (string= str "")
	  (setq xref (list nil (or art-no article-current))
		xgp (list nil group-current))
	(while (string-match "\\([^ :]+\\):\\([0-9]+\\)" str ss)
	  (nconc xgp (list (substring
			     str (match-beginning 1) (match-end 1))))
	  (nconc xref (list (string-to-int
			      (substring
				str (match-beginning 2) (match-end 2)))))
	  (setq ss (match-end 0))))
      (gnews-map (function
		   (lambda (x y)
		     (let* ((ngl (assoc x group-roster))
			    (ngam (gnddr ngl)))
		       (if (and ngl (or article-junk-unsub (gnadr ngl)))
			   (if ngam (amark-list-insert y ngam)
			     (nconc ngl (list y)))))))
	(cdr xgp)
	(cdr xref)))
    (setq group-checkpoint t
	  amark (gnddr (assoc group-current group-roster)))
    (if (and (boundp 'index-pop)		; index-mode is around
	     (not nntp-index-p)			; and we're not killing
	     (null art-no))			; but this was direct
	(let ((b (current-buffer)))
	  (save-excursion
	    (set-buffer index-buffer)
	    (goto-char 1)
	    (if (re-search-forward
		  (concat "^\\(^ *" article-current "\\).") nil t)
		(index-junk-line nil)))
	  (set-buffer b)))
    (if (interactive-p) (message "%d: junked" (or art-no article-current))))
  (gnews-flush))

(defun article-junk-local ()
  "Mark current article as junked in current newsgroup only."
  (interactive)
  (article-junk t)
  (if (interactive-p) (message "%d: junked" article-current))
  (gnews-flush))

(defun article-ignore () "Ignore this article" t)

(defun article-yes ()
  "For use within hook-kill processing: break out and set to the article."
  (setq hook-kill-continue))

(put 'article-junk 'hook-kill-junk t)
(put 'article-junk-local 'hook-kill-junk t)
(put 'article-ignore 'hook-kill-junk t)

(defun article-restart ()
  "Restart the current article."
  (interactive)
  (if (< article-final article-current)
      (ding)
    (setq buffer-read-only)
    (article-header-clean nil)
    (setq buffer-read-only t)
    (article-mode)
    (goto-char 1)
    (article-%-compute))
  (gnews-hilite)
  (gnews-flush))

(defun article-restart-verbose ()
  "Restart the current article with full headers."
  (interactive)
  (if (< article-final article-current)
      (ding)
    (article-forward-intern nil)
    (goto-char 1)
    (setq buffer-read-only)
    (gnews-delete-paragraph)
    (mapcar '(lambda (h) (insert (car h) ": " (cdr h) ?\n))
	    (cdr article-field-list))
    (goto-char 1)
    (run-hooks 'article-header-hook)
    (goto-char 1)
    (setq buffer-read-only t)
    (article-%-compute)
    (if (article-done) (article-quit) (article-mode))
    (goto-char 1))
  (gnews-flush))

(defun article-restart-reset ()
  "Completely restart the current article."
  (interactive)
  (cond ((< article-final article-current)
	 (ding))
	((and (< 0 article-current)
	      (catch 'article-nil (article-get article-current)))
	 (article-junk-local)
	 (message "whoops--this article was cancelled"))
	((= 0 article-current)
	 (article-get-msg-id (article-field "Message-ID"))))
  (gnews-flush))

(defun article-rot13 ()
  "Rot13 the current article."
  (interactive)
  (if (< article-final article-current)
      (ding)
    (setq buffer-read-only)
    (save-excursion
      (goto-char (article-min))
      (gnews-rot13 (article-min) (article-max)))
    (setq buffer-read-only t
	  gnews-rot13-p (not gnews-rot13-p))
    (article-%-compute)
    ;; note: for a-r13-r, we must return !nil if a-q
    (if (article-done) (article-quit))))

(defun article-rot13-restart ()
  "Restart and rot13 the current article."
  (interactive)
  (let (gnews-rot13-p) (article-restart))	; shield for sake of a-r13
  (article-rot13))

(defun article-rot13-forward ()
  "Scroll forward and rot13 the current article."
  (interactive)
  (or (article-rot13) (article-forward)))

(defun article-downcase ()
  "Lowercase the current article."
  (interactive)
  (if (< article-final article-current)
      (ding)
    (article-forward-intern t)
    (setq buffer-read-only)
    (save-excursion 
      (downcase-region (article-min) (article-max)))
    (setq buffer-read-only t gnews-edit-p t)
    (if (article-done) (article-quit)))
  (gnews-flush))

(defun article-ununderline ()
  "Remove underlining from the current article."
  (interactive)
  (if (< article-final article-current)
      (ding)
    (article-forward-intern t)
    (setq buffer-read-only)
    (save-excursion 
      (ununderline-region (article-min) (article-max)))
    (setq buffer-read-only t gnews-edit-p t)
    (if (article-done) (article-quit)))) ; must return !nil if quit

(defun article-back ()
  "Scroll back a page in the current article."
  (interactive)
  (if (< article-final article-current)
      (ding)
    (scroll-down nil)
    (article-%-compute))
  (move-to-window-line 0)
  (gnews-hilite)
  (gnews-flush))

(defun article-back-half ()
  "Scroll back half a page in the current article."
  (interactive)
  (if (< article-final article-current)
      (ding)
    (scroll-down (/ (window-height) 2))
    (article-%-compute)
    (if (article-done) (article-quit)))
  (move-to-window-line 0)
  (gnews-hilite)
  (gnews-flush))

(defun article-end ()
  "Go to the end of the current article."
  (interactive)
  (article-forward-intern nil)
  (end-of-buffer)
  (recenter (- (window-height) 1))
  (move-to-window-line -1)
  (sit-for 0)
  (article-%-compute)
  (if (article-done) (article-quit))
  (gnews-flush))

(defun article-grep (pfx)
  "Search for a regexp in the article--non-nil prefix argument PFX
means search backwards."
  (interactive "P")
  (article-forward-intern nil)
  (setq article-grep-directions '(re-search-forward . re-search-backward))
  (if pfx (setq article-grep-directions (nreverse article-grep-directions)))
  (funcall (car article-grep-directions) 
	   (setq article-grep-string (or (read-string "grep for: ")
					 article-grep-string))
	   nil t)
  (recenter article-grep-height)
  (article-%-compute)
  (if (article-done) (article-quit))
  (gnews-flush))

(defun article-grep-repeat (pfx)
  "Repeat previous regexp search--non-nil prefix argument PFX means
reverse search direction."
  (interactive "P")
  (if (null article-grep-string) (article-grep pfx)
    (article-forward-intern nil)
    (if pfx (setq article-grep-directions (nreverse article-grep-directions)))
    (funcall (car article-grep-directions)
	     article-grep-string nil t)
    (recenter article-grep-height)
    (article-%-compute)
    (if (article-done) (article-quit))
    (gnews-flush)))

(defun article-grep-digest (pfx)
  "Search for a digest separator--non-nil prefix argument PFX means
search backwards."
  (interactive "P")
  (article-forward-intern nil)
  (if pfx nil (forward-line 1))
  (funcall (if pfx 're-search-backward 're-search-forward)
	   article-digest-separator nil t)
  (recenter 0)
  (beginning-of-line)
  (article-%-compute)
  (if (article-done) (article-quit))
  (gnews-flush))

(defun article-skip-indent ()
  "Skip past indentation, ie, the first character on the bottom line"
  (interactive)
  (move-to-window-line -1)
  (beginning-of-line)
  (re-search-forward
    (concat "^[^" (regexp-quote
		    (char-to-string
		      (following-char))) "]") nil t)
  (beginning-of-line)
  (article-forward-intern nil)
  (recenter article-grep-height)
  (article-%-compute)
  (if (article-done) (article-quit))
  (gnews-flush))

(defun article-recenter (&optional pfx arg)
  "Recenter the current article, as in recenter.\n
In Lisp code, if optional PFX is non-nil, then ARG is passed to recenter."
  (interactive "P\np")
  (if (< article-final article-current)
      (ding)
    (recenter (cond (pfx arg)
		    ((interactive-p) nil)
		    (t 0)))
    (article-%-compute)
    (if (article-done) (article-quit)))
  (gnews-hilite)
  (gnews-flush))

(defun article-isearch-forward ()
  (interactive)
  (article-forward-intern nil)
  (isearch t)
  (article-%-compute))

(defun article-isearch-backward ()
  (interactive)
  (article-forward-intern nil)
  (isearch nil)
  (article-%-compute))

(defun article-isearch-forward-regexp ()
  (interactive)
  (article-forward-intern nil)
  (isearch t t)
  (article-%-compute))

(defun article-isearch-backward-regexp ()
  (interactive)
  (article-forward-intern nil)
  (isearch nil t)
  (article-%-compute))

;;; the basic article getting primitives

;;; article-get's complexity comes from its attempt to be quick and clever.

;;; First, only the head is gotten, permitting quick checks for hook-kill
;;; purposes.  If the article passes, then the body is gotten too.  But we
;;; do a little buffering along the way, only grabbing enough of the body
;;; to get a quick display up.  If the length of the article is greater
;;; than article-big lines, then we do a full display, even if the user
;;; by default asks for just a partial display--ie, just give him something
;;; to read while waiting.  Also, the *first* character hit by a user while
;;; waiting is processed--normally all typeahead is flushed.

;;; article-get-slow is not actually that slow (except for large articles)
;;; it just has none of the above sophistication.  Being simpler, it should
;;; be less error prone, but this is counterbalanced by the fact that I work
;;; primarily with/on article-get.  At the moment, I still must use it in
;;; forward/backward pattern searches and with the spool code.  Both needs
;;; are a mystery to me.

(defun article-get (number &optional hooklist interact)
  "Display article NUMBER of the current newsgroup.\n
In Lisp code, optional argument HOOKLIST is a list of per-hooks to
apply, and a non-nil INTERACT means pretend this function was called
interactively."
  (interactive "narticle #: ")
  (setq gnews-edit-p nil
	gnews-rot13-p nil
	interact (or (interactive-p) interact))
  (if (< article-final number)
      (group-last)
    (if (nntp-exec t t "head" number)
	(let ((b (current-buffer))
	      (hook-kill-continue t)
	      lines)
	  (set-buffer nntp-buffer)
	  (if interact (article-current-set number))
	  (setq article-field-list)
	  (while (and hooklist hook-kill-continue)
	    (if (hook-kill-do (car hooklist) nil)
		(progn				; hook-kill-junk property
		  (hook-kill-junk-message number (car hooklist))
		  (set-buffer b)		; I have to doooo this?
		  (throw 'article-nil t)))	; article KILLed; try again
	    (setq hooklist (cdr hooklist)))
	  (article-header-clean t)
	  (if interact
	      (article-history-append number group-current
				      (article-field "Message-ID")))
	  (setq lines (article-effective-init-display))
	  (nntp-exec nil nil "body" number)
	  (while (and (nntp-run-p)		; catch a broken connection
		      (not nntp-eot)
		      (< (count-lines 1 (point-max))
			 (or lines
			     (window-height
			       (get-buffer-window news-buffer)))))
	    (gnews-accept-process-output nntp))
	  (if (not (nntp-run-p))
	      (news-quit
		(y-or-n-p "Connection died: save the current roster? ")))
	  (save-excursion			; get rid of NNTP info
	    (goto-char 1)
	    (re-search-forward "^222.*$")	; the NNTP info message
	    (setq nntp-info-true (gnews-match 0))
	    (replace-match ""))
	  (if lines (forward-line lines))
	  (setq article-grab-point (if lines (point)))
	  (article-display-init)
	  (nntp-finish)
	  (set-buffer nntp-buffer)
	  (goto-char 1)
	  (insert nntp-info ?\n)		; put back first field
	  (setq nntp-info nntp-info-true)
	  (set-buffer news-buffer)
	  (gnews-exec-1-pending)		; get one out before flushing
	  (if (setq article-junkable (article-done)) (article-quit)))
      (or (article-run-hooks number hooklist interact 'article-not-found-hooks)
	  (throw 'article-nil t))))		; article not found--give up
  (gnews-flush))

(defun article-get-msg-id (msg-id)
   "Display the article with message-ID MSG-ID.  Interactively, the
first message-ID that ends at point or after, if it exists, is offered
as a default choice.\n
The enclosing angle brackets are optional."
   (interactive
     (list (read-string "Message-ID: " (article-msg-id-after-point))))
   (setq gnews-edit-p)
   (if (string= (substring msg-id 0 1) "<") nil
     (setq msg-id (concat "<" msg-id)))
   (if (string= (substring msg-id -1) ">") nil
     (setq msg-id (concat msg-id ">")))
   (setq article-field-list-previous article-field-list)
   (if (nntp-exec t t "article" msg-id)
       (let (lines)
	 (set-buffer nntp-buffer)
	 (article-header-clean t)
	 (setq lines (article-effective-init-display))
	 (if lines (forward-line lines))
	 (setq article-grab-point (if lines (point)))
	 (if (cdr article-field-list)
	     (progn
	       (if (< 0 article-current)
		   (setq article-message-id msg-id
			 article-trace article-current
			 article-current 0)) ; NNTP can't return #/gp
	       (article-display-init t))
	   (message "Message-ID %s: no such article" msg-id (ding))
	   (setq article-field-list article-field-list-previous)))
     (or (article-run-hooks msg-id 0 t 'article-not-found-hooks)
	 (message "Message-ID %s: no such article" msg-id (ding))
	 (setq article-field-list article-field-list-previous)))
   (gnews-flush))

(defun article-get-slow (number &optional hook interact)
  "Display article NUMBER of the current newsgroup.\n
In Lisp code, optional argument HOOK is a list of per-hooks to apply, and
non-nil INTERACT means to pretend this function was called interactively.\n"
  (interactive "narticle #: ")
  (setq gnews-edit-p nil
	gnews-rot13-p nil
	interact (or (interactive-p) interact))
  (if (< article-final number)
      (group-last)
    (if (nntp-exec t t "article" number)
	(let ((b (current-buffer))
	      lines)
	  (set-buffer nntp-buffer)
	  (if interact (article-current-set number))
	  (article-header-clean t)
	  (if interact
	      (article-history-append number group-current
				      (article-field "Message-ID")))
	  (setq lines (article-effective-init-display))
	  (let ((article-current number)
		(hook-kill-continue t))
	    (while (and hook hook-kill-continue)
	      (if (hook-kill-do (car hook) t)
		  (progn
		    (hook-kill-junk-message number hook)
		    (set-buffer b) ; I have to doooo this?
		    (throw 'article-nil t)))	; article KILLed; try again
	      (setq hook (cdr hook))))
	  (if lines (forward-line lines))
	  (setq article-grab-point (if lines (point)))
	  (article-display-init t)
	  (if (setq article-junkable (article-done)) (article-quit)))
      (or (article-run-hooks number hooklist interact 'article-not-found-hooks)
	  (throw 'article-nil t))))		; article not found--give it up
  (gnews-flush))

(defun article-run-hooks (art-no kills interact hooks)
  "Run an article-fetching hook or through a list of article-fetchings
hooks until one of them returns non-nil.\n
This is used by the article-get-* commands to permit non-standard news
article fetching to intermix with the NNTP.  If a hook returns non-nil,
it means that Gnews is now set to the indicated article, and is ready
for the user to read the article.\n
Arguments are ART-NO, the article number or message-id of interest,
KILLS, the hook-kills that are applicable, INTERACT, non-nil if the call
is to be treated as if it were interactive, and HOOKS, the hook or list
of hooks to run through.\n
The article Message-ID's case uses 0 for KILLS, since there are no kills
associated with article-get-msg-id.\n
Each hook function should take three arguments: ART-NO, KILLS, INTERACT."
  (setq hooks (cond ((fboundp hooks) (list hooks))
		    ((boundp hooks) (eval hooks))))
  (let ((hook (car hooks)) found)
    (while (and hooks (not found))
      (if (fboundp hook)
	  (setq found (funcall hook art-no kills interact)))
      (setq hooks (cdr hooks) hook (car hooks)))
    found))

;;; Gnews Edit mode

;;; Edit the display of the current article.  The article itself is
;;; of course unaffected, and any changes will be lost upon exiting
;;; the current article.  Useful for fixing things that don't quite
;;; match the expectations of your own article filters and the like.

;;; Does not change any internals, like article-field-list.

(if gnews-edit-mode-map nil
  (setq gnews-edit-mode-map (gnews-copy-keymap text-mode-map))
  (gnews-key-bind gnews-edit-mode-map
		  '(("\^c\^c".gnews-edit-exit)
		    ("\^c\^]".gnews-edit-abort)
		    ("\^c\^r".gnews-edit-rot13)
		    ("\^c?".describe-mode)
		    ("\^c\^h".gnews-describe-mode))))

(defun gnews-edit-mode ()
  "Mode to edit the current article with.  It is Text mode with a few
extra commands:\n\\<gnews-edit-mode-map>
\t\\[gnews-edit-exit] to implement the changes and return, and
\t\\[gnews-edit-abort] to ignore the changes and return.\n
\t\\[gnews-edit-rot13] to rot13 regions of text.\n
Also, there is help via:\n
\t\\[describe-mode] to summarize this mode's commands, and
\t\\[gnews-describe-mode] to describe this mode's commands.\n"
  (interactive)
  (text-mode)
  (use-local-map gnews-edit-mode-map)
  (make-local-variable 'gnews-edit-p)
  (setq major-mode 'gnews-edit-mode
	mode-name "Gnews Edit"
	gnews-edit-p nil
	gnews-read-p nil
	gnews-hook-p t)
  (run-hooks 'text-mode-hook 'gnews-edit-hook))

(defun article-edit ()
  "Edit the contents of the current article."
  (interactive)
  (article-forward-intern nil)
  (setq gnews-pre-edit-text (buffer-substring (point-min) (point-max))
	gnews-pre-edit-point (point)
	buffer-read-only nil)
  (gnews-edit-mode)
  (gnews-set-mode-line)
  (message "Use %s to implement changes, %s to abort"
	   (if (eq (key-binding "\C-c\C-c") 'gnews-edit-exit) "C-c C-c"
	     (substitute-command-keys "\\[gnews-edit-exit]"))
	   (if (eq (key-binding "\C-c\C-]") 'gnews-edit-abort) "C-c C-]"
	     (substitute-command-keys "\\[gnews-edit-abort]"))))

(defun gnews-edit-rot13 (beg end)
  "Rot13 the region."
  (interactive "*r")
  (gnews-rot13 beg end))

(defun gnews-edit-exit (edp)
  "Return to group/pager mode, with changes in effect."
  (interactive (list t))
  (setq buffer-read-only t
	gnews-edit-p (or gnews-edit-p	; once edited, always edited
			 (and edp (not (eq last-command 'article-edit)))))
  (article-mode)
  (article-%-compute)
  (if (article-done) (article-quit)))

(defun gnews-edit-abort ()
  "Return to group/pager mode, with changes ignored."
  (interactive)
  (delete-region (point-min) (point-max))
  (insert gnews-pre-edit-text)
  (goto-char gnews-pre-edit-point)
  (gnews-edit-exit nil))

;;; Mail box saving

;;; If there are any other styles worth emulating, write your own, and
;;; let me know about it.  A saver is invoked only if gnews-save-style
;;; is set to the name of the saver function.

(defun gnews-output-to-rmail-file (file-name)
  "Append the current article to an Rmail file named FILE-NAME.
If the file does not exist, ask if it should be created.
If file is being visited, the message is appended to the Emacs
buffer visiting that file."
  ;; full header handling added by unido!pbinfo!michael (Michael Schmidt)
  (interactive (list (read-file-name
		       (concat "Rmail save: (default "
			       (file-name-nondirectory group-last-save)
			       ") ")
		       (file-name-directory group-last-save)
		       (if (file-directory-p group-last-save)
			   (concat group-last-save article-current)
			 group-last-save))))
  (require 'rmail)
  (if article-grab-point (article-forward-intern group-save-junk))
  (setq file-name (expand-file-name file-name)
	group-last-save file-name)
  (or (get-file-buffer file-name)
      (file-exists-p file-name)
      (if (y-or-n-p
	    (concat "\"" file-name "\" does not exist, create it? "))
	  (let ((file-buffer (create-file-buffer file-name)))
	    (save-excursion
	      (set-buffer file-buffer)
	      (rmail-insert-rmail-file-header)
	      (let ((require-final-newline nil))
		(write-region (point-min) (point-max) file-name t 1)))
	    (kill-buffer file-buffer))
	(error "Output file does not exist")))
  (save-restriction
    (widen)
    ;; Decide whether to append to a file or to an Emacs buffer.
    (save-excursion
      (let ((buf (get-file-buffer file-name))
	    (cur (current-buffer))
	    (from (reply-domain (article-field "From")))
	    (time (current-time-string))
	    (beg 1)
	    (end 1)
	    (buffer-read-only nil))
	;; now fake some Babyl
	(goto-char 1)
	(insert "\f\n1,,\n")
	(insert (format "Summary-line: %2d-%3s  %25s  #%s\n"
			(string-to-int (substring time 11 13))
			(substring time 4 7)
			(if (< (length from) 26) from (substring from 0 25))
			(article-field "Subject")))
	(mapcar '(lambda (h) (insert (car h) ": " (cdr h) ?\n))
		(cdr article-field-list))
	(insert "\n*** EOOH ***\n")
	(goto-char (point-max))
	(insert ?\^_)
	(setq end (point-max))
	(if (not buf)
	    (append-to-file beg end file-name)
	  ;; File has been visited, in buffer BUF.
	  (set-buffer buf)
	  (let ((buffer-read-only nil)
		(msg (and (boundp 'rmail-current-message)
			  rmail-current-message)))
	    ;; If MSG is non-nil, buffer is in RMAIL mode.
	    (if msg
		(progn (widen)
		       (narrow-to-region (point-max) (point-max))))
	    (insert-buffer-substring cur beg end)
	    (if msg
		(progn
		  (goto-char (point-min))
		  (widen)
		  (search-backward "\^_")
		  (narrow-to-region (point) (point-max))
		  (goto-char (1+ (point-min)))
		  (rmail-count-new-messages t)
		  (rmail-show-message msg)))))
	;; and now unfake the Babyl
	(set-buffer cur)
	(goto-char 1)
	(let ((beg (point)))
	  (search-forward "*** EOOH ***\n")
	  (delete-region beg (point)))
	(goto-char (point-max))
	(delete-char -1)))))

(defun gnews-output-to-mbox-file (file-name)
  "Append the current article to a Unix mail file named FILE-NAME."
  (interactive (list (read-file-name
		       (concat "Mbox save: (default "
			       (file-name-nondirectory group-last-save)
			       ") ")
		       (file-name-directory group-last-save)
		       (if (file-directory-p group-last-save)
			   (concat group-last-save article-current)
			 group-last-save))))
  (require 'rmail)
  (if article-grab-point (article-forward-intern group-save-junk))
  (setq file-name (expand-file-name file-name)
	group-last-save file-name)
  (let ((b (current-buffer))
	(mb (get-buffer-create "*gnews*mbox*"))
	(case-fold-search t))
    (save-excursion
      (set-buffer mb)
      (erase-buffer)
      (insert-buffer-substring b)
      (insert "\n")
      (goto-char (point-min))
      (insert "From "
	      (reply-domain (article-field "From")) " "		; MJS
	      (current-time-string) "\n")
      (while (search-forward "\nFrom " nil t)
	(forward-char -5)
	(insert ?>))
      (append-to-file (point-min) (point-max) file-name))
    (kill-buffer mb)))

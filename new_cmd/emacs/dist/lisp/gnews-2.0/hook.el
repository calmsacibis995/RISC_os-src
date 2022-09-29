;;; hook.el: hook-kill utilities for Gnews
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

(defvar hook-kill-all nil
  "*List of hook-kills.  It is a list of the form
  ((REGEXP HOOK-KILL HOOK-KILL ...) (REGEXP HOOK-KILL ...) ...).
Each REGEXP is a regular expression, whose associated HOOK-KILLs are
applied if the name of the current newsgroup matches REGEXP.\n
Each HOOK-KILL is one of three forms:
	(pre IGN FUNCTION ARGS ...)
	(post IGN FUNCTION ARGS ...)
	(FIELD RXP FUNCTION ARGS ...).
The first two are called pre- and post-hooks; (FUNCTION ARGS ...) is
evaluated upon newsgroup entry/exit respectively.  (The second field
is ignored.)  The third is called a per-hook; for each article, if its
FIELD header matches the regular expression RXP, (FUNCTION ARGS ...) is
evaluated.  If FIELD is the empty string, the entire header is searched
for RXP.\n
Useful functions to remember are article-junk, article-junk-local,
gnews-set, gnews-key, gnews-fset, article-digest, which see.  These
can be added by the appropriate hook-kill-mode commands.\n
In a per-hook, if FUNCTION has a non-nil 'hook-kill-junk property
and is applied, then the user is informed of the junking, hook-kill
processing for this article is finished, and Gnews goes on to the next
article.  Failing this, if FUNCTION sets hook-kill-continue to nil,
then no further hook-kill processing will be done, and the article will
be set to.")

;;; hook-kill processing

(defun hook-kill-set (group)
  "Set the hook-kill variables for regexp GROUP."
  (setq index-kill-per)			; used internally by indexing
  (let ((hook (apply 'append (mapcar (function
				       (lambda (g)
					 (if (string-match g group)
					     (cdr (assoc g hook-kill-all)))))
				     (mapcar 'car (cdr hook-kill-all))))))
    (gnews-map (function
		 (lambda (var test)
		   (set var (copy-alist
			      (delq nil
				    (mapcar (function
					      (lambda (h) (if (eval test) h)))
					    hook))))))
      '(hook-kill-pre hook-kill-post hook-kill-per)
      '((eq (car h) 'pre) (eq (car h) 'post) (stringp (car h))))))

(defun hook-kill-do (hook &optional quick)
  "Carry out the indicated HOOK action.  Return the hook-kill-junk
property of the invoked function.\n
If the optional second argument QUICK is t, identify headers by
article-field.  If QUICK is a string, use as the article headers.
Otherwise, identify headers by searching in the nntp-buffer.\n
If the variable hook-kill-art is bound (to an article number), first
get the relevant headers for that article via the NNTP internals, then
search the buffer."
  (let ((h (nth 0 hook))
	(r (nth 1 hook))
	(f (nthcdr 2 hook))
	(b (current-buffer))
	s sm hl hh)
    (if (symbolp h)
	(progn (hook-kill-eval f) nil)
      (and (cond ((and (eq quick t) (boundp 'article-field-list))
		  (cond ((string= "" h)
			 (setq hl (cdr article-field-list)
			       hh (car hl))
			 (while hh
			   (if (string-match r (cdr hh))
			       (setq h (car hh) hh nil)		; h reset
			     (setq hl (cdr hl) hh (car hl))))))
		  (cond ((string< "" h)
			 (prog2
			     (setq s (article-field h))
			     (setq sm (string-match r s))
			   (if sm (setq hook-kill-line (concat h ": " s)))))))
		 (t
		  (save-excursion
		    (set-buffer nntp-buffer)
		    (cond (quick				; a string
			   (setq s (buffer-string))
			   (insert quick))
			  ((boundp 'hook-kill-art)
			   (setq s (buffer-string))
			   (nntp-exec t t "head" hook-kill-art)))
		    (goto-char 1)
		    (prog1 (if (string< "" h)
			       (and (re-search-forward
				      (concat "^" h ":") nil t)
				    (re-search-forward r (gnews-eol) t))
			     (re-search-forward r nil t))
		      (setq hook-kill-line
			    (buffer-substring (gnews-bol) (gnews-eol)))
		      (if (or quick (boundp 'hook-kill-art))
			  (progn
			    (erase-buffer)
			    (insert s)))
		      (set-buffer b)))))	; I have to dooooo this?
	   (progn				; header was found: hook away.
	     (hook-kill-eval f)
	     (get (car f) 'hook-kill-junk))))))

(defun hook-kill-junk-message (art-no hook)
  "Display a message upon junking article ART-NO because of hook HOOK."
  (message "%d: %s" art-no (if article-junk-terse "junked" hook-kill-line)))

(defun hook-kill-eval (hk)
  "Evaluate the hook cddr HK.  An eval, with some forms caught."
  (let ((b (current-buffer)))
    (set-buffer news-buffer)		; local variables may lurk
    (cond ((eq (car hk) 'setq)
	   (set (gnadr hk) (eval (gnaddr hk))))
	  (t				; any others???
	   (eval hk)))
    (set-buffer b)))

(defun hook-kill-add (group hook)
  "For regexp GROUP, add hook-kill HOOK."
  (let ((gh (assoc group hook-kill-all)))
    (cond (gh
	   (nconc gh (list hook)))
	  (t
	   (nconc hook-kill-all (list (list group hook)))))
    (setq gnews-hook-dangle t)))

(defun hook-kill-delete (group hook)
  "For regexp GROUP, delete hook-kill HOOK."
  (let* ((s (assoc group hook-kill-all))
	 (c (prin1-to-string (cdr s)))
	 (h (regexp-quote (prin1-to-string hook))))
    (cond ((string-match h c)
	   (setcdr s (read (concat (substring c 0 (match-beginning 0))
				   (substring c (match-end 0)))))
	   (setq gnews-hook-dangle t)))))

(defun hook-kill-group (group)
  "Get the designated regexp for GROUP: all hook-kills for GROUP use
the return string from this function."
  (or (cdr (assoc group hook-kill-alist)) group))

;;; hook-kill support

(defun gnews-set (u v &optional hooklist no-reset)
  "For use within pre-hooks.\n
Sets U to V.  Like set, both are eval'ed.  If U currently has a
value, an expression to reset U back to this value is appended to
hook-kill-post.  If U is currently unbound, then instead an
expression to reunbind U is appended to hook-kill-post.\n
Optional third argument HOOKLIST, if a non-nil symbol, means use
HOOKLIST instead of hook-kill-post.  Optional fourth argument
NO-RESET means just set, never mind about the hooks."
  (interactive
    (let ((uu (read-variable "In this newsgroup, set variable: ")))
      (list uu (read-from-minibuffer
		 (concat "Set " (symbol-name uu) " to: ") nil nil t))))
  (if no-reset nil
    (setq hooklist (or hooklist 'hook-kill-post))
    (if (not (boundp hooklist)) (set hooklist nil))
    (set hooklist				; FILO
	 (cons (if (boundp u)
		   (list 'post 'nil 'setq u (list 'quote (eval u)))
		 (list 'post 'nil 'makunbound (list 'quote u)))
	       (eval hooklist))))
  (set u v))

(defun gnews-fset (f g &optional hooklist no-reset)
  "For use within pre-hooks.\n
Fsets F to G.  Like fset, both are eval'ed.  If F currently has a
function value, an expression to re-fset F to this value is appended
to hook-kill-post.  If F is currently not fboundp, then instead an
expression to re-un-fbind F is appended to hook-kill-post.\n
Optional third argument HOOKLIST, if a non-nil symbol, means use
HOOKLIST instead of hook-kill-post.  Optional fourth argument
NO-RESET means just fset, never mind about the hooks."
  (interactive
    (let ((ff (completing-read "In this newsgroup, redefine function: "
			       obarray 'fboundp t)))
      (list (read ff)
	    (read (completing-read
		    (format "Redefine %s to: " ff) obarray 'fboundp t)))))
  (if no-reset nil
    (setq hooklist (or hooklist 'hook-kill-post))
    (if (not (boundp hooklist)) (set hooklist nil))
    (set hooklist				; FILO
	 (cons (if (fboundp f)
		   (list 'post 'nil 'fset
			 (list 'quote f) (list 'quote (symbol-function f)))
		 (list 'post 'nil 'fmakunbound (list 'quote f)))
	       (eval hooklist))))
  (fset f g))

(defun gnews-key (keys def &optional hooklist no-reset mode)
  "For use within pre-hooks.\n
Rebind KEYS within {group,article}-mode-map to DEF.  An expression
to rebind the former definition of KEYS is appended to hook-kill-post.\n
Optional third argument HOOKLIST, if a non-nil symbol, means use
HOOKLIST instead of hook-kill-post.  Optional fourth argument
NO-RESET means just rebind the keys, never mind about the hooks.
Optional fifth argument MODE if the atom 'article means just rebind
in article-mode, if 'group then in group-mode."
  (interactive
    (let ((kk (read-key-sequence "In this newsgroup, bind key: ")))
      (list kk (read-command
		 (concat "Bind " (key-description kk) " to: ")))))
  (let ((art-ok (or (not mode) (eq mode 'article)))
	(group-ok (or (not mode) (eq mode 'group))))
    (if no-reset nil
      (setq hooklist (or hooklist 'hook-kill-post))
      (if (not (boundp hooklist)) (set hooklist nil))
      (if art-ok
	  (set hooklist			; FILO
	       (cons (list 'post nil 'define-key 'article-mode-map keys
			   (list 'quote (lookup-key article-mode-map keys)))
		     (eval hooklist))))
      (if group-ok
	  (set hooklist			; FILO
	       (cons (list 'post nil 'define-key 'group-mode-map keys
			   (list 'quote (lookup-key group-mode-map keys)))
		     (eval hooklist)))))
    (if art-ok (define-key article-mode-map keys def))
    (if group-ok (define-key group-mode-map keys def))))

(put 'hook-kill-add 'lisp-indent-hook 1)
(put 'hook-kill-delete 'lisp-indent-hook 1)

;;; Set up Hook-Kill editing

(defun group-hook-edit ()
  "Edit the hooks associated with the current newsgroup."
  (interactive)
  (news-hook-edit (hook-kill-group group-current)))

(defun news-hook-edit (group)
  "Edit the hooks for the prompted regexp GROUP.  Default is \"\", referring
to the global hooks.  Name completion is done on existing hook regexps
and newsgroup names, but there is no abbreviation expansion."
  (interactive
    (list (completing-read "Edit hooks for (regexp): "
			   (append hook-kill-all group-roster))))
  (gnews-buffer hook-kill-pop hook-kill-buffer)
  (erase-buffer)
  (mapcar 'hook-kill-pretty-print (cdr (assoc group hook-kill-all)))
  (goto-char 1)
  (if (not (eobp)) (delete-char 1))
  (hook-kill-mode)
  (setq gnews-mode-string group)
  (gnews-set-mode-line)
  (message "Use %s to implement changes, %s to abort"
	   (if (eq (key-binding "\C-c\C-c") 'hook-kill-exit) "C-c C-c"
	     (substitute-command-keys "\\[hook-kill-exit]"))
	   (if (eq (key-binding "\C-c\C-]") 'hook-kill-abort) "C-c C-]"
	     (substitute-command-keys "\\[hook-kill-abort]"))))

(defconst hook-kill-ppchars
  '((?\\.?\\ )(?\n.?n)(?\t.?t)(?\b.?b)(?\e.?e)(?\r.?r)(?\f.?f)(?\".?\")))

(defun hook-kill-pretty-print (a &optional nonewline nonewlinerecurse)
  "Pretty print a typical hook-kill item."
  ;; I have to dooo this?
  (cond ((null a)
	 (insert "nil"))
	((stringp a)
	 (let ((p (gnews-bol)) (q (point)))
	   (if (< (+ p (window-width))
		  (apply '+ q 2
			 (mapcar '(lambda (c)
				    (cond ((assoc c hook-kill-ppchars) 2)
					  ((= c 0) 4)
					  ((= c 127) 4)
					  ((< c 27) 4)
					  (t 1)))
				 a)))
	       (newline)))
	 (insert ?\" )
	 (mapcar '(lambda (c)
		    (let ((d (cdr (assoc c hook-kill-ppchars))))
		      (cond (d (insert ?\\ d))
			    ((= c 0) (insert "\C-@"))
			    ((= c 127) (insert "\C-?"))
			    ((< c 27) (insert "\\C-" (+ ?` c)))
			    (t (insert c)))))
		 a)
	 (insert ?\"))
	((numberp a)
	 (insert (concat a)))
	((atom a)
	 (insert (symbol-name a)))
	((listp a)
	 (cond ((eq (car a) 'quote)
		(if (and (listp (gnadr a))
			 (eq (car (gnadr a)) 'lambda))
		    (insert ?\n))
		(insert ?')
		(hook-kill-pretty-print
		  (read (substring (prin1-to-string a) 7 -1)) t))
	       ((eq (car a) 'if)
		(insert "\n(if ")
		(hook-kill-pretty-print (gnadr a) t t)
		(insert " ")
		(mapcar '(lambda (i)
			   (hook-kill-pretty-print i) (insert ? ))
			(gnddr a))
		(delete-char -1)
		(insert ")"))
	       ((eq (nth 3 a) 'index-perm-marks)
		(insert ?\n (prin1-to-string a)))
	       (t
		(lisp-indent-line)
		(end-of-line)
		(or nonewline (listp (car a)) (insert ?\n))
		(insert ?\()
		(lisp-indent-line)
		(hook-kill-pretty-print (car a) t)
		(insert ? )
		(mapcar '(lambda (i)
			   (hook-kill-pretty-print i nonewlinerecurse)
			   (insert ? ))
			(cdr a))
		(delete-char -1) 
		(insert ?\))
		(lisp-indent-line))))
	(t
	 (insert (prin1-to-string a)))))

; Something is rotten in undo here:
;(hook-kill-pretty-print (symbol-function 'hook-kill-pretty-print))


;;; Hook Kill mode

(if hook-kill-mode-map nil
  (setq hook-kill-mode-map (gnews-copy-keymap emacs-lisp-mode-map))
  (gnews-key-bind hook-kill-mode-map
		  '(("\^c\^c".hook-kill-exit)
		    ("\^c\^]".hook-kill-abort)
		    ("\^c\^d".hook-kill-insert-digest)
		    ("\^c\^f".hook-kill-insert-fset)
		    ("\^c\^k".hook-kill-insert-key)
		    ("\^c\^s".hook-kill-insert-set)
		    ("\^c\^i".hook-kill-insert-index)
		    ("\^c\^j".hook-kill-insert-junk)
		    ("\^c\^l".hook-kill-insert-junk-local)
		    ("\^c\^y".hook-kill-insert-yes)
		    ("\^c\^o".hook-kill-insert-pre)
		    ("\^c\^p".hook-kill-insert-per)
		    ("\^c\^q".hook-kill-insert-post)
		    ("\^c?".describe-mode)
		    ("\^c\^h".gnews-describe-mode))))

(defun hook-kill-mode ()
  "Mode to edit hook-kills with.  It is Emacs-Lisp mode with a few
extra commands, to either exit, or to insert stereotyped hook-kill
templates.\n
The commands are:
\\{hook-kill-mode-map}
The buffer contents should be a sequence of sexps of the form
\(per/post/\"header\" ...\), as described in the documentation for
the variable hook-kill-all."
  (interactive)
  (emacs-lisp-mode)
  (use-local-map hook-kill-mode-map)
  (make-local-variable 'gnews-mode-string)
  (make-local-variable 'gnews-minor-mode-alist)
  (setq major-mode 'hook-kill-mode
	mode-name "Hook Kill"
	gnews-minor-mode-alist nil
	gnews-read-p nil
	gnews-hook-p t)
  (run-hooks 'emacs-lisp-mode-hook 'hook-kill-hook))

(defun hook-kill-abort ()
  "Return to news reading, ignoring any changes."
  (interactive)
  (bury-buffer)
  (delete-windows-on (current-buffer))
  (gnews))

(defun hook-kill-exit ()
  "Return to news reading, installing any changes."
  (interactive)
  (condition-case nil
      (let ((hk (read (concat "(" (buffer-string) ")")))
	    (g (assoc gnews-mode-string hook-kill-all)))
	(if g
	    (setcdr g hk)
	  (nconc hook-kill-all (list (append (list gnews-mode-string) hk))))
	(setq hook-kill-per (append hook-kill-per hk)
	      gnews-hook-dangle t)
	(hook-kill-abort))
    (error (error "Mismatched parentheses/Missing quotation mark ?"))))

(defun hook-kill-insert-set (var)
  "Insert a pre-hook for gnews-set, which see."
  (interactive "*vHook set variable: ")
  (beginning-of-line)
  (insert "(pre nil gnews-set '" (symbol-name var) " )\n")
  (forward-char -2))

(defun hook-kill-insert-fset (func)
  "Insert a pre-hook for gnews-fset, which see."
  (interactive "*aHook fset function: ")
  (beginning-of-line)
  (insert "(pre nil gnews-fset '" (symbol-name func) " ')\n")
  (forward-char -2))

(defun hook-kill-insert-key (key comm)
  "Insert a pre-hook for gnews-key, which see."
  (interactive "*kHook bind key: \nCHook bind to command: ")
  (beginning-of-line)
  (insert "(pre nil gnews-key \"" key "\" '" (symbol-name comm) ")\n"))

(defun hook-kill-insert-index (count)
  "Insert a pre-hook for index-if, which see.\n
Prefix argument gives a COUNT, the minimum number of unread articles
that must be proffered to trigger the indexing."
  (interactive "*p")
  (beginning-of-line)
  (insert "(pre nil index-if " (concat count) ")\n")
  (forward-char -2))

(defun hook-kill-insert-junk (header)
  "Insert an article-junk per-hook for the current article.  This
command prompts for the header."
  (interactive
    (list (let ((afl (cons (cons "" nil) article-field-list)))
	    (completing-read "Header: " afl nil t))))
  (beginning-of-line)
  (if (string< "" header)
      (insert "(\"" header "\" \""
	      (cdr (assoc header article-field-list))
	      "\" article-junk)\n")
    (insert "(\"\" \""
	    (gnews-replace "\\\\" "\\\\\\\\" (read-string "Regexp: "))
	    "\" article-junk)\n")))

(defun hook-kill-insert-junk-local (header)
  "Insert an article-junk-local per-hook for the current article.  This
command prompts for the header."
  (interactive
    (list (let ((afl (cons (cons "" nil) article-field-list)))
	    (completing-read "Header: " afl nil t))))
  (beginning-of-line)
  (if (string< "" header)
      (insert "(\"" header "\" \""
	      (cdr (assoc header article-field-list))
	      "\" article-junk-local)\n")
    (insert "(\"\" \""
	    (gnews-replace "\\\\" "\\\\\\\\" (read-string "Regexp: "))
	    "\" article-junk-local)\n")))

(defun hook-kill-insert-yes (header)
  "Insert an article-yes per-hook for the current article.  This
command prompts for the header."
  (interactive
    (list (let ((afl (cons (cons "" nil) article-field-list)))
	    (completing-read "Header: " afl nil t))))
  (beginning-of-line)
  (if (string< "" header)
      (insert "(\"" header "\" \""
	      (cdr (assoc header article-field-list))
	      "\" article-yes)\n")
    (insert "(\"\" \""
	    (gnews-replace "\\\\" "\\\\\\\\" (read-string "Regexp: "))
	    "\" article-yes)\n")))

(defun hook-kill-insert-pre ()
  (interactive)
  (beginning-of-line)
  (insert "(pre nil )\n")
  (forward-char -2))

(defun hook-kill-insert-per ()
  (interactive)
  (beginning-of-line)
  (let* ((afl (cons (cons "" nil) article-field-list))
	 (h (reply-re-0
	      (completing-read "Header: " afl nil t)))
	 (f (cdr (assoc h afl))))
    (setq f (or f (gnews-replace "\\\\" "\\\\\\\\" (read-string "Regexp: "))))
    (insert "(\"" h "\" \"" f "\" )\n"))
  (forward-char -2))

(defun hook-kill-insert-post ()
  (interactive)
  (beginning-of-line)
  (insert "(post nil )\n")
  (forward-char -2))

(defun hook-kill-insert-digest (&optional pfx)
  "Insert or modifiy a pre-hook for the Digest minor mode.  If the current
line is not a digest pre-hook, a pre-hook will be created.  If looking
at a digest pre-hook for conditional digests, add the current article's
\"From:\" field to the argument list.\n
With a prefix argument, insert a new pre-hook, or modify the current
line's pre-hook, for rot13-ed digest settings."
  (interactive "*P")
  (beginning-of-line)
  (let ((from (gnews-string-as-buffer
		(reply-domain (article-field-raw "From")) 'b
		(if (re-search-forward "[ \t]" nil t)
		    (forward-char -1)))))
    (cond ((looking-at "(pre nil article-digest[^-]")	 ; rot13 it
	   (down-list 1)
	   (forward-sexp 3)
	   (if (looking-at "[ \t]*)")
	       (insert " nil t")
	     (forward-sexp 1)
	     (insert " t")))
	  ((looking-at "(pre nil article-digest-if ")
	   (cond (pfx
		  (if (search-forward (concat from "\"") nil t)
		      (if (looking-at "[ \t]*)")	 ; missing MAIL, ROT13
			  (insert " nil t")
			(forward-sexp 1)
			(if (looking-at "[ \t]*)")	 ; missing ROT13
			    (insert " t")
			  (delete-region (point)
					 (progn (forward-sexp 1) (point)))
			  (insert " t")))
		    (beginning-of-line)			 ; missing DIGEST
		    (forward-sexp 1)
		    (search-backward ")")
		    (insert " \"" from "\" t t")))
		 (t
		  (if (search-forward from nil t) nil	 ; already there
		    (down-list 1)
		    (forward-sexp 3)
		    (insert " \"" from "\" t nil")))))
	  (t
	   (if (gnews-mod-p group-current)
	       (insert "(pre nil article-digest"	 ; standard digest
		     (if pfx " nil nil t" "") ")")
	     (insert "(pre nil article-digest-if \"" from "\" t"
		       (if pfx " t" "") ")"))))))	 ; conditional digest

;;; digest.el: digest support for Gnews
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

;;; Put (pre nil article-digest) in your hooks for comp.risks, etc.

(defvar article-formfeed-post-digest
  "\n\n+\\(.*\n\\)?\\(Date:\\|From:\\|Subject:\\|End of .* Digest\\)"
  "Default regexp that must match after a putative digest separating
string to confirm that the separation is genuine.  For rot13-ed, the
default is based on article-formfeed-post-digest-rot13, which see.")

(defvar article-formfeed-post-digest-rot13
  ;; The {rot13,plain} versions are for scrolling {forward,backward}.
  (concat "\n\n+\\(.*\n\\)?\\(Date:\\|From:\\|Subject:\\|End of .* Digest"
	  "\\|Qngr:\\|Sebz:\\|Fhowrpg:\\|Raq bs .* Qvtrfg\\)")
  "Default regexp--for rot13-ed digests only--that must match after a
putative digest separating string to confirm that the separation is
genuine.")

(defvar article-formfeed-digest "^----------------------*$")

(defvar n-reply-digest-mail nil
  "If nil, post followups to a digest using standard posting code.  If
non-nil, post followups to a digest using e-mail.  The variable is used
as the recipient's address if it is a string, otherwise, the address
will be taken from the digest's own headers.")

(defun gnews-digest-p ()
  "Return t if the current article is a digest, nil otherwise."
  (let ((b (current-buffer)))
    (prog2
	(set-buffer news-buffer)
	gnews-digest-p
      (set-buffer b))))

(defvar article-formfeed-p nil
  "Non-nil means Gnews is in a digest, and a formfeed has been passed.")

(defun article-digest (&optional hooklist repeatable rot13)
  "Use digest appropriate bindings in this newsgroup.\n
Note that when talking about digests, \"article\" refers to the individual
submissions, while \"digest\" refers to the entire item that is bundled
together as a single Usenet article.\n
In programming code, an optional argument HOOKLIST, a symbol, means
compile post-hook resets into the named symbol if it is undefined or
nil, or if REPEATABLE is non-nil.  If HOOKLIST is nil, the compilation
goes to hook-kill-post.\n
Argument ROT13 indicates a rot13-ed digest: the initial part is assumed
to be non-rot13, but the rest of the digest is rot13ed.  This behavior
can be modified by setting article-formfeed-p, which see.\n
Interactively, both HOOKLIST and REPEATABLE are nil.  ROT13 is the
prefix argument.\n
Within pre-hooks, (pre nil article-digest) should be correct for
moderated newsgroups.  For unmoderated groups, use article-digest-if
in pre-hooks, which see.  Both of these can be inserted in Hook Kill
mode by the hook-kill-insert-digest command, which see."
  (interactive (list nil nil current-prefix-arg))
  (if (not (boundp hooklist)) (set hooklist nil))
  (let ((r (and (eval hooklist) (not repeatable)))
	(b (current-buffer)))
    (set-buffer news-buffer)
    (setq article-grab-point-old nil
	  article-digest-rot13-p rot13
	  article-formfeed-p (not rot13))
    (gnews-set 'article-formfeed article-formfeed-digest hooklist r)
    (gnews-set 'article-formfeed-post
	       (if rot13
		   article-formfeed-post-digest-rot13
		 article-formfeed-post-digest)
	       hooklist r)
    (gnews-set 'article-formfeed-top 0 hooklist r)
    (gnews-set 'gnews-digest-p t hooklist r)
    (gnews-key "<" 'article-digest-restart hooklist r 'article)
    (gnews-key "n" 'article-digest-next hooklist r)
    (gnews-key "p" 'article-digest-previous hooklist r)
    (gnews-key "s" 'article-digest-save hooklist r 'article)
    (gnews-fset 'reply-yank 'article-digest-yank hooklist r)
    (run-hooks 'article-digest-hook)
    (set-buffer b)))

(defun article-digest-if (&rest args)
  "Set up Digest minor mode, conditionally activated by the appropriate
\"From:\" fields.\n
This function interprets its arguments in groups of three, one triple
per possible digest to consider.  If the current article does not match
one of the triples, then the Digest minor mode internals will be cleared.\n
The first argument within a triple is a string, meaning activate Digest
mode if the current article's \"From:\" field matches the string.\n
The second argument within a triple describes how to submit articles to
that digest.  It is given in n-reply-digest-mail format, which see.\n
The third argument within a triple is a rot13 flag.  This is usually
nil.  If non-nil, the digest is assumed to consist of an unrot13ed header
and rot13ed articles.  The latter will be shown decrypted.\n
Interactively, conditional activation hooks for the current \"From:\"
field will be set up (with a prefix argument, the digest will be assumed
to be rot13ed).  Then the current Usenet article will be restarted."
  (interactive (list (gnews-string-as-buffer
		       (reply-domain (article-field-raw "From")) 'b
		       (if (re-search-forward "[ \t]" nil t)
			   (forward-char -1)))
		     t (if current-prefix-arg t)))
  (let ((adm-args (list (list 'lambda nil
			      (append '(article-digest-maybe) args))))
	(adm (list 'article-digest-maybe)))
    (cond ((fboundp 'article-header-hook)
	   (gnews-set 'article-header-hook
		      (append (list article-header-hook) adm-args)))
	  ((listp 'article-header-hook)
	   (gnews-set 'article-header-hook
		      (append article-header-hook adm-args)))
	  (t
	   (gnews-set 'article-header-hook adm-args)))
    (cond ((fboundp 'group-last-hook)
	   (gnews-set 'group-last-hook
		      (append (list group-last-hook) adm)))
	  ((listp 'group-last-hook)
	   (gnews-set 'group-last-hook (append group-last-hook adm)))
	  (t
	   (gnews-set 'group-last-hook adm)))
    (setq hook-kill-post (cons (list 'post nil 'article-digest-maybe)
			       hook-kill-post)))
  (if (interactive-p) (article-restart-reset)))

(defun article-digest-maybe (&rest args)
  "Internal function for conditionally activating the Digest minor mode.
With no arguments, turn off the minor mode.  With arguments, they are
interpreted as in article-digest-if, which see."
  (let ((from (article-field-raw "From"))
	addr mail rot13 match)
    (while (and args (not match))
      (setq addr (car args) mail (gnadr args) rot13 (gnaddr args))
      (if (string-match addr from)
	  (setq match t n-reply-digest-mail mail)
	(setq args (cdr (gnddr args)))))
    (if match
	(article-digest 'article-digest-maybe nil rot13)
      (mapcar 'hook-kill-do article-digest-maybe)
      (setq n-reply-digest-mail nil article-digest-maybe nil))))

;;; digest-mode commands

(defun article-digest-next ()
  "Move forward within a digest to the next digest article.\n
If there are no more digest articles, move to the next Usenet
article within the newsgroup."
  (interactive)
  (cond (article-grab-point
	 (goto-char article-grab-point)
	 (beginning-of-line)
	 (if (looking-at article-formfeed)
	     (recenter article-formfeed-top))
	 (article-forward-intern t article-formfeed article-formfeed-post))
	(t
	 (group-next-unread))))

(defun article-digest-previous ()
  "Move to the previous article within a digest."
  (interactive)
  (let ((i 0) rsb)
    (setq article-grab-point (or article-grab-point (point-max)))
    (goto-char article-grab-point)
    (while (< i 1)
      (while (and (not (beginning-of-line))
		  (setq rsb (re-search-backward article-formfeed 1 t))
		  (not (end-of-line))
		  (not (looking-at article-formfeed-post))))
      (end-of-line)
      (setq buffer-read-only nil)
      (delete-region (point) article-grab-point)
      (setq buffer-read-only t article-grab-point (point) i (1+ i)))
    (if (not rsb)
	(message "Beginning of digest" (goto-char 1) (ding))
      (while (and (not (beginning-of-line))
		  (setq rsb (re-search-backward article-formfeed 1 t))
		  (not (end-of-line))
		  (not (looking-at article-formfeed-post))))
      (setq article-grab-point-old (if rsb (point)))
      (beginning-of-line)
      (if rsb (recenter article-formfeed-top) (goto-char 1)))
    (article-%-compute)
    (gnews-set-mode-line)))

(defun article-digest-restart ()
  "Move to the beginning of the current article within a digest."
  (interactive)
  (let (rsb)
    (goto-char article-grab-point)
    (while (and (not (beginning-of-line))
		(setq rsb (re-search-backward article-formfeed 1 t))
		(not (end-of-line))
		(not (looking-at article-formfeed-post))))
    (beginning-of-line)
    (if rsb (recenter article-formfeed-top) (goto-char 1))
    (article-%-compute)
    (gnews-set-mode-line)))

;;; reply support for digests

;;; Based on code originally written by ram-ashwin@yale.arpa (Ashwin Ram)

(defun article-digest-yank (arg)
  "Yank in text of current article. (which need not necessarily be the one
being responded to).  The identifying blurb is formed by one of the members
of the list reply-blurb.  If the prefix argument is 0, no blurb is given.
If the prefix argument is <nn> \\[universal-argument]'s, then the function
\(nth <nn> reply-blurb\) is run."
  (interactive "p")
  (if (and article-grab-point article-grab-point-old)
      (let ((b (current-buffer)) s p)
	(delete-windows-on mail-reply-buffer)
	(set-buffer mail-reply-buffer)
	(goto-char article-grab-point-old)
	(skip-chars-forward " \t\n")
	(forward-paragraph 1)
	(forward-line 1)
	(setq p (point))
	(goto-char article-grab-point)
	(beginning-of-line)
	(skip-chars-backward " \t\n")
	(setq s (buffer-substring p (point)))
	(set-buffer b)
	(setq p (point))
	(insert s)
	(setq reply-yank-min p reply-yank-max (point))
	(reply-indent reply-yank-min reply-yank-max)
	(if (< 0 arg)
	    (funcall (nth (min (1- (length reply-blurb))
			       (gnews-arg-count arg))
			  reply-blurb)))
	(goto-char reply-yank-min)))
  (open-line 2)
  (delete-blank-lines)
  (forward-char 1))

(defun article-digest-field-raw (header)
  (let ((dig-head (article-field-raw header)) p art-head)
    (if article-grab-point-old
	(save-excursion
	  (set-buffer news-buffer)
	  (goto-char article-grab-point-old)
	  (skip-chars-forward " \t\n")
	  (setq p (point))
	  (forward-paragraph 1)
	  (setq art-head (and (re-search-backward
				(concat "^\\("
					(regexp-quote (or header ""))
					"\\):[ \t]*\\(.*\\)$")
				p t)
			      (gnews-match 2)))
	  (article-digest-header-tweak header dig-head art-head))
      dig-head)))

(defun article-digest-header-tweak (header digest article)
  "Return the appropriate HEADER field for an article within a digest,
based, perhaps, on the DIGEST's value of the field and the ARTICLE's
value of the field."
  (cond ((string= header "Message-ID")
	 (concat "<" (article-field-raw "Subject") ">"))
	((string= header "Subject")
	 (reply-re-0 (or article digest)))
	((string= header "From")
	 (require 'mail-utils)
	 (mail-strip-quoted-names (or article "")))
	(t (or article digest))))

;;; saving articles

(defun article-digest-save ()
  "Append current digest article to a file.  The variable gnews-save-style
controls the default output style, which see."
  (interactive)
  (if gnews-slashify (gnews-mkdir (file-name-directory group-last-save)))
  (save-excursion
    (save-restriction
      (narrow-to-region
	article-grab-point-old (or article-grab-point (point-max)))
      (if (commandp gnews-save-style)	; must expect, reset group-last-save
	  (call-interactively gnews-save-style)
	(let ((save (if gnews-save-style
			(file-name-nondirectory group-last-save)
		      (gnews-save-name group-current
				       (concat article-current))))
	      (dir (file-name-directory group-last-save)))
	  (setq gnews-last-save (expand-file-name
				  (read-file-name
				    (format "Save to file: (default %s) " save)
				    (file-name-directory group-last-save)
				    (file-name-nondirectory save)) ;EHL
				  dir)))
	(write-region (point-min) (point-max) gnews-last-save t))))
  (recenter 0)
  (gnews-flush))

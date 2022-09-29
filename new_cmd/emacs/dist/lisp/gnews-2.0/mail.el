;;; mail.el: mail-mode compatible Gnews reply commands
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

(provide 'gnews-mail)

;;; basic reply variables

(defvar reply-prefix ">"		; duplicated from Init.el
  "*String to indent replies with")	; for sake of mail-modes
(defvar reply-prefix-forward ">"
  "*String for indenting forwarded replies with")
(defvar reply-digest-separator "----------------"
  "*Digest article separating string")
(defvar reply-prefix-match "^a-zA-Z0-9([$`'\""
  "*Characters that define prefixes for \\[reply-split-line]")
(defvar reply-prefix-initials "[a-zA-Z]?[a-zA-Z]?[a-zA-Z]?[a-zA-Z]?>")
(defvar reply-prefix-fill "[^a-zA-Z0-9([$`'\"]*[ \t]*"
  "*Regexp that defines prefixes for \\[reply-yank-fill]")

(defvar reply-max-back-name 60
  "*Distance from end to look for user's name when inhibiting signatures.")
(defvar reply-max-back-ps 500
  "*Distance from end to look for a postscript when inhibiting signatures.")
(defvar reply-ps-match
  (concat "^\\(" (regexp-quote reply-prefix) "\\)?\\(p[. ]*\\)+s[^a-z]")
  "*Regexp that matches postscript beginnings")

(defvar reply-signature-inhibitions
  '(reply-inhibit-name reply-inhibit-login reply-inhibit-ps)
  "*List of functions that must all come out nil before adding a
signature via \\[reply-signature].\n
The default inhibitions provided with Gnews return t when your first
name occurs within the last reply-max-back-name characters of the end,
or either your banged login name or a postscript occurs within the last
max-back-ps characters of the end.\n
Signature inhibition can be overridden by invoking \\[reply-signature] \
directly.\n
Postscripts are defined as whatever matches the regexp reply-ps-match.\n")

(defvar reply-valediction "-"
  "*String to prepend very brief signatures with--no newline is inserted")
(defvar reply-signature-separator "-- "
  "*String to prepend long signatures with--a newline is inserted")

(defvar reply-signature-default-major (concat gnews-dot-dir ".signature")
  "*The primary default signature file.")

(defvar reply-signature-default-minor '(t . "")
  "*A dotted pair describing the secondary default signature.  See
the two variables {e,n}-reply-signature-prefix-pointers for the
conditions in which the secondary default signature is appended.\n
Its car, if nil, means to use a file (looked for in the standard
places ($DOTDIR,$HOME)) that contains the signature.  If its car is
non-nil, it means to expect a literal string to use as signature.\n
The cdr is either a string (either the name of a file, in case the
car were nil, or the very signature itself, if the car were non-nil)
or a function, whose return value is the appropriate string.\n
The provided default is '(t.\"\"), for usage with inews, which
appends signatures automatically.  For raw news posters that do not
appends signatures, the default should be '(nil.\".signature\").")

(defvar e-reply-signature-prefix-pointers (list 0 nil nil 1)
  "*A list of the values of arguments to \\[reply-send] which mean, in E-Reply
mode, to append no signature, to append the user's first name only, to
append the user's full name, and to append the primary default signature
file respectively.  If the argument to \\[reply-send] does not match any of
the members of this list, then append the secondary default signature.\n
The primary default is the value of reply-signature-default-major; the
secondary default is coded in the value of reply-signature-default-minor,
In the case of user's first/full name, the string reply-valediction is
prepended.\n
The default is set up so that an explicit argument 0 suppresses the
signature, no argument appends the default signature file, and any
other argument appends the secondary default signature.")

(defvar n-reply-signature-prefix-pointers (list 0 nil nil nil)
  "*A list of the values of arguments to \\[reply-send] which mean, in N-Reply
mode, to append no signature, to append the user's first name only, to
append the user's full name, and to append the primary default signature
file respectively.  If the argument to \\[reply-send] does not match any of
the members of this list, then append the secondary default signature.\n
The primary default is the value of reply-signature-default-major; the
secondary default is coded in the value of reply-signature-default-minor,
In the case of user's first/full name, the string reply-valediction is
prepended.\n
The default is set up so that an explicit argument 0 suppresses the
signature, and anything else invokes the secondary default mechanism.
(Which also defaults to no signature.  In other words, things are set
up so that inews appends your signature for you.  If you use a different
posting mechanism, for example, if n-reply-program is nil, then you
should change your defaults to be more like the E-Reply defaults.)")

(defvar reply-signature-point (set-marker (make-marker) 1)
  "Marker at the beginning of the signature just after its insertion.")

;;; path aliasing--not for everyone

(provide 'gnews-mail)

(defconst path-buffer-name "*gnews*path*" "Internal path-finding buffer")

(defun path-alias (site)
  "Turns \"SITE\" into \"path!SITE\"."
  ;; This assumes that "telnet PATH-HOST PATH-SERVICE" is around.  See
  ;; RFC 915.  If you have a different way of pathaliasing, then rewrite.
  (let* ((b (current-buffer))
	 (path-buffer (generate-new-buffer path-buffer-name))
	 (path-finder (open-network-stream
		       "path-finder" path-buffer path-host path-service))
	 p)
    (accept-process-output path-finder)
    (send-string path-finder (concat "path " site "\n"))
    (accept-process-output path-finder)
    (delete-process path-finder)
    (set-buffer path-buffer)
    (goto-char (point-max))
    (previous-line 1)
    (prog2
	(setq p (point))
	(if (string= "220" (buffer-substring p (+ 3 p)))
	    (buffer-substring (+ 4 p) (- (point-max) 5)))
      (set-buffer b)
      (kill-buffer path-buffer))))

(defun reply-path-alias (pfx)
  "Look for the standard UUCP map path to the site name bounding point,
and if found, insert it."
  (interactive "*P")
  (if pfx
      (progn
	(end-of-line)
	(search-backward "!")
	(insert ".ATT.COM"))
    (backward-char)
    (if (looking-at "[! ]")
	(forward-char)
      (forward-char)
      (backward-word 1))
    (let ((path (path-alias (buffer-substring
			      (point) (save-excursion
					(forward-word 1) (point))))))
      (if (not path)
	  (message "unknown site" (ding))
	(gnews-delete-word)
	(insert path)))))

(defvar path-uucp nil
  "*A list of your UUCP neighbors, as symbols.")
(defvar path-arpa nil
  "*An alist of site names (symbols) and their domainized forms (strings).")
(defvar path-hops nil
  "*An alist of site names (symbols) to UUCP paths (strings).")
(defvar path-all nil
  "The list of known sites to internal path aliasing, as symbols.")

(defvar path-data-file "your.path.file"
  "*File containing some simple path aliasing rules.\n
Its contents must look something like the following:\n
  (setq path-uucp '(ucdavis ucsbcsl usenix trwrb tolerant alice ...))
  (setq path-arpa '((uunet . \"uunet.uu.net\") (ames . \"arc.nasa.gov\") ...)
  (setq path-hops '((killer . \"decwrl\") (snark . \"uunet!cbmvax\") ...))\n
That is, path-uucp is a list of your UUCP neighbors, path-arpa is an alist
of domainized conversions (ARPA or not), and path-hops is an alist of UUCP
paths to get to the indicated sites.")

(defun reply-to-simplify (&optional pfx recurse)
  "To/Cc/Bcc simplifier."
  (interactive "*P")
  (beginning-of-line)
  (if (looking-at "^\\(to\\|b?cc\\): ") nil
    (e-reply-position-on-field "to")
    (beginning-of-line)
    (re-search-forward "^\\(to\\|b?cc\\): "))
  (if (looking-at "^\\(to\\|b?cc\\): \\([^@\n]+\\)@\\([^@\n]+\\)\\.uucp$")
      (replace-match "\\1: \\2%\\3.uucp@uunet.UU.NET")
    (if path-all nil
      (load path-data-file nil t)
      (setq path-all (append path-uucp
			     (mapcar 'car path-arpa)
			     (mapcar 'car path-hops))))
    (let ((bolpoint (match-end 1)) eolpoint
	  site arpa p name lead site-name-length @ok)
      (goto-char bolpoint)
      (just-one-space)
      (insert ?!)
      (setq bolpoint (1- (point)))
      (end-of-line)
      (setq eolpoint (point) @ok)
      (if (search-backward "%" bolpoint t)
	  (progn
	    (replace-match "@")
	    (if (re-search-forward "\\..*@" eolpoint t)
		nil
	      (setq @ok t)
	      (if (search-forward "@" eolpoint t) (replace-match ".")))))
      (if (and (not @ok) (search-backward "@" bolpoint t))
	  (progn (gnews-delete-line) (newline)))
      (search-backward "!" bolpoint t)
      (if (not pfx)
	  (while (and (not site)
		  (not arpa)
		  (setq p (point))
		  (search-backward "!" bolpoint t))
	(setq name (buffer-substring (1+ (point)) p)
	      site (car (memq (car (read-from-string name)) path-all))
	      arpa (if (save-excursion (search-forward "." p t))
		       name (cdr (assq site path-arpa)))
	      lead (or (cdr (assq site path-hops))))
	(if (and recurse lead)
	    (setq site nil lead nil))
	(cond ((or site arpa)
	       (setq site-name-length (- p (point)))
	       (forward-char 1)
	       (cond (arpa
		      (delete-backward-char (- (point) bolpoint))
		      (delete-backward-char (- site-name-length))
		      (end-of-line)
		      (insert ?@ arpa))
		     (lead
		      (delete-backward-char (- (point) bolpoint))
		      (insert lead ?!)
		      (reply-to-simplify nil t))
		     (site
		      (delete-backward-char (- (point) bolpoint)))))))
	(insert ".att.com")
	(reply-to-simplify nil t))))
  (if (looking-at "!") (delete-char 1 t)))

;; blurbs

(defvar reply-blurb '(reply-short-blurb reply-long-blurb)
  "*List of blurb functions")

(defun reply-short-blurb ()
  "Insert a brief identifying blurb for quoted material."
  (goto-char reply-yank-min)
  (open-line 1)
  (insert ?\n "In " (if (gnews-digest-p) "" "article ")
	  (article-field "Message-ID"))
  (if (or (eq major-mode 'n-reply-mode)
	  (eq this-command 'group-follow-yank))
      (insert ", " (reply-domain (article-field "From")) " writes:")
    (insert " you write:")
    (forward-char 1)))

(defun reply-long-blurb ()
  "Insert a long identifying blurb for quoted material."
  (goto-char reply-yank-min)
  (open-line 1)
  (insert "From: " (article-field "From") ?\n
	  "Subject: " (article-field "Subject") ?\n)
  (if (gnews-digest-p) nil
    (insert "Message-ID: " (article-field "Message-ID") ?\n))
  (forward-char 1))

;;; headers

(defun reply-position-on-field (field pos)
  "Move to end of header labeled FIELD if POS is non-nil, or to the
beginning of the field if POS is nil."
  (if (<= (article-min) (point))	; save old position
      (set-marker reply-body-marker (point)))
  (goto-char (marker-position reply-begin-marker))
  (if (re-search-forward (concat "^" (regexp-quote field) ":") (article-min) t)
      (if pos
	  (end-of-line)
	(beginning-of-line)
	(search-forward ":")
	(forward-char 1))
    (forward-paragraph 1)
    (insert field ": \n")
    (backward-char 1)))

(defun e-reply-position-on-field (field &optional pos)
  "Like mail-position-on-field, but with the mail-header-separator
set to the empty string, as in Rnmail."
  (let ((mail-header-separator "")) (mail-position-on-field field pos)))

;;; signature handling

(defun reply-inhibit-name ()
  "Return non-nil iff user's first name is within reply-max-back-name chars."
  (search-backward (progn
		     (string-match "\\<[^ ]*\\>" (user-full-name))
		     (substring (user-full-name)
				(match-beginning 0) (match-end 0)))
		   (max (- (point-max) reply-max-back-name) max-back-search)
		   t))

(defun reply-inhibit-login ()
  "Return non-nil iff user's login name is within reply-max-back-name chars."
  (search-backward (concat "!" (user-login-name))
		   (max (- (point-max) reply-max-back-ps) max-back-search)
		   t))

(defun reply-inhibit-ps ()
  "Return non-nil iff a postscript is within reply-max-back-ps chars."
  (re-search-backward reply-ps-match
		      (max (- (point-max) reply-max-back-ps) max-back-search)
		      t))

(defun reply-signature (mustsign &optional sigpair)
  "Append a signature.\n
If MUSTSIGN is non-nil, or this function is run interactively, then do
not look through the reply-signature-inhibitions, which see.  Optional
argument SIGPAIR is a dotted pair in reply-signature-default-minor format."
  (interactive (list t))
  (setq reply-position (point))
  (goto-char (point-max))
  (if (/= ?\n (or (char-after (1- (point-max))) 0)) (insert ?\n))
  (let ((max-back-search (save-excursion
			   (goto-char 1)
			   (if (or (string-match "*gnews*" (buffer-name))
				   (zerop (length mail-header-separator)))
			       (forward-paragraph)
			     (search-forward mail-header-separator))
			   (forward-line 1)
			   (point)))
	(ihl reply-signature-inhibitions)
	(inh t))
    (if (or mustsign
	    (progn
	      (while (and ihl inh)
		(setq inh (not (condition-case nil
				   (funcall (car ihl))
				 (error nil)))
		      ihl (cdr ihl)))
	      inh))
	(reply-signature-insert sigpair))))

(defun reply-signature-insert (&optional sigpair)
  "Insert a signature at point, as coded by SIGPAIR.  If SIGPAIR is nil,
use the pair reply-signature-default-minor."
  (interactive)
  (setq sigpair (or sigpair reply-signature-default-minor))
  (let ((p (point))
	(s (cdr sigpair)))
    (if (car sigpair)
	(insert				; get from a string
	  (cond ((stringp s) s)
		((fboundp s) (setq s (funcall s)))))
      (condition-case ()
	  (insert-file			; get from a file
	    (expand-file-name
	      (cond ((stringp s) (concat (or (getenv "DOTDIR")
					     (getenv "HOME") "~")
					 "/" s))
		    ((fboundp s) (setq s (funcall s))))))
	(file-error (if (y-or-n-p "Signature file error, proceed? ")
			(setq s "")
		      (error "")))))
    (if (zerop (length s))
	(set-marker reply-signature-marker (point))
      (goto-char p)
      (open-line 2)
      (delete-blank-lines)
      (set-marker reply-signature-marker (point))
      (insert reply-signature-separator))))

(defun reply-signature-default-pair (arg sig-pfx-ptr)
  "Set the signature pair based on ARG and SIG-PFX-PTR list.\n
This means:
 * If ARG = member 0 of SIG-PFX-PTR, append no signature.
 * If ARG = member 1 of SIG-PFX-PTR, append user's first name.
 * If ARG = member 2 of SIG-PFX-PTR, append user's full name.
 * If ARG = member 3 of SIG-PFX-PTR, append reply-signature-default-major.
 * If ARG isn't a member of SIG-PFX-PTR, use reply-signature-default-minor."
  (cond ((eq arg (nth 0 sig-pfx-ptr))		; no signature
	 (cons t ""))
	((eq arg (nth 1 sig-pfx-ptr))		; first name only
	 (cons t (concat reply-valediction
			 (progn
			   (string-match "\\<[^ ]*\\>" (user-full-name))
			   (substring (user-full-name)
				      (match-beginning 0) (match-end 0)))
			 "\n")))
	((eq arg (nth 2 sig-pfx-ptr))		; formal name
	 (cons t (concat reply-valediction (user-full-name) "\n")))
	((eq arg (nth 3 sig-pfx-ptr))		; default file
	 (cons nil reply-signature-default-major))
	(t					; fancy signature
         reply-signature-default-minor)))

(defun reply-fluff ()
  "Remove unneeded white-space."
  (interactive)
  (save-excursion
    (goto-char 1)
    (while (re-search-forward "[ \t]+$" nil t)
      (delete-region (match-beginning 0) (point)))
    (goto-char (point-max))
    (while (looking-at "^$") (gnews-delete-line) (forward-line -1))))

;;; toggle between {e,n}-reply

(defun n-reply-toggle (pfx)
  "Redo the current headers so that they are appropriate for mailing,
and change to e-reply-mode."
  (interactive "P")
  (run-hooks 'group-reply-start-hook)
  (let (to subj ngs inrep)
    (e-reply-mode)
    (goto-char 1)
    (gnews-delete-paragraph)
    (setq to (article-to)
	  subj (reply-re-0 (article-field "Subject"))
	  ngs (article-field "Newsgroups")
	  inrep (article-field "Message-ID")
	  this-command 'n-reply-toggle)
    (insert "To: " to ?\n)
    (if mail-default-reply-to
	(insert "Reply-to: " mail-default-reply-to ?\n))
    (if mail-self-blind
	(insert "Bcc: " (user-login-name) ?\n))
    (if mail-archive-file-name
	(insert "Fcc: " mail-archive-file-name ?\n))
    (insert "Subject: " (if (string= subj "") "" (concat "Re: " subj)) ?\n
	    "Newsgroups: " ngs ?\n
	    "In-Reply-To: " inrep ?\n
	    "Organization: " gnews-organization ?\n))
  (set-buffer-modified-p t)
  (sit-for 0)
  (run-hooks 'group-reply-hook))

(defun e-reply-toggle (pfx)
  "Redo the current headers so that they are appropriate for posting,
and change to n-reply-mode.\n
With prefix argument, leave the first two headers in place, so that
simultaneous posting/mailing takes place."
  (interactive "P")
  (if (not n-reply-allowed)
      (error "Sorry, but posting is not allowed.")
    (run-hooks 'group-follow-start-hook)
    (let (p subj ngs from refs repto msgid org key wasfrom folup goal-column)
      (n-reply-mode)
      (goto-char 1)
      (e-reply-position-on-field "Subject")
      (setq p (point))
      (re-search-backward "^Subject: ")
      (setq subj (buffer-substring (+ 9 (point)) p))
      (goto-char 1)
      (if pfx
	  (progn
	    (forward-line 2)
	    (insert "\n")))
      (gnews-delete-paragraph)
      (setq ngs (article-field "Followup-To" "Newsgroups")
	    from (concat gnews-user "@" (reply-domain gnews-machine)
			 " (" gnews-name ")")
	    refs (article-field "References")
	    msgid (article-field "Message-ID")
	    refs (if (string= refs "") msgid (concat refs " " msgid))
	    repto (concat gnews-user "@" gnews-machine " (" gnews-name ")")
	    org gnews-organization
	    key (article-field "Keywords")
	    wasfrom (article-field "From")
	    dist (article-field "Distribution")
	    folup (let ((i (string-match "," ngs)))
		    (if i (substring ngs 0 i) ""))
	    this-command 'e-reply-toggle)
      (insert "Newsgroups: " ngs ?\n
	      "From: " from ?\n
	      "Subject: " subj ?\n
	      "Summary: \n"
	      "Expires: \n"
	      "References: " refs ?\n
	      "Sender: \n"
	      "Reply-To: " repto ?\n
	      "Followup-To: " folup ?\n
	      (if (string< "" wasfrom)
		  (concat "In-reply-to: " wasfrom "\n")
		"")
	      "Distribution: " dist ?\n
	      "Organization: " gnews-organization ?\n
	      "Keywords: " key
	      (if gnews-advertise
		  (concat "\nPosting-Front-End: Gnews " gnews-version) "")
	      "\n"))
    (set-buffer-modified-p t)
    (sit-for 0)
    (run-hooks 'group-follow-hook)))

;;; indented lines

(defvar reply-paragraph-start
  "^[^a-zA-Z0-9]*\\($\\|In .*\\(article\\|digest\\)\\|.*writes:$\\)")

(defun reply-yank-fill (pfx)
  "Reformat the current paragraph, trying to identify the fill-prefix
to use automatically.  Hit return if the displayed prefix is correct,
otherwise change it.\n
Non-nil prefix argument means to also right-justify.\n
It can't deal with one-line paragraphs."
  (interactive "*P")
  (let (p q r s fill-prefix list-of-lines)
    (forward-char 1)
    (while (not (looking-at reply-paragraph-start))
      (forward-line -1))
    (forward-line 1)
    (setq p (point))
    (while (and (not (eobp)) (not (looking-at reply-paragraph-start)))
      (setq r (point))
      (end-of-line)
      (setq s (point))
      (untabify r s)
      (setq s (point))
      (setq list-of-lines (cons (list (buffer-substring r s))
				list-of-lines))
      (forward-char 1))
    (setq q (point))
    (if list-of-lines
	(progn
	  (if (< 1 (length list-of-lines)) nil
	    (setq r (string-match (concat "^[" reply-prefix-match "]*"
					  reply-prefix-initials)
				  (gnaar list-of-lines)))
	    (if r (setq list-of-lines (list (list
					      (substring (gnaar list-of-lines)
							 0 (match-end 0)))))))
	  (setq fill-prefix (read-string
			      "Fill-prefix: "
			      (try-completion "" list-of-lines)))
	  (goto-char p)
	  (while (not (looking-at (concat "^" (regexp-quote fill-prefix))))
	    (forward-line 1))
	  (setq p (point))
	  (goto-char q)
	  (forward-line -1)
	  (while (not (looking-at (concat "^" (regexp-quote fill-prefix))))
	    (forward-line -1))
	  (forward-line 1)
	  (setq q (point))
	  (if (< p q) (fill-region-as-paragraph p q pfx))))))

(defun reply-indent (beg end &optional arg)
  "Indent region with reply-prefix.  Leaves point at end.  By default
no identifying blurb is put on top--but with <nn> \\[universal-argument]'s,
then the <nn>'th blurb function is invoked.\n
For simplicity, indentation only refers to whole lines.  If the beginning
of the region is in the middle of a line, the region will be redefined
to include the beginning of the line."
  (interactive "r\np")
  (goto-char beg)
  (if (bolp) nil
    (beginning-of-line)
    (setq beg (point)))
  (set-mark end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (while (not (eobp))
	(insert reply-prefix)
	(forward-line 1))
      (let ((blurb (gnews-arg-count (or arg 1)))
	    (reply-yank-min beg)
	    (reply-yank-max end))
	(if (< 0 blurb)
	    (progn
	      (funcall (nth (1- (min (length reply-blurb) blurb)) reply-blurb))
	      (goto-char beg)		; most (all!) blurbs open an extra line
	      (gnews-delete-line))))))
  (exchange-point-and-mark))

(defun reply-split-line (p &optional arg)
  "Without a prefix argument, split the line at point, while preserving
the line's prefix and inserting three blank lines.  With a prefix argument,
blank the line backwards from point to the line's prefix.\n
A line's prefix is defined as that part of the line which preceeds the
regexp reply-prefix-match."
  (interactive "*d\nP")
  (if (eolp)
      (progn (open-line 3) (forward-line 2))	; now that was easy
    (let (goal-column blank line)		; forward-line must be vertical
      (untabify (gnews-bol) p)			; must get the char count exact
      (setq p (point))
      (if arg nil
	(beginning-of-line)
	(setq line (buffer-substring (point) (gnews-eol)))
	(goto-char p)
	(delete-region (point) (gnews-eol))
	(insert ?\n line)
	(goto-char p)
	(next-line 1)				; vertical motion down
	(setq p (point)))
      (beginning-of-line)			; second copy in place
      (skip-chars-forward reply-prefix-match)
      (if (looking-at reply-prefix-initials)
	  (goto-char (match-end 0)))		; funky initials prefix
      (setq blank (- p (point)))		; got past the prefix
      (delete-char blank)
      (insert-char ?  blank)			; blank first part
      (tabify (gnews-bol) p)
      (beginning-of-line)
      (if arg nil
	(open-line 3)
	(forward-line 1)))))

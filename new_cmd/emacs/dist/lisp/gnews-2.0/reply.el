;;; reply.el: reply-mode commands for Gnews
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

(provide 'gnews-reply)

;;; primitive Gnews reply user variables--see also top of mail.el

(defvar e-reply-do-function send-mail-function
  "*Function that mails buffer contents")
(defvar n-reply-do-function 'n-reply-do
  "*Function that posts buffer contents")
(defvar n-reply-program news-inews-program
  "*If non-nil, articles are posted via the named program.  If nil, they
are posted via internal NNTP calls.\n
Note that \"inews\" is not often in most paths.")
(defvar n-reply-program-args '("-h")	; ignored by remote inews, btw!
  "*List of strings of program arguments for the news posting program")

(defvar reply-path nil
  "*If non-nil, a string to override the posting software's default for
the \"Path:\" field.  If nil, use the posting software's default.")

(defvar reply-buffer-save t
  "*If non-nil, offer to save *reply* buffers before exiting Emacs.")
(defvar reply-query-clear 0
  "*If non-nil and non-numeric, get user confirmation, before starting
a new reply that would overwrite a message in the *gnews*reply* buffer.\n
If numeric (the default), get confirmation for overwriting unsent replies
only--already sent messages will be silently overwritten.\n
If nil, no confirmation is needed; all messages will be overwritten.")

;;; interesting Gnews internal reply variables

(defvar reply-begin-marker nil
  "Marker located at the beginning of the followup reply.")

(defvar reply-was-sent nil
  "Non-nil if reply was sent.")

(defvar reply-yank-min nil
  "Beginning of region that was yanked in by a \\[reply-yank].")
(defvar reply-yank-max nil
  "End of region that was yanked in by a \\[reply-yank].")
(defvar reply-yank-tail-point nil
  "Last place in current article that a \\[reply-yank-tail] yanked from.")

(defvar n-reply-allowed nil
  "Non-nil if user is allowed to post.")

;;; Gnews reply modes

(defun e-reply-mode ()
  "e-reply-mode is used by Gnews for replying to articles via e-mail.\n
Commands are:
\\{e-reply-mode-map}"
  (interactive)
  (require 'sendmail)
  (kill-all-local-variables)
  (mail-mode)
  (if e-reply-mode-map nil
    (setq e-reply-mode-map (gnews-copy-keymap mail-mode-map))
    (gnews-key-bind e-reply-mode-map			 ; I've replaced:
		    '(("\^c?".describe-mode)
		      ("\^c-".e-reply-toggle)
		      ("\^c>".reply->-replace)
		      ("\^c.".reply-field-return)
		      ("\^c\^c".reply-return)		 ; mail-send-and-exit
		      ("\^c\^f\^b".n-reply-mail-bcc)	 ; mail-bcc ;I hate CC
		      ("\^c\^f\^c".n-reply-mail-cc)	 ; mail-cc  ; and BCC.
		      ("\^c\^f\^d".e-reply-mail-digest)
		      ("\^c\^f\^t".n-reply-mail-to)	 ; mail-to
		      ("\^c\^h".gnews-describe-mode)
		      ("\^c\^i".reply-indent)
		      ("\^c\^k".reply-digest-kill)
		      ("\^c\^n".reply-newsgroup)
		      ("\^c\^o".reply-split-line)
     		      ("\^c\^p".reply-path-alias)
		      ("\^c\^q".reply-yank-fill) ; mail-fill-yanked-message
		      ("\^c\^r".reply-rot13)
		      ("\^c\^s".reply-send)	 	; mail-send
   		      ("\^c\^t".reply-to-simplify)
		      ("\^c\^v".reply-signature-insert)
		      ("\^c\^w".reply-signature)	; mail-signature
		      ("\^c\^y".reply-yank)		; mail-yank-original
		      ("\^c\^z".reply-yank-tail)
		      ("\^c\^]".reply-return-no-send))))
  (use-local-map e-reply-mode-map)
  (make-local-variable 'reply-was-sent)
  (setq major-mode 'e-reply-mode
	mode-name "E-Reply"
	mail-reply-buffer news-buffer
	buffer-offer-save reply-buffer-save)
  (run-hooks 'e-reply-hook))

(defun n-reply-mode ()
  "n-reply-mode is used by Gnews for replying to articles via USENET.
It may be used to post an original article by going to the pseudoarticle
at the end of a newsgroup.\n
Commands are:
\\{n-reply-mode-map}"
  (interactive)
  (require 'sendmail)
  (kill-all-local-variables)
  (if n-reply-mode-map nil
    (setq n-reply-mode-map (make-sparse-keymap))
    (gnews-key-bind n-reply-mode-map
		    '(("\^c?".describe-mode)
		      ("\^c-".n-reply-toggle)
		      ("\^c.".reply-field-return)
		      ("\^c<".n-reply-beginning)
		      ("\^c>".reply->-replace)
		      ("\^c%".reply->-count)
		      ("\^c\^c".reply-return)
		      ("\^c\^f\^d".n-reply-distribution)
		      ("\^c\^f\^f".n-reply-followup-to)
		      ("\^c\^f\^k".n-reply-keywords)
		      ("\^c\^f\^m".n-reply-summary)
		      ("\^c\^f\^n".n-reply-newsgroups)
		      ("\^c\^f\^r".n-reply-references)
		      ("\^c\^f\^s".n-reply-subject)
		      ("\^c\^h".gnews-describe-mode)
		      ("\^c\^i".reply-indent)
		      ("\^c\^k".reply-digest-kill)
		      ("\^c\^m\^t".n-reply-mail-to)
		      ("\^c\^m\^b".n-reply-mail-bcc)
		      ("\^c\^m\^c".n-reply-mail-cc)
		      ("\^c\^m\^s".n-reply-mail-subject)
		      ("\^c\^m\^w".n-reply-mail-signature)
		      ("\^c\^n".reply-newsgroup)
		      ("\^c\^o".reply-split-line)
		      ("\^c\^q".reply-yank-fill)
		      ("\^c\^r".reply-rot13)
		      ("\^c\^s".reply-send)
		      ("\^c\^v".reply-signature-insert)
		      ("\^c\^w".reply-signature)
		      ("\^c\^y".reply-yank)
		      ("\^c\^z".reply-yank-tail)
		      ("\^c\^]".reply-return-no-send))))
  (use-local-map n-reply-mode-map)
  (make-local-variable 'reply-was-sent)
  (setq major-mode 'n-reply-mode
	mode-name "N-Reply"
	mail-reply-buffer news-buffer
	buffer-offer-save reply-buffer-save)
  (run-hooks 'n-reply-hook))

;;; Group mode reply commands

(defvar gnews-advertise nil
  "*If non-nil, include a Posting-Front-End field in followups.")

(defun group-reply (&optional mail-to arg)
  "Reply by e-mail to current article.  With non-nil prefix argument PFX,
set up the current article for e-mail forwarding."
  ;; second argument is not used, but needed for group-digit
  (interactive "P\np")
  (if (and mail-to (not (stringp mail-to)))	; stringp => digest mail reply
      (group-forward-article)
    (run-hooks 'group-reply-start-hook)
    (if gnews-digest-p nil (article-forward-intern nil))
    (setq reply-yank-tail-point (article-max))
    (switch-to-buffer (get-buffer-create reply-buffer-name))
    (auto-save-mode auto-save-default)
    (if (and (buffer-modified-p)
	     reply-query-clear
	     (not (if reply-was-sent
		      (or (numberp reply-query-clear)
			  (y-or-n-p "Reply was sent--erase? "))
		    (y-or-n-p "Unsent reply in progess--erase? "))))
	(and gnews-flush-typeahead (discard-input) nil)
      (e-reply-mode)
      (erase-buffer)
      (setq article-replied article-current
	    group-replied group-current
	    reply-was-sent nil
	    reply-body-marker (make-marker)
	    reply-signature-marker (make-marker))
      (let ((to (or mail-to (article-to)))
	    (subj (reply-re-0 (article-field "Subject")))
	    (ngs (article-field "Newsgroups"))
	    (inrep (article-field "Message-ID"))
	    (digest (not (and article-grab-point article-grab-point-old))))
	(insert "To: " to ?\n)
	(if mail-default-reply-to
	    (insert "Reply-to: " mail-default-reply-to "\n"))
	(if mail-self-blind
	    (insert "Bcc: " (user-login-name) "\n"))
	(if mail-archive-file-name
	    (insert "Fcc: " mail-archive-file-name "\n"))
	(if (and mail-to digest) (setq subj ""))
	(insert "Subject: " (if (string= subj "") "" (concat "Re: " subj)) ?\n
		"Newsgroups: " ngs ?\n
		"In-Reply-To: " inrep ?\n
		"Organization: " gnews-organization ?\n
		"Cc: " (if (if mail-to digest t) ""
			 (article-field-intern "From")) ?\n
	        (if (interactive-p) ?\n ""))
	(if (interactive-p) (run-hooks 'group-reply-hook))
	(set-buffer-modified-p nil)
	t))))

(defun group-reply-yank (pfx arg)
  "Reply by e-mail to current article with original article inserted.
With non-nil prefix argument PFX, set up the current article for e-mail
forwarding."
  (interactive "P\np")
  (if pfx (group-forward-article)
    (if (group-reply)
	(progn
	  (forward-paragraph)
	  (save-excursion
	    (reply-yank 1))
	  (run-hooks 'group-reply-hook)
	  (set-buffer-modified-p nil)))))

(defun group-forward-article ()		 ; AR
  "Forward current article to someone by e-mail."
  (interactive)		
  (article-forward-intern nil)
  (run-hooks 'group-reply-start-hook)
  (setq reply-yank-tail-point (article-max))
  (switch-to-buffer (get-buffer-create reply-buffer-name))
  (auto-save-mode auto-save-default)
  (if (and (buffer-modified-p)
	   reply-query-clear
	   (not (if reply-was-sent
		    (or (numberp reply-query-clear)
			(y-or-n-p "Reply was sent--erase? "))
		  (y-or-n-p "Unsent reply in progess--erase? "))))
      (and gnews-flush-typeahead (discard-input) nil)
    (e-reply-mode)
    (erase-buffer)
    (setq article-replied article-current
	  group-replied group-current
	  reply-was-sent nil
	  reply-body-marker (make-marker)
	  reply-signature-marker (make-marker))
    (insert "To: " ?\n)
    (if mail-default-reply-to
	(insert "Reply-to: " mail-default-reply-to "\n"))
    (if mail-self-blind
	(insert "Bcc: " (user-login-name) "\n"))
    (if mail-archive-file-name
	(insert "Fcc: " mail-archive-file-name "\n"))
    (let ((subj (reply-re-0 (article-field "Subject")))
	  (repto (concat gnews-user "@" gnews-machine " (" gnews-name ")")))
      (insert "Subject: " (concat "[" (gnews-string-as-buffer
					(article-field "From") 'b
					(skip-chars-forward "^ \t"))
				  ": " (or (article-field "Subject") "")
				  "]") ?\n
	      "Organization: " gnews-organization ?\n ?\n)
      (forward-paragraph)
      (let ((reply-prefix reply-forward-prefix))
	(reply-yank 0)
	(reply-forward-blurb))
      (goto-char 1)			 ; Sould use n-reply-mail-to,
      (end-of-line)			 ; but it doesn't work.
      (if (interactive-p) (run-hooks 'group-reply-hook))
      t)))

(defun group-follow (pfx arg)
  "Follow up to current article with an original article.  Prefix args
PFX and ARG mean start a To: field for e-mailing a copy to the original
poster.\n
This may also be used to post directly: go to the newsgroup you wish
to post to, then go to the pseudo-article at the end, via \\[group-last],
and then run this command.\n
Note that if the \"Followup-To:\" field is \"poster\", only an e-mail
reply is set up.\n
Mailing of the entire buffer is attempted whenever the first line
does not begin with a \"Path:\", \"From:\", or \"Newsgroups:\" header.
Everything from the first such header on down is posted.  This is
the theory, and it seems to work in practice, but one never can tell
if some glitch will post to the net what you intended to be private.
The cautious will restrict this feature to the convenience of simul-
taneous forwarding and posting.\n
\(Of course, if you think your e-mail is really private, well, that's
another story for another day.\)"
  (interactive "P\np")
  (cond ((string= (article-field "Followup-To") "poster")
	 (group-reply))
	(n-reply-digest-mail
	 (group-reply (if (stringp n-reply-digest-mail)
			  n-reply-digest-mail
			(article-field-raw "From"))))
	(n-reply-allowed
	 (run-hooks 'group-follow-start-hook)
	 (if gnews-digest-p nil (article-forward-intern nil))
	 (setq reply-yank-tail-point (article-max))
	 (switch-to-buffer (get-buffer-create reply-buffer-name))
	 (auto-save-mode auto-save-default)
	 (if (and (buffer-modified-p)
		  reply-query-clear
		  (not (if reply-was-sent
			   (or (numberp reply-query-clear)
			       (y-or-n-p "Reply was sent--erase? "))
			 (y-or-n-p "Unsent reply in progess--erase? "))))
	     (and gnews-flush-typeahead (discard-input) nil)
	   (n-reply-mode)
	   (erase-buffer)
	   (setq article-replied article-current
		 group-replied group-current
		 reply-was-sent nil
		 reply-body-marker (make-marker)
		 reply-signature-marker (make-marker))
	   (if (< 80 (window-width))	; quite often these
	       (reply-wide))		; people are pond scum
	   (let* ((ngs (article-field "Followup-To" "Newsgroups"))
		  (from (concat gnews-user "@" (reply-domain gnews-machine)
				" (" gnews-name ")"))
		  (sub (article-field "Subject"))
		  (subj (if (string= sub "") "" (reply-re-1 sub)))
		  (refs (article-field "References"))
		  (msgid (article-field "Message-ID"))
		  (refs (if (string= refs "") msgid (concat refs " " msgid)))
		  (repto (concat gnews-user "@" gnews-machine
				 " (" gnews-name ")"))
		  (org gnews-organization)
		  (key (article-field "Keywords"))
		  (wasfrom (article-field "From"))
		  (dist (article-field "Distribution"))
		  (folup (let ((i (string-match "," ngs)))
			   (if i (substring ngs 0 i) ""))))
	     (if pfx (insert "To: " (article-to) "\nSubject: " subj "\n\n"))
	     (setq reply-begin-marker (point-marker))
	     (if reply-path (insert "Path: " reply-path ?\n))
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
			 (concat "\nPosting-Front-End: Gnews "
				 gnews-version) "")
		     (if (interactive-p) "\n\n" ""))
	     (forward-line 1)
	     (if (interactive-p) (run-hooks 'group-follow-hook))
	     (set-buffer-modified-p nil)
	     t)))
	(t
	 (error "Sorry, but posting is not allowed."))))

(defun group-follow-yank (pfx arg)
  "Follow up to current article with that article inserted, indented
using the value of reply-prefix.\n
Otherwise identical to group-follow, which see."
  (interactive "P\np")
  (let ((end (< article-final article-current))
	file goal-column)
    (if end (setq file (read-file-name "Include file: " nil "" t)
		  file (if (string< "" file) (expand-file-name file))))
    (if (group-follow pfx arg)
	(progn
	  (if file
	      (progn
		(goto-char (point-max))
		(insert ?\n?\n)
		(insert-file file)
		(if (looking-at "^\\(Newsgroups\\|Path\\|From\\): ")
		    (delete-region 1 (point)))))
	  (forward-paragraph)
	  (if (not file) (open-line 1))
	  (forward-line 1)
	  (if (not end)
	      (save-excursion
		(reply-yank 1)))
	  (run-hooks 'group-follow-hook)
	  (set-buffer-modified-p nil)))))

(defun group-cancel (art-list)
  "Cancel the current article--must be owner or superuser to do so."
  (interactive (list (list article-current)))
  (setq art-list (car art-list))  ;; not loopable--ugh
  (let ((from (article-field "From"))
	(send (article-field "Sender"))
	(patt (concat "^" (regexp-quote gnews-user)))
	(article-min 1))
    (cond ((and (or (= (user-real-uid) 0)
		    (string-match patt from)
		    (string-match patt (setq from send)))
		(y-or-n-p "Really cancel? "))
	   (message "Cancelling...")
	   (save-excursion
	     (set-buffer (get-buffer-create article-cancel-buffer-name))
	     (erase-buffer)
	     (if reply-path (insert "Path: " reply-path ?\n))
	     (insert "Newsgroups: " (article-field "Newsgroups") ?\n
		     "From: " from ?\n
		     "Subject: cancel " (article-field "Message-ID") ?\n
		     "Control: cancel " (article-field "Message-ID") ?\n
		     "Distribution: " (article-field "Distribution") ?\n
		     "Organization: " gnews-organization ?\n ?\n
		     (if gnews-advertise
			 (concat "Cancelled in Gnews " gnews-version "\n")
		       ""))
	     (run-hooks 'article-cancel-hook)
	     (if n-reply-program
		 (apply 'call-process-region article-min (point-max)
			n-reply-program () 0 () n-reply-program-args)
	       (nntp-exec-post)))
	   (message "Cancelling...done"))
	  ((message "Not owner" (ding)))))
  (gnews-flush))

(defun group-supersede ()
  "Begin a posting to supersede the current article--must be sender to
do so.\n
The function prompts for a file to use.  It tries to determine if the
named file already has headers--if it doesn't think so it will insert
them.  If no file name is given, superseding will proceed starting with
the current article's contents."
  (interactive)
  (article-forward-intern nil)
  (let ((from (article-field "From"))
	(send (article-field "Sender"))
	(patt (concat "^" (regexp-quote gnews-user)))
	(dont ":Path:Message-ID:Date:Supersedes:Lines:Xref:")
	file head body)
    (cond ((and (or (= (user-real-uid) 0)
		    (string-match patt from)
		    (string-match patt (setq from send))))
	   (setq file (read-file-name "Supersede from file: " nil "" t)
		 file (if (string< "" file) (expand-file-name file))
		 body (buffer-substring (article-min) (article-max)))
	   (switch-to-buffer (get-buffer-create article-cancel-buffer-name))
	   (n-reply-mode)
	   (erase-buffer)
	   (setq article-replied article-current
		 group-replied group-current
		 reply-was-sent nil
		 reply-body-marker (make-marker)
		 reply-signature-marker (make-marker))
	   (if file
	       (progn
		 (insert-file file)
		 (goto-char 1)
		 (setq head (looking-at "^\\(Newsgroups\\|Path\\|From\\): ")))
	     (insert body)
	     (setq head t))
	   (goto-char 1)
	   (if head
	       (progn
		 (if reply-path (insert "Path: " reply-path ?\n))
		 (mapcar '(lambda (f) 
			    (if (not (string-match
				       (concat ":" (car f) ":") dont))
				(insert (car f) ": " (cdr f) "\n")))
			 (cdr article-field-list))
		 (insert ?\n)))
	   (goto-char 1)
	   (forward-paragraph)
	   (insert "Supersedes: " (article-field "Message-ID") "\n")
	   (run-hooks 'article-supersede-hook))
	  ((message "Not owner" (ding))
	   (gnews-flush)))))

(defun group-reply-return ()
  "Return to the article which the current reply in progress was
originally made in response."
  (interactive)
  (cond ((or (and (= article-current article-replied)
		  (string= group-current group-replied))
	     (not (y-or-n-p "Return to replied-to article? ")))
	 (switch-to-buffer news-buffer)
	 (if (< article-final article-current)
	     (group-last)
	   (gnews-hilite)
	   (if (article-done) (article-quit))))
	((message "returning...")
	 (if (string= group-current group-replied) nil
	   (group-quit-intern)
	   (group-get group-replied))
	 (if (< article-final article-replied)
	     (group-last)
	   (cond ((catch 'article-nil (article-get article-replied nil t))
		  (article-junk-local)
		  (message "whoops--%d was just cancelled" article-replied))
		 ((message "returning...done"))))))
  (gnews-flush))

;;; reply functions

(defun reply (&optional arg)
  "Post/mail the article in the current buffer, based on mode.  ARG is
looked up in {e,n}-reply-signature-prefix-pointers; how far down ARG
occurs in that variable determines what signature to append."
  (interactive "*p")
  (let* ((mode (substring (symbol-name major-mode) 0 1))
	 (sig-pp (eval (intern
			 (concat mode "-reply-signature-prefix-pointers"))))
	 (do-func (eval (intern (concat mode "-reply-do-function"))))
	 (hook (intern (concat mode "-reply-send-hook")))
	 (word (if (string= mode "n") "Post" "Send")))
    ;; appending the signature
    (let ((s-pair (reply-signature-default-pair arg sig-pp)))
      (if s-pair (progn (reply-signature nil s-pair) (run-hooks hook))))
    ;; display at bottom
    (goto-char (point-max))
    (forward-line -1)
    (recenter -1)
    (sit-for 0)
    ;; the actual sending is set inside a cond
    ;; so as to return nil in case of failure
    (cond ((y-or-n-p (concat word "? "))
	   (message "%sing..." word)
	   (let ((mail-header-separator "")) (funcall do-func))
	   (delete-auto-save-file-if-necessary)
	   ;; (set-buffer-modified-p nil) not yet
	   (message "%sing...done" word)))))

(defun n-reply-do ()
  "Post current buffer, from first Newsgroups/Path/From line on down.
Anything above the first such header must be e-mail; the article being
posted is included with the e-mail."
  (let ((article-min (save-excursion
		       (goto-char (point-min))
		       (re-search-forward
			 "^\\(Newsgroups\\|Path\\|From\\): ")
		       (match-beginning 0))))
    ;; strip off null headers
    (save-excursion
      (goto-char article-min)
      (narrow-to-region 1 (save-excursion (forward-paragraph) (point)))
      (delete-matching-lines "^[a-zA-Z-]*:[ \t]*$")
      (widen))
    ;; posting
    (if n-reply-program
	(apply 'call-process-region article-min (point-max)
	       n-reply-program () 0 () n-reply-program-args)
      (nntp-exec-post))
    ;; check for mail headers at top
    (cond ((< (point-min) article-min)
	   (message "And sending...")
	   (funcall e-reply-do-function)
	   (message "And sending...done")))))

(defun nntp-exec-post ()
  "Post the current buffer via NNTP internals."
  (interactive)			; Perhaps this is a no-no.
  (if (nntp-exec t t "post")
      (let ((buf (current-buffer))
	    (art (buffer-substring (article-min) (point-max))))
	(set-buffer nntp-buffer)
	(erase-buffer)
	(insert art)
	(goto-char 1)
	(while (re-search-forward "^\\." nil t)
	  (replace-match "..")
	  (forward-line 1))
	(goto-char (point-max))
	(insert ".\n")
	(send-string nntp (buffer-string))	 ; I ought to catch
	(accept-process-output nntp)		 ; failed postings.
	(goto-char (point-max))
	(forward-line -1)
	(setq nntp-info (buffer-substring (point) (1- (point-max))))
	(set-buffer buf))
    (error "%s" nntp-info)))

;;; reply support functions

(defun article-to ()
  "Return string to be used for the To: field.  User redefinition of
this function is best done via the group-*-start-hooks."
  (article-field "Reply-To" "Reply-Path" "From"))

(defun reply-wide ()
  "How to deal with wide terminals when posting."
  (setq fill-column (min fill-column 79))
  (auto-fill-mode 1))

(defun reply-forward-blurb ()
  "Insert an identifying blurb for forwarded material."
  (goto-char reply-yank-min)
  (open-line 1)
  (insert reply-prefix "From: " (article-field "From") ?\n
	  reply-prefix "Subject: " (article-field "Subject") ?\n
          reply-prefix "Newsgroups: " (article-field "Newsgroups") ?\n
	  reply-prefix "Message-ID: " (article-field "Message-ID") ?\n
	  reply-prefix)
  (forward-char 1))

;;; {e,n}-reply-mode

;;; basic functions

(defun reply-send (arg)
  "Send the reply."
  (interactive "p")
  (if (reply arg) (setq reply-was-sent t))
  (set-buffer-modified-p t)
  (sit-for 0))

(defun reply-return (arg)
  "Send the reply if already unsent, and returns to the original article."
  (interactive "p")
  (if (not reply-was-sent) (reply-send arg))
  (reply-return-no-send))

(defun reply-return-no-send ()
  "Return to the original article immediately."
  (interactive)
  (if (and (fboundp 'nntp-run-p)
	   (nntp-run-p))
      (progn				; Gnews invocation
	(switch-to-buffer news-buffer)
	(group-reply-return))
    (bury-buffer (current-buffer))	; isolated invocation
    (call-interactively 'switch-to-buffer)))

(defun reply-yank (arg)
  "Yank in text of current article (which is not necessarily the one being
responded to).  The identifying blurb is formed by one of the members of the
list reply-blurb.  If the prefix argument is 0, no blurb is given.  If it is
<nn> \\[universal-argument]'s, the function (nth <nn> reply-blurb) is run."
  (interactive "p")
  (delete-windows-on mail-reply-buffer)
  (insert-buffer mail-reply-buffer)
  (beginning-of-line)
  (gnews-delete-paragraph)
  (gnews-delete-line)
  (setq reply-yank-min (point) reply-yank-max (mark))
  (reply-indent reply-yank-min reply-yank-max)
  (if (< 0 arg)
      (funcall (nth (min (1- (length reply-blurb)) (gnews-arg-count arg))
		    reply-blurb)))
  (goto-char reply-yank-min)
  (open-line 2)
  (delete-blank-lines)
  (forward-char 1))

(defun reply-yank-tail (count)
  "Yank in the last COUNT lines from the replied-to message."
  (interactive "p")
  (let (str)
    (save-excursion
      (set-buffer mail-reply-buffer)
      (goto-char reply-yank-tail-point)
      (forward-line (- count))
      (setq str (buffer-substring (point) reply-yank-tail-point)
	    reply-yank-tail-point (point)))
    (insert str)
    (exchange-point-and-mark)
    (open-line 1)
    (forward-line 1)
    (reply-indent (point) (mark))))

;;; convenient functions

(defun reply-digest-kill (count)
  "Kill individual articles within a digest.  A prefix argument gives a
count of how many to kill--positive for forward, negative for backward."
  (interactive "*p")
  (let ((top (save-excursion (goto-char 1) (forward-paragraph) (point))))
    (if (<= (point) top)			; get past any headers
	(progn (forward-paragraph) (forward-line 1)))
    (while (and (< 0 count) (< (point) (point-max)))
      (undo-boundary)
      (delete-region			; delete in the forward direction
	(point)
	(progn
	  (search-forward
	    (concat "\n" reply-prefix reply-digest-separator) nil 0)
	  (forward-line 1)
	  (point)))
      (setq count (1- count)))
    (while (and (< count 0) (< top (1- (point))))
      (undo-boundary)
      (delete-region			; delete in the backward direction
	(save-excursion
	  (search-backward
	    (concat "\n" reply-prefix reply-digest-separator) top 0)
	  (forward-line 1)
	  (point))
	(point))
      (setq count (1+ count)))))

(defun reply-rot13 (beg end)
  "Rot13 the region."
  (interactive "r")
  (gnews-rot13 beg end))

(defun reply->-count ()
  "Return the %-age of `>' lines."
  (interactive)
  (save-excursion
    (goto-char (if (eq major-mode 'e-reply-mode) (article-min) 1))
    (let ((fill-column nil) (count 0) (total 0) %-age)
      (while (not (eobp))
	(if (looking-at "^>") (setq count (1+ count)))
	(setq total (1+ total))
	(forward-line 1))
      (setq %-age (/ (* count 100) total))
      (if (interactive-p)
	  (message "Percentage > lines: %d%%" %-age)
	%-age))))

(defun reply->-replace (beg end pfx &optional newpfx)
  "Change the reply prefix in the region.  With prefix argument, multiple
prefixes will be changed; without a prefix argument, only the first one
will be changed.  The command prompts for the new prefix.\n
In Lisp code, arguments are BEG, END for the region, PFX non-nil for
multiple replacement, and an optional NEWPFX for the replacement prefix.
If the last is nil, the user will be prompted."
  (interactive "*r\nP")
  (let ((oldpfx (concat "^" reply-prefix (if pfx "+")))
	goal-column mb me)
    (if newpfx nil
      (setq newpfx (read-from-minibuffer
		     (format "Change leading %s%s to: "
			     reply-prefix (if pfx "s" "")))))
    (save-excursion
      (goto-char end)
      (while (<= beg (point))
	(if (looking-at oldpfx)
	    (progn
	      (setq mb (match-beginning 0) me (match-end 0))
	      (delete-region mb me)
	      (while (< mb me)
		(insert newpfx)
		(setq mb (1+ mb)))))
	(forward-line -1)))))

(defun reply-newsgroup (arg)
  "Insert a newsgroup name.  Prompt for newsgroup, offering name completion
and abbreviation.\n
With no prefix argument, restrict to the current Newsgroups: header.
With one prefix argument, restrict to your .gnewsrc.
With two prefix arguments, restrict to your site's active list."
  (interactive "*p")
  (let ((grouplist (cond ((= arg 1)
			  (gnews-comma-parse (article-field "Newsgroups")))
			 ((= arg 4)
			  group-roster)
			 ((= arg 16)
			  (roster-all)))))
    (insert (group-name-read "Insert newsgroup: " grouplist 'news-all))))

;;; header motion

(defun n-reply-newsgroups ()
  "Move point to end of Newsgroups header."
  (interactive)
  (expand-abbrev)
  (reply-position-on-field "Newsgroups" t))

(defun n-reply-subject ()
  "Move point to end of Subject header."
  (interactive)
  (expand-abbrev)
  (reply-position-on-field "Subject" t))

(defun n-reply-followup-to ()
  "Move point to end of Followup-To header."
  (interactive)
  (expand-abbrev)
  (reply-position-on-field "Followup-To" t))

(defun n-reply-distribution ()
  "Move point to end of Distribution header."
  (interactive)
  (expand-abbrev)
  (reply-position-on-field "Distribution" t))

(defun n-reply-keywords ()
  "Move point to end of Keywords header."
  (interactive)
  (expand-abbrev)
  (reply-position-on-field "Keywords" t))

(defun n-reply-summary ()
  "Move point to end of Summary header."
  (interactive)
  (expand-abbrev)
  (reply-position-on-field "Summary" t))

(defun n-reply-references (arg)
  "Delete ARG many references (starting from the oldest)."
  (interactive "p")
  (expand-abbrev)
  (reply-position-on-field "References" nil)
  (let ((p (point)))
    (if (search-forward "<" (gnews-eol) t (1+ arg))
	(progn
	  (delete-region (1+ p) (1- (point)))
	  (forward-char -2)
	  (if (looking-at "<") (delete-char 1 t)))
      (delete-region p (gnews-eol)))))

(defun n-reply-mail-cc ()
  "Move point to end of Cc field in a mail header.  Create a Cc field
if none exists."
  (interactive)
  (expand-abbrev)
  (set-marker reply-body-marker (point))
  (or (e-reply-position-on-field "cc" t)
      (progn (e-reply-position-on-field "to")
	     (insert "\nCc: "))))

(defun n-reply-mail-bcc ()
  "Move point to end of Bcc field in a mail header.  Create a Bcc field
if none exists."
  (interactive)
  (expand-abbrev)
  (set-marker reply-body-marker (point))
  (or (e-reply-position-on-field "bcc" t)
      (progn (e-reply-position-on-field "to")
	     (insert "\nBcc: "))))

(defun n-reply-mail-subject ()
  "Move point to end of Subject field in a mail header.  Creates a Subject
field if none."
  (interactive)
  (expand-abbrev)
  (set-marker reply-body-marker (point))
  (e-reply-position-on-field "subject" t))

(defun n-reply-mail-to ()
  "Move point to end of To field in a mail header.  Creates a To field if
none."
  (interactive)
  (expand-abbrev)
  (set-marker reply-body-marker (point))
  (e-reply-position-on-field "to" t))

(defun e-reply-mail-digest ()
  "Convert a digest@site address to a digest-request@site address."
  (interactive)
  (let ((case-fold-search t))
    (beginning-of-line)
    (if (re-search-forward "\\(to\\|b?cc\\): \\([^@]+\\)@" nil (gnews-eol))
	(replace-match "\\1: \\2-request@" t)
      (message "Not a To: line" (ding)))))

(defun n-reply-mail-signature (arg)
  "Append a signature in pre-article portion."
  (interactive "p")
  (goto-char (marker-position reply-begin-marker))
  (open-line 1)
  (reply-signature-insert
   (reply-signature-default-pair arg e-reply-signature-prefix-pointers)))

(defun reply-field-return ()
  "Return to where point was before visiting the headers."
  (interactive)
  (goto-char reply-body-marker))

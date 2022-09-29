;;; Help.el: help/debug functions for Gnews
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

;;; help functions

(defvar gnews-Info-nodes
  '((news-mode."news") (group-mode."group") (article-mode."group")
    (index-mode."index") (e-reply-mode."reply") (n-reply-mode."reply")
    (roster-mode.nil) (hook-kill-mode."hook-kill") (gnews-edit-mode."edit"))
  "Association list of Info nodes to visit for the given Gnews mode.")

(defun gnews-describe-mode (pfx)
  "Describe the current mode in detail.\n
Without an argument, it expands upon the usual describe-mode by filling
in the doc strings of all functions.\n
With an argument, it goes to the appropriate section of the Info manual."
  (interactive "P")
  (if pfx
      (let ((node (cdr (assoc major-mode gnews-Info-nodes))))
	(if (and (string= node "group") gnews-digest-p)
	    (setq node "digest"))
	(require 'info)
	(Info-find-node "gnews" node))
    (message "just a moment...")
    (let ((k (substitute-command-keys
	       (concat "\\{" (symbol-name major-mode) "-map}"))))
      (with-output-to-temp-buffer "*Help*"
	(princ
	  (gnews-string-as-buffer k t
	    (let (f)
	      (delete-matching-lines "Prefix Command")
	      (goto-char 1)
	      (while (not (eobp))
		(forward-line 1)
		(if (eobp) nil
		  (if (eolp) (gnews-delete-line))
		  (forward-char 16)
		  (setq f (read (buffer-substring (point) (gnews-eol))))
		  (end-of-line)
		  (insert ":\n"
			  (if (fboundp f)
			      (if (eq f 'shell)
				  "Run an inferior shell."
				(or (documentation f) "undocumented"))
			    "undocumented")
			  "\n\n")))))))))
  (message ""))

;;; Info file commands

(defun gnews-Info ()
  "Go to the Gnews on-line Info manual."
  (interactive)
  (require 'info)
  (Info-find-node "gnews" "top"))

(defun gnews-features ()
  "Describe the major features of Gnews compared with rrn."
  (interactive)
  (require 'info)
  (Info-find-node "gnews" "features"))

(defun gnews-ances ()
  "Summarize the known Gnews bugs and other nuisances."
  (interactive)
  (require 'info)
  (Info-find-node "gnews" "bugs"))

(fset 'gnews-bugs 'gnews-ances)		; Some people just got no sensayuma.

(defun gnews-customize ()
  "For your perusal, edification and/or amusement, the author's own
customizations (along with some others') are presented, give or take
a Rich L Rosen."
  (interactive)
  (require 'info)
  (Info-find-node "gnews" "customize"))

(defun gnews-legalese-no-warranty ()
  "Describe Gnews' lack of warranty."
  (interactive)
  (require 'info)
  (Info-find-node "gnews" "NO WARRANTY"))

(defun gnews-legalese-license ()
  "Display the Gnews General License."
  (interactive)
  (require 'info)
  (Info-find-node "gnews" "License"))

(defun gnews-legalese-copying ()
  "Display the copying permissions for Gnews."
  (interactive)
  (require 'info)
  (Info-find-node "gnews" "Copying"))


;;; creating the manual

(defun gnews-texinfo-info ()
  "Convert the current buffer into the Gnews Info file."
  (interactive)
  (save-excursion
    (goto-char 1)
    (if (re-search-forward "^@settitle Gnews" nil 200)
	(progn
	  (re-search-backward "^\\(@setfilename\\)\\(.*\\)$")
	  (let (buffer-read-only)
	    (replace-match (concat "\\1 "
				   (regexp-quote
				     (directory-file-name Info-directory))
				   "/gnews")))
	  (put 'l 'texinfo-format 'texinfo-format-code)
	  (texinfo-format-buffer)))))

(defun gnews-texinfo-tex ())

;;; debugging aids

(defun gnews-variable-values ()
  "Insert into the current buffer the names and values of the
Gnews variables mentioned in gnews-variable-list.  If the latter
is unbound, then all Gnews variables will be listed."
  (interactive)
  (mapcar (function
	    (lambda (var)
	      (insert (prin1-to-string var) ":"
		      (if (keymapp (eval var))
			  (substitute-command-keys
			    (concat "\\{" (symbol-name var) "}"))
			(concat "\n" (prin1-to-string (eval var)) "\n"))
		      "\n")))
	  (sort
	    (if (boundp 'gnews-variable-list)
		gnews-variable-list
	      (apply 'append
		     (mapcar
		       (function
			 (lambda (pfx)
			   (mapcar 'intern
				   (all-completions
				     (symbol-name pfx) obarray 'boundp))))
		       '(gnews nntp amark news group article index roster
			       e-reply n-reply reply path hook-kill))))
	    'string<)))

(defun gnews-functions ()
  "Insert into the current buffer the names of all Gnews functions
currently bound."
  (interactive)
  (mapcar (function (lambda (fun)
		      (insert (prin1-to-string fun) ?\n)))
	  (sort
	    (apply 'append
		   (mapcar (function
			     (lambda (pfx)
			       (mapcar 'intern
				       (all-completions (symbol-name pfx)
							obarray 'fboundp))))
			   '(gnews nntp amark umark news group article index
				   e-reply n-reply reply roster path
				   hook-kill)))
	    'string<)))

(defun gnews-bug-report (pfx topic)
  "Report a Gnews bug.\n
Prompts for bug subject.  Leaves you in a mail buffer."
  (interactive "P\nsGnews bug subject: ")
  (if pfx (load-library "emacsbug"))
  (mail nil gnews-bug-address topic nil (if pfx bug-gnu-emacs))
  (goto-char (point-max))
  (insert "In Gnews version " (gnews-version)
	  " [NNTP=" (nntp-version) "] under\n"
	  (emacs-version))
  (forward-line -1)
  (re-search-forward "\\([0-9] 19[89][0-9]\\) \\(on.*\\)$")
  (let ((sys (gnews-match 2)))
    (replace-match "\\1")
    (insert "\n" sys "\n\n"))
  (message (substitute-command-keys
	     "Type \\[mail-send] to send bug report.")))

(defun gnews-rename ()
  "Transform names in the current buffer from pre-2.0 to 2.0."
  (interactive)
  (mapcar '(lambda (x)
	     (goto-char 1)
	     (while (re-search-forward
		      (concat "\\<" (symbol-name (car x)) "\\>") nil t)
	       (replace-match (symbol-name (cdr x)))))
	  '((gnews-hook-list.hook-kill-all)
	    (gnews-hook-do.hook-kill-do)
	    (gnews-hook-pre.hook-kill-pre)
	    (gnews-hook-per.hook-kill-per)
	    (gnews-hook-post.hook-kill-post)
	    (gnews-hook-kill-alist.hook-kill-alist)
	    (gnews-hook-continue.hook-kill-continue)
	    (gnews-hook-mode.hook-kill-mode)
	    (gnews-hook-hook.hook-kill-hook)
	    (gnews-hook-junk-message.hook-kill-junk-message)
	    (gnews-hook-pop.hook-kill-pop)
	    (gnews-hook-exit.hook-kill-exit)
	    (gnews-hook-abort.hook-kill-abort)
	    (group-same-subject-prompt.group-prompt-same-subject)
	    (article-init-display.article-display-count)
	    (index-show-all.index-show-kills)
	    (article-forward-adjust.article-forward-intern)
	    (gnews-ignore-headers.article-header-ignore)
	    (group-show-headers.article-header-show)
	    (gnews-show-headers.article-header-show)
	    (group-all-headers.article-header-all-p)))
  (goto-char 1)
  (while (re-search-forward "\\<gnews-hook-insert" nil t)
    (replace-match "hook-kill-insert"))
  (goto-char 1)
  (while (re-search-forward "group-\\([bf]\\)-prompt" nil t)
    (replace-match "group-query-p\\1"))
  (goto-char 1)
  (while (re-search-forward "\\(news\\|group\\)-\\([^-]*\\)-prompt" nil t)
    (replace-match "\\1-prompt-\\2"))
  (goto-char 1)
  (while (re-search-forward "\\(autoload[ \t\n]+\\)'gnews\\([ \t\n]\\)" nil t)
    (replace-match "\\1'Gnews\\2")))

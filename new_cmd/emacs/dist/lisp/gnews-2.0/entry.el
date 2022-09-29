;;; entry.el: main Gnews entry point
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

;;; To see the basic user variables, look in Init.el
;;; To see the index-mode user variables, look in index.el
;;; To see the reply-mode user variables, look in {mail,reply}.el

;;; The main entry to Gnews

(defun gnews (&rest ignore)
  "Inform the user of how to convert from pre-2.0 to 2.0 and up."
  (interactive)
  (switch-to-buffer "*gnews*warning*")
  (insert "** WARNING **\n\n\
`M-x gnews' has been renamed `M-x Gnews' for the next several versions.
This is because I (Matthew P Wiener) have renamed numerous internal
variables in the source, so as to make many things cleaner and more
logical overall.  (For example, several variables in the manual's
index are now grouped together appropriately.)

You will have to fix a few other internal variables that you might use
somewhere.  Fortunately, the command to do this has been written to
help you with this.  It is named `M-x gnews-rename', and is in found
in `gnews/Help.el'.  (It should be autoloadable if you've gotten this
far.)  You do not have to run this immediately: if you don't certain
of your customizations may fail to take effect, nothing worse.  Running
`M-x Gnews' without this will trigger a message similar to this one,
once only.  (The critical variable containing the hook-kill list will
be renamed automatically if it is out-of-date.)

You use `M-x gnews-rename' by quitting out of Gnews, then visiting your
`.emacs' file, your `.gnewsrc.hook' file, and any file where you contain
Emacs Lisp source for the sake of Gnews.  Type `M-x gnews-rename' for
each file, save the result, and in the case of Lisp, byte-compile if
this is needed.  An autoload for 'gnews, if in a fairly standard form,
will be converted to an autoload 'Gnews.

This message will be saved in `~/.gnewsrc.warn'.  Hit any key to continue.")
  (read-char)
  (write-region 1 (point-max) "~/.gnewsrc.warn"))

(defun Gnews (&optional pfx arg)
  "Run the Gnews reader/poster/mailer.  If Gnews is already running,
return to the previous context, otherwise start up a fresh Gnews.\n
The two optional arguments, ARG and PFX, select the .gnewsrc file to
obtain user newsgroup data from on initial start up.  They are inter-
preted as the value and literal prefix respectively of an interactive
prefix argument; no consistency check between the two is made.  Their
meaning is:
 * If PFX is numeric, Gnews will prompt for the first newsgroup.
 * If PFX is non-numeric, Gnews will seek on its own after unread news.
 * If ARG > 1, the .gnewsrc.new file will be used.
 * If ARG < 0, the .gnewsrc.old file will be used.
 * If ARG = 0 or 1, the .gnewsrc file will be used.
Keep in mind that normally .gnewsrc.new and .gnewsrc are identical,
as the former is used for checkpointing the latter.\n
If the .gnewsrc file is to be used, but it is older than the checkpoint
file, then Gnews assumes an abnormal exit occurred the last time, and
offers to start news from the .gnewsrc.new file."
  (interactive "P\np")
  (if (null arg) (setq arg 1))
  (if (interactive-p)		; avoid tripping over internal usages
      (setq gnews-buffer-return (current-buffer)
	    gnews-configuration-return (current-window-configuration)))
  (if (nntp-run-p)
      ;; Return to previous context.
      (let* ((bl (buffer-list))
	     (b (car bl))
	     (bl (cdr bl)))
	(set-buffer b)
	(while (not (memq major-mode gnews-modes))
	  (setq b (car bl))
	  (setq bl (cdr bl))
	  (set-buffer b))
	(switch-to-buffer b)
	(cond ((eq major-mode 'news-mode)
	       (if group-current
		   (group-proffer-must group-current)
		 (news-end)))
	      ((eq major-mode 'group-mode)
	       (if (< article-final article-current)
		   (group-last) (article-recenter)))
	      ((eq major-mode 'article-mode)
	       (article-recenter))))
    ;; the real beginning:
    (run-hooks 'gnews-start-hook)
    (setq nntp-process "gnews"
	  news-buffer-name "*gnews*"
	  reply-buffer-name "*gnews*reply*"
	  index-buffer-name "*gnews*index*"
	  roster-buffer-name "*gnews*Roster*"
	  nntp-buffer-name "*gnews*nntp*"
	  nntp-index-buffer-name "*gnews*nntp*index*"
	  hook-kill-buffer-name "*gnews*hook*"
	  article-cancel-buffer-name "*gnews*cancel*"
	  nntp-buffer (get-buffer-create nntp-buffer-name)
	  nntp-index-buffer (get-buffer-create nntp-index-buffer-name)
	  news-buffer (get-buffer-create news-buffer-name)
	  roster-buffer (get-buffer-create roster-buffer-name)
	  index-buffer (get-buffer-create index-buffer-name)
	  hook-kill-buffer (get-buffer-create hook-kill-buffer-name)
	  gnews-warn-buffer "*gnews*warning*"
	  gnews-rc-file (concat gnews-dot-dir gnews-rc-file-name)
	  gnews-rc-file-old (concat gnews-rc-file ".old")
	  gnews-rc-file-new (concat gnews-rc-file ".new")
	  gnews-hook-file (concat gnews-rc-file ".hook")
	  gnews-time-file (concat gnews-rc-file ".time")
	  gnews-list-file (concat gnews-rc-file ".list")
	  group-previous nil
	  nntp-index-final 0
	  nntp-index-p nil
	  news-new-noted nil
	  news-pattern ""
	  hook-kill-pre-ok t
	  gnews-rc-dangle t		 ; AR: in case user edits .gnewsrc
	  gnews-hook-dangle nil		 ; and then didn't read an article
	  group-mark-later-list (list nil)
	  group-warn-p nil
	  nntp-exec-log nil
	  article-header-all-p nil
	  article-current 0
	  article-junkable nil
	  article-% nil
	  article-field-same-no 1
	  roster-new nil)
    (if (eq news-buffer gnews-buffer-return)
	(setq gnews-buffer-return "*scratch*"))
    (switch-to-buffer news-buffer)
    (gnews-mkdir gnews-news-dir)	; make sure it exists
    (cd gnews-news-dir)
    (make-variable-buffer-local 'gnews-mode-string)
    (make-variable-buffer-local 'gnews-read-p)
    (make-variable-buffer-local 'gnews-hook-p)
    (make-variable-buffer-local 'gnews-edit-p)
    (make-variable-buffer-local 'gnews-rot13-p)
    (make-variable-buffer-local 'cursor-in-echo-area)
    (setq buffer-read-only)
    (erase-buffer)
    (if (not (assq 'reply-was-sent minor-mode-alist))
	(setq minor-mode-alist (cons '(reply-was-sent ":sent")
				     minor-mode-alist)))
    (setq buffer-read-only t)
    (news-mode)
    (if (numberp pfx) (setq news-default-command 'news-first-unread))
    (sit-for 0)
    (gnews-set-mode-line)
    (nntp-start "")
    (nntp-index-start)
    (gnews-rc arg)
    (gnews-hook-load)			; creates *.hook if needed
    (gnews-copyright gnews-copyright-display)
    (if (file-exists-p gnews-rc-file)
	(copy-file gnews-rc-file gnews-rc-file-old t))
    (roster-string-set)
    (roster-new-check)
    (setq group-current (gnaar group-roster)
	  group-set-current ""
	  news-next-message ""
	  cursor-in-echo-area news-grab-cursor
	  article-grep-string nil)
    (fset 'news-restrictions 'news-all)
    (group-set group-current)
    (run-hooks 'gnews-ready-hook)
    (if roster-new-p
	(message "New newsgroups have arrived, use %s to subscribe"
		 (if (eq (key-binding "\M-g") 'news-new) "ESC g"
		   (substitute-command-keys "\\[news-new]")))
      (if (numberp pfx)
	  (gnews-ready pfx)
	(news-first-unread))))
  (gnews-flush))

(defun gnews-rc (arg)
  "Load/create the correct .gnewsrc file.  ARG as in gnews, which see."
  (cond ((< 1 arg) 			; .gnewsrc.new
	 (gnews-load gnews-rc-file-new nil nil t))
	((< arg 0)			; .gnewsrc.old
	 (gnews-load gnews-rc-file-old nil nil t))
	((file-exists-p gnews-rc-file)
	 (if (and (file-exists-p gnews-rc-file-new)
		  (file-newer-than-file-p gnews-rc-file-new gnews-rc-file)
		  (y-or-n-p "Your checkpoint file is newer--use it? "))
	     (gnews-load gnews-rc-file-new nil nil t)
	   (gnews-load gnews-rc-file nil nil t)))
	((file-exists-p (concat gnews-dot-dir ".newsrc"))
	 (gnews-from-news-rc))	; conversion
	(t			; nothing
	 ;; I do not start the default from the active list.
	 ;; With name completion, adding names is not so onerous.
	 (setq group-roster (if (y-or-n-p "Are you a (news) virgin? ")
				'(("news.announce.important" t)
				  ("news.announce.newusers" t))
			      '(("news.announce.important" t)
				("news.announce.newusers" nil))))
	 (group-roster-write gnews-rc-file-new)
	 (setq gnews-rc-dangle t))))

(defun gnews-copyright (display)
  "Display the copyright notice."
  (interactive (list t))
  (message
    (cond ((eq display t)
	   (concat "Copyright (C) 1987, 1988 Matthew P Wiener; "
		   "for license type "
		   (if (eq (key-binding "\M-ll") 'gnews-legalese-license)
		       "ESC l l" (substitute-command-keys
				   "\\[gnews-legalese-license]"))))
	  ((eq display 'yow)
	   (let ((y "\n")) (while (string-match "\n" y) (setq y (yow))) y))
	  ((and (symbolp display) (fboundp display))
	   (funcall display))
	  ((stringp display) display)
	  (t ""))))

(defun gnews-ready (pfx)
  (message "ready %s ? " news-prompt-next))

;;; the major modes [[roster-mode is incomplete]]

(defconst gnews-modes
  '(news-mode group-mode article-mode roster-mode index-mode
	      e-reply-mode n-reply-mode hook-kill-mode gnews-edit-mode)
  "List of Emacs modes used by Gnews.")

(mapcar '(lambda (m) (put m 'mode-class t)) gnews-modes)

(defun news-mode ()
  "News-mode is used by Gnews for selecting newsgroups.\n
Commands are:
\\{news-mode-map}"
  (interactive)
  (if (not (eq (current-buffer) news-buffer)) nil
    (kill-all-local-variables)
    (make-local-variable 'version-control)
    (make-local-variable 'gnews-mode-string)
    (make-local-variable 'gnews-edit-p)
    (make-local-variable 'gnews-rot13-p)
    (make-local-variable 'article-digest-rot13-p)
    (make-local-variable 'gnews-digest-p)
    (make-local-variable 'cursor-in-echo-area)
    (setq major-mode 'news-mode
	  mode-name "News"
	  gnews-mode-string ""
	  gnews-read-p t
	  gnews-hook-p nil
	  gnews-edit-p nil
	  gnews-rot13-p nil
	  gnews-digest-p nil
	  cursor-in-echo-area news-grab-cursor
	  news-default-command 'news-yes
	  group-default-command 'group-next-unread
	  group-prompt-default group-prompt-normal
	  version-control 'never)
    (article-%-clear)
    (use-local-map news-mode-map)
    (run-hooks 'news-hook)
    (gnews-set-mode-line)))

(defun group-mode ()
  "Group-mode is used by Gnews for reading through a newsgroup.\n
Commands are:
\\{group-mode-map}"
  (interactive)
  (if (not (eq (current-buffer) news-buffer)) nil
    (setq major-mode 'group-mode
	  mode-name "Group"
	  gnews-mode-string (concat group-current " "
				    (or article-current "*")
				    "/" article-final)
	  cursor-in-echo-area nil
	  gnews-read-p t
	  gnews-hook-p nil
	  gnews-rot13-p (if (gnews-digest-p) article-digest-rot13-p))
    (use-local-map group-mode-map)
    (run-hooks 'group-hook)
    (gnews-set-mode-line)))

(defun article-mode ()
  "Article-mode is used by Gnews for reading individual articles.
For some reason, it calls itself \"Pager\" mode.\n
Commands are:
\\{article-mode-map}"
  (interactive)
  (if (not (eq (current-buffer) news-buffer)) nil
    (setq major-mode 'article-mode
	  mode-name "Pager"
	  gnews-mode-string (concat group-current " "
				  article-current "/" article-final)
	  cursor-in-echo-area nil
	  gnews-read-p t
	  gnews-hook-p nil)
    (if (gnews-digest-p) (setq gnews-rot13-p article-digest-rot13-p))
    (use-local-map article-mode-map)
    (run-hooks 'article-hook)
    (gnews-set-mode-line)))

;;; the major keymaps

(if news-mode-map nil
  (setq news-mode-map (make-keymap))
  (suppress-keymap news-mode-map t)
  (gnews-key-bind news-mode-map
		  '((" ".news-default)
		    ("y".news-yes)
		    ("=".news-index)
		    ("N".news-next)
		    ("n".news-next-unread)
		    ("P".news-previous)
		    ("p".news-previous-unread)
		    ("-".news-last-seen)
		    ("1".news-first)
		    ("^".news-first-unread)
		    ("$".news-end)
		    ("g".news-goto)
		    ("v".news-visit)
		    ("G".news-general-goto)
		    ("\eg".news-new)
		    ("D".news-delete)
		    ("/".news-pattern-forward)
		    ("?".news-pattern-backward)
		    ("u".news-unsubscribe)
		    ("@".news-at)
		    ("l".news-list)
		    ("L".news-list-rc)
		    ("M".news-move)
		    ("c".news-catchup)
		    ("m".news-mark)
		    ("o".news-only-match)
		    ("a".news-add-match)
		    (".".news-immediate)
		    ("\^a".gnews-abbrev-add)
		    ("\ea".gnews-abbrev-delete)
		    ("\e\^a".gnews-abbrev-list)
		    ("!".shell)
		    ("q".news-quit)
		    ("x".news-quit-restore)
		    ("Q".news-rc-restore)
		    ("R".news-restart)
		    ("\ek".news-hook-edit)
		    ("V".gnews-version)
		    ("h".describe-mode)
		    ("H".gnews-describe-mode)
		    ("C".gnews-customize)
		    ("I".gnews-Info)
		    ("\eh".gnews-features)
		    ("\e\^h".gnews-ances)
		    ("\^w".gnews-bug-report)
		    ("\ell".gnews-legalese-license)
		    ("\elw".gnews-legalese-no-warranty)
		    ("\elc".gnews-legalese-copying))))

(if group-mode-map nil
  (setq group-mode-map (make-keymap))
  (suppress-keymap group-mode-map t)
  (gnews-key-bind group-mode-map
		  '((" ".group-default)
		    ("n".group-next-unread)
		    ("N".group-next)
		    ("\en".group-next-same-subject)
		    ("p".group-previous-unread)
		    ("P".group-previous)
		    ("-".group-last-seen)
		    ("\ep".group-previous-same-subject)
		    ("\er".article-restart-reset)
		    ("<".article-restart)
		    ("v".article-restart-verbose)
		    (".".article-restart-reset)
		    ("\e\^x".article-rot13-restart)
		    ("X".article-rot13)
		    ("l".article-downcase)
		    ("_".article-ununderline)
		    ("\^?".group-back)
		    ("b".group-back)
		    ("B".group-back-half)
		    ("q".group-quit)
		    ("\e\^q".group-quit-emergency)
		    ("^".group-first)
		    ("$".group-last)
		    ("j".group-junk)
		    ("J".group-junk-local)
		    ("m".group-mark)
		    ("M".group-mark-later)
		    ("\em".group-mark-permanent)
		    ("/".group-pattern-forward)
		    ("\e/".group-pattern-forward-new)
		    ("?".group-pattern-backward)
		    ("\e?".group-pattern-backward-new)
		    ("k".group-kill)
		    ("K".group-kill-permanent)
		    ("\ek".group-hook-edit)
		    ("r".group-reply)
		    ("R".group-reply-yank)
		    ("f".group-follow)
		    ("F".group-follow-yank)
		    ("S".group-supersede)
		    ("C".group-cancel)
		    ("c".group-catchup)
		    ("u".group-unsubscribe)
		    ("s".group-save)
		    ("o".gnews-output-to-rmail-file) ; not quite right (?)
		    ("\^o".gnews-output-to-mbox-file)
		    ("|".group-pipe)
		    ("=".group-index)
		    ("t".group-trace)
		    ("T".group-trace-return)
		    ("*".group-reply-return)
		    ("#".article-get)
		    ("@".article-get-msg-id)
		    ("\^s".article-isearch-forward)
		    ("\^r".article-isearch-backward)
		    ("\e\^s".article-isearch-forward-regexp)
		    ("\e\^s".article-isearch-backward-regexp)
		    ("\^l".article-recenter)
		    ("\^w".gnews-bug-report)
		    ("W".article-edit)
		    ("!".shell)
		    ("h".describe-mode)
		    ("H".gnews-describe-mode)))
  (mapcar '(lambda (x)
	     (define-key group-mode-map (concat x) 'group-digit))
	  '(0 1 2 3 4 5 6 7 8 9)))

(if article-mode-map nil
  (setq article-mode-map (copy-keymap group-mode-map))
  (gnews-key-bind article-mode-map
		  '((" ".article-forward)
		    ("d".article-down)
		    ("\n".article-line)
		    ("\r".article-line)
		    ("b".article-back)
		    ("B".article-back-half)
		    (">".article-end)
		    ("g".article-grep)
		    ("G".article-grep-repeat)
		    ("\eg".article-grep-digest)
		    ("\t".article-skip-indent)
		    ("x".article-rot13-forward)
		   ;("q".article-quit)     ; an unpopular rn-ism
		    ("j".article-junk)
		    ("J".article-junk-local))))

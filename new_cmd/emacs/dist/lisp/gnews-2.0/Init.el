;;; Init.el: initialization for Gnews
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

(defconst gnews-version "2.0"
  "Gnews version number as of Mon Oct  3 01:03:32 PDT 1988")

;@@ gnews-code-directory: the defvar goes hunting for likely directories,
;@@ so hopefully you won't ever have to fix this manually.  Because the
;@@ result is quite unseemly looking, I've moved it to the end of this file.

;;; primary site dependent variables

(defvar gnews-spool-machine (or (getenv "NNTPSERVER") "your.nntp.host")
  "*Server to read news off of.")

(defvar gnews-machine (system-name)
  "*User's machine, in domainized form.")

(defvar gnews-organization (or (getenv "ORGANIZATION")
			       "Replace me with your organization")
  "*User's organization") ; <-- DON'T touch that string--it's its docstring

;;; obscure site dependent variables

(defvar nntp-service "nntp"
  "Server name for NNTP.  Either \"nntp\" or 119 or nil.\n
Both the string and the number refer to NNTP.  nil means use spool code.")

(defvar path-host "your.path.host"
  "Site where UUCP path info is kept")

(defvar path-service "uucp-path"
  "Server name for getting paths")

(defvar gnews-bug-address "weemba@garnet.berkeley.edu (Matthew P Wiener)"
  "Address for mailing Gnews bugs.")

;;; user dependent variables

(defvar gnews-home-dir (file-name-as-directory
			 (or (getenv "LOGDIR") (getenv "HOME") "~"))
  "*User's home directory")
(defvar gnews-dot-dir (file-name-as-directory
			(or (getenv "DOTDIR") gnews-home-dir))
  "*User's Gnews bookkeeping files' directory")
(defvar gnews-rc-file-name ".gnewsrc"
  "*User's newsgroup roster file name")
(defvar gnews-news-dir (file-name-as-directory
			 (or (getenv "NEWSDIR") "~/News/"))
  "*Directory to keep user's news save files")
(defvar gnews-user (or (getenv "LOGNAME") (getenv "USER")
		       (user-real-login-name))
  "*User's login name")
(defvar gnews-name (or (getenv "NAME") (user-full-name))
  "*User's name, as gleaned, by default, from the /etc/passwd file")
(defvar gnews-copyright-display t		;; Don't change it here.
  "*Display copyright notice if non-nil")	;; Just set it yourself.

;;; standard options

(defvar article-grep-height 4
  "*Line on screen at which to display article-grep matches")
(defvar article-digest-separator "^--------------------"
  "*Regexp that identifies the separator between digests")
(defvar article-formfeed "^\f$"
  "*Regexp that identifies \"formfeeds\".  If non-nil, Gnews stops at
formfeeds when scrolling through an article.")
(defvar article-formfeed-top nil
  "*If non-nil, it is a number to indicate which line to redraw the formfeed
at after scrolling past a formfeed.  If nil, scrolling past a formfeed will
merely fill the screen with more of the article.")
(defvar article-formfeed-post nil
  "*If non-nil, a regexp used to distinguish true formfeeds from false
formfeeds.  True formfeeds are those that are followed by a string that
matches the regexp.")

(defvar reply-prefix ">"
  "*String to indent replies with")
(defvar reply-forward-prefix ">"
  "*String to indent forwarded articles with")

(defvar gnews-flush-typeahead t
  "*If non-nil, typeahead is flushed")
(defvar article-display-count nil
  "*The number of lines of the article to get and display initially.
If nil, a entire screen is displayed first.")
(defvar article-big 200
  "*The number of lines which indicate an article is considered big, for
which the article-display-count is temporarily treated as nil.\n
Thus, the normal wait for the rest of the article to be read in can
be filled with your reading.")

(defvar article-get-really-line 0
  "*An explicit numeric argument of this size or less is treated as an
argument for article-line, instead of for article-get.  In other words,
scrolling up to that many lines is supported with explicit digits.")

(defvar article-auto-junk-locally nil
  "*Non-nil means article hook-kills are done by local junks.")
(defvar article-junk-terse nil
  "*Non-nil means junk articles without echoing subject.")
(defvar article-junk-unsub t
  "*Non-nil means junking of cross-posted articles will include all the
unsubscribed-to newsgroups.  Setting this to nil cuts down on garbage
collection.")
(defvar group-kill-auto t
  "*Non-nil means kill the current subject if a group-kill-* command is
invoked.  Nil means query first.")

(defvar group-name-confirm t
  "*Non-nil means confirm newsgroup names on completion-and-exit commands.")

(defvar news-grab-cursor t
  "*Non-nil means keep the cursor in the minibuffer when in news-mode.")

(defvar hook-kill-pop t
  "*Non-nil means pop into the hook-kill buffer.")

(defvar article-header-ignore
  '(Path Xref Approved Posted Date-Received Nf-ID Nf-From Article-I\.D\.
	 Posting-Version Relay-Version ReSent-Date ReSent-From ReSent-To
	 Return-path Posting-Front-End To TO Newsgroup Checksum X-Edited 
	 Reply-Path Received UUCP-Path Expires X-Andrew-Authenticated-as
	 Disclaimer DISCLAIMER XPortal-User-Id Archive-Name Submitted-By
	 X-Mailer X-Archive Sender Cc Supersedes In-Reply-To In-reply-to
	 Comments-to Submissions-to X-mailer X-Trace or_perhaps_Reply_to
	 Return-Path Advert X-Unparsable-Date)
  "*List of unsightly header fields--some of which are truly bozonic.\n
To see a header that's been ignored, set the variable article-header-show
to a list just like article-header-ignore, only shorter.")
(defvar article-header-show nil
  "*List of headers to show, even if article-header-ignore says not to.")
(defvar article-header-all-p nil
  "*If non-nil, show all headers.")

(defvar article-same-subject-trigger nil
  "*nil means do not enter subject-search mode except by request, and an
integer value means use subject search mode by default whenever there are
at least that many unread articles in a group.")

(defun news-all (g)
  "Given newsgroup G, returns t if G satisfies the current restrictions.\n
All groups return t by default.  This behavior may be set with \
\\[news-only-match]."
  t)

(fset 'news-restrictions 'news-all)

(defvar gnews-slashify nil
  "*Control whether to slashify or dotify newsgroup names.  See
gnews-save-name for the exact details.")
(defvar gnews-save-style nil
  "*If non-nil, the default save file is that of the previous save within
this newsgroup; saving always appends.\n
If nil, the default save file is computed by the function gnews-save-name,
which see.\n
To get more fancy saving, set gnews-save-style to a command symbol.  The
supplied examples are gnews-output-to-rmail-file, which produces Rmail style
saving, and gnews-output-to-mbox-file, which produces ordinary Unix mail
style saving.")

(defun gnews-save-name (ng art)
  "Internal routine for generating default save file names.  It is invoked
with the newsgroup string NG and current article number ART.  The exact
name is controlled by the variable gnews-slashify (with default nil).\n
If it is t, this function returns something like comp/sys/eniac/345.\n
If it is nil, this returns something like comp.sys.eniac.345.\n
Otherwise, this returns something like comp.sys.eniac/345."
  (cond ((eq gnews-slashify t)
	 (concat (gnews-replace "\\." "/" ng) "/" art))
	((eq gnews-slashify nil)
	 (concat ng "." art))
	(t
	 (concat ng "/" art))))

(defvar group-save-junk nil
  "Junk articles upon saving if non-nil.")
(defvar group-pipe-junk nil
  "Junk articles upon piping if non-nil.")

(defvar gnews-minor-mode-alist
  '((gnews-digest-p " digest")
    (gnews-edit-p " edited")
    (gnews-rot13-p " rot13"))
  "*Alist of minor modes to apply when in Gnews.")

;;; possibly interesting internal Gnews variables

(defvar group-roster nil
  "The contents of the user's .gnewsrc file.")
(defvar roster nil
  "List of all newsgroups available at this site.")
(defvar roster-new nil
  "List of time new newsgroups were last checked and of the new newsgroups.")

(defvar group-current nil
  "Current newsgroup.")
(defvar article-current 0
  "Current article number.")
(defvar article-first 0
  "First available article within the current newsgroup.")
(defvar article-final 0
  "Last available article within the current newsgroup.")
(defvar article-count 0
  "Count of unread articles upon newsgroup entry.")
(defvar amark nil
  "List of currently marked articles.")
(defvar amark-entry nil
  "List of articles as marked upon entry.")

(defvar article-previous 0
  "Article number of the previous article seen.")
(defvar article-trace 0
  "Article number where the current reference trace back began.")
(defvar article-message-id "<>"
  "Message-ID of article last set to via message-ID.")
(defvar article-replied 0
  "Article number of article being replied to.")
(defvar group-replied 0
  "Newsgroup of article being replied to.")

(defvar article-junkable nil
  "Non-nil if the current article is to be junked upon leaving.")

(defvar group-bogus nil
  "Non-nil if the current newsgroup is bogus.")

(defvar news-pattern ""
  "Most recent pattern match in news-mode.")
(defvar article-grep-string ""
  "Most recent grep string in an article-grep-* command.")
(defvar group-pattern-field "Subject"
  "Header to look for pattern matches in group-pattern-* commands.")
(defvar group-pattern ""
  "Regexp for pattern matches in group-pattern-* commands.")
(defvar group-pattern-command nil
  "[[Command to execute upon finding a match in group-pattern-* commands.]]")

(defvar group-default-command nil
  "Command to execute on a \\<group-mode-map>\\[group-default].")
(defvar news-default-command nil
  "Command to execute on a \\<news-mode-map>\\[news-default].")

(defvar news-seen nil
  "Non-nil if news was offered along the way to the end but not read.")
(defvar news-new-noted nil
  "Non-nil if user tried to add the new newsgroups.")
(defvar hook-kill-alist nil
  "*Alist of the form ((GROUP.REGEXP)...).  By default, hook-kills for
GROUP will be for REGEXP instead.")

(defvar group-last-save ""
  "*Last save file done in this newsgroup--initially the correct directory.")
(defvar group-read-mood 0
  "First character typed upon getting a newsgroup name.")
(defvar group-checkpoint nil
  "Non-nil when a checkpointing opportunity has arisen.")
(defvar article-subject-hilite nil
  "*Non-nil means highlight the subject of the current article.")

(defvar gnews-buffer-clean t
  "*Non-nil means kill, as oppose to bury, Gnews' work buffers upon
quitting.")

(defvar gnews-buffer-return nil
  "Buffer to return to upon exiting Gnews.")

(defvar hook-kill-per nil
  "The list of current per-hooks.")
(defvar hook-kill-post nil
  "The list of current post-hooks.")
(defvar hook-kill-pre nil
  "The list of current pre-hooks.")

(defvar hook-kill-pre-ok nil
  "Non-nil if post-hooks have been processed.")

(defvar gnews-mode-string nil
  "Mode string to use in most Gnews buffers.")
(defvar gnews-read-p nil
  "Non-nil if looking at an article.")
(defvar gnews-hook-p nil
  "Non-nil if in the hook-kill buffer.")
(defvar gnews-edit-p nil
  "Non-nil if the current article has been edited.")
(defvar gnews-rot13-p nil
  "Non-nil if the current article has been rot13ed.")

(defvar gnews-rc-dangle nil
  "Non-nil if .gnewsrc needs to be updated.")
(defvar gnews-hook-dangle nil
  "Non-nil if .gnewsrc.hook should be updated.")

(defvar gnews-rc-file nil
  "The user's .gnewsrc file.")
(defvar gnews-rc-file-new nil
  "The user's .gnewsrc.new file.")
(defvar gnews-rc-file-old nil
  "The user's .gnewsrc.old file.")
(defvar hook-kill-file nil
  "The user's .gnewsrc.hook file.")
(defvar gnews-time-file nil
  "The user's .gnewsrc.time file")

(defvar roster-string nil
  "The list of active newsgroups in active-file format.")

(defvar news-prompt-yes "[ynp]"
  "Prompt when reaching a newsgroup with unread news.")
(defvar news-prompt-next "[npq]"
  "Prompt when reaching a newsgroup with no unread news.")
(defvar news-prompt-quit "[qnp]"
  "Prompt when it looks like there's no more unread news.")
(defvar news-prompt-first "[npq]"
  "Prompt when the end of the newsgroup list has been reached.")
(defvar news-prompt-return news-prompt-yes
  "Prompt when returning to Gnews from elsewhere.")

(defvar group-prompt-normal "[npq]"
  "Prompt when subject-searching is turned off.")
(defvar group-prompt-same-subject "[~Nnpq]"
  "Prompt when subject-searching is turned on.")
(defvar group-prompt-default group-prompt-normal
  "Prompt when at the end of an article.")
(defvar group-prompt-pf "[/?npq]"
  "Prompt when pattern searching backwards.")
(defvar group-prompt-pb "[?/npq]"
  "Prompt when pattern searching backwards.")
(defvar group-query-pf "/-search "
  "Prompt for \\<group-mode-map>\\[group-pattern-forward].")
(defvar group-query-pb "?-search "
  "Prompt for \\<group-mode-map>\\[group-pattern-backward].")

;;; autoloads

(defvar gnews-code-directory (let ((d (expand-file-name "~"))
				   (p load-path)
				   (g "/gnews/")
				   (v (concat "/gnews-" gnews-version "/"))
				   gcd)
			       (setq gcd nil)
			       (while d
				 (if (string-match "/gnews[^/]*/?$" d)
				     (setq gcd d d)
				   (setq gcd (concat d g))
				   (if (file-exists-p gcd) (setq d)
				     (setq gcd (concat d v))
				     (if (file-exists-p gcd) (setq d)
				       (setq d (car p) p (cdr p))))))
			       (file-name-as-directory (or d gcd))) ; JR
  ;; This monstrosity merely looks for .../gnews/ or .../gnews-#.#/, a
  ;; directory either in your load-path, or else one down.  I hope this
  ;; is general enough for most people's conventions.  Let me know.
  "Directory where Gnews Lisp code is kept.")

(defun gnews-file (file) (concat gnews-code-directory file))

(autoload 'gnews-describe-mode (gnews-file "Help")
  "Describe the current mode in detail." t)
(autoload 'gnews-Info (gnews-file "Help")
  "Go to the Gnews on-line Info manual." t)
(autoload 'gnews-features (gnews-file "Help")
  "Describe the major features of Gnews compared with rrn." t)
(autoload 'gnews-ances (gnews-file "Help")
  "Summarize the known Gnews bugs and other nuisances." t)
(autoload 'gnews-bugs (gnews-file "Help")
  "Summarize the known Gnews bugs and other nuisances." t)
(autoload 'gnews-weemba (gnews-file "Help")
  "For your perusal, edification and/or amusement, a buffer is filled
with the author's own customizations, give or take a Rich L Rosen." t)
(autoload 'gnews-texinfo-info (gnews-file "Help")
  "Convert the current buffer into the Gnews Info file." t)
(autoload 'gnews-texinfo-tex (gnews-file "Help")
  "Convert the current buffer into the Gnews TeX manual." t)
(autoload 'gnews-functions (gnews-file "Help")
  "Insert into the current buffer the names of all Gnews functions
currently bound." t)
(autoload 'gnews-variable-values (gnews-file "Help")
  "Insert into the current buffer the names and values of the
Gnews variables mentioned in gnews-variable-list.  If the latter
is unbound, then all Gnews variables will be listed." t)
(autoload 'gnews-bug-report (gnews-file "Help")
  "Report a Gnews bug.\n
Prompts for bug subject.  Leaves you in a mail buffer." t)
(autoload 'gnews-legalese-license (gnews-file "Help")
  "Display the Gnews General License." t)
(autoload 'gnews-legalese-copying (gnews-file "Help")
  "Display the copying permissions for Gnews." t)
(autoload 'gnews-legalese-no-warranty (gnews-file "Help")
  "Describe Gnews' lack of warranty." t)
(autoload 'gnews-rename (gnews-file "Help")
  "Transform names in the current buffer from pre-2.0 to 2.0." t)

;;; Load the essentials

;;; This is the only defmacro not in prims.el.  It is used nowhere else.

(defmacro gnews-code-load (file)
  (` (load (gnews-file (, file)) nil t)))

(gnews-code-load "prims")
(gnews-code-load "NNTP")
(gnews-code-load "utils")
(gnews-code-load "entry")
(gnews-code-load "news")
(gnews-code-load "group")
(gnews-code-load "art")
(gnews-code-load "hook")
(gnews-code-load "digest")
(gnews-code-load "Roster")
(gnews-code-load "index")
(gnews-code-load "mail")
(gnews-code-load "reply")

;;; spool code

(if nntp-service nil (gnews-code-load "Spool"))

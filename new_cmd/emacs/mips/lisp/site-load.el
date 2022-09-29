(setq news-inews-program "/usr/new/lib/news/inews")

(setq auto-mode-alist (cons (cons "\\.p$" 'pascal-mode) auto-mode-alist))

(autoload 'pascal-mode "pascal.elc"
  "Mode to support program development in Pascal.
The prefix-key for pascal-mode is C-c.

  TAB      pascal-indent-line       C-c TAB     pascal-tab-to-tab-col
  C-j      pascal-newline           C-c b       pascal-begin
  C-c C-f  pascal-forward-block     C-c C-b     pascal-backward-block
  C-c C-d  pascal-down-block        C-c C-u     pascal-back-pascal-up-block
  C-c C-e  pascal-up-block          C-c C-@     pascal-mark-block
  C-c C-n  pascal-narrow-to-block   C-c ~       pascal-self-assign-stmt
  C-c C-[  pascal-open-comment-box  C-c C-m     pascal-continue-comment-box
  C-c }    pascal-end-comment       C-c >       pascal-set-end-comment-column

  pascal-indent controls the number of spaces for each indentation."
  t)

(autoload 'background "background.elc"
  "Run COMMAND in the background like csh.  A message is displayed when
the job starts and finishes.  The buffer is in shell mode, so among
other things you can control the job and send input to it.  The
process object is returned if anyone cares.  See also shell-mode and
the variables background-show and background-select."
  t)

(autoload 'dbx "mipsdbx.elc"
  "Run dbx on program FILE in buffer *dbx-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for DBX.  If you wish to change this, use
the DBX commands `cd DIR' and `directory'."
  t)

(autoload 'gnews "/usr/new/lib/emacs/lisp/gnews-2.0/Init.elc"
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
offers to start news from the .gnewsrc.new file.\n
\\<news-mode-map>\
 @ What is Gnews?\n
Gnews is an NNTP-based rn-styled newsreader, in other words, an Emacs \"rrn\".
At the top level, run \\[gnews-features] for a list of the major differences \
with rrn,
and \\[gnews-ances] for a partial bug list.\n
At each level--newsgroup selection, article selection, and paging--run \
\\[describe-mode]
to get a list of the available Gnews commands.  Gnews provides separate
Emacs modes for the rn levels, and furthermore specialized index, reply,
and hook (aka hook-kill) modes.\n
Within most of the modes, \\[gnews-describe-mode] provides a more \
complete description of the
available commands.\n
The behavior of Gnews can be customized extensively.  First, there are
oodles of user variables and switches--for now scan the top of gnews.el
to see the major ones.  And second, there are hooks galore:\n
 * gnews-start-hook is run immediately upon starting gnews, which is where
   the basic non-default variables, like gnews-dot-dir, should be set.  See
   the top of gnews.el for the complete list of these variables.  Do not set
   any local variables here.
 * gnews-ready-hook is run after loading the user's .gnewsrc file, but before
   looking for unread news.
 * gnews-quit-hook is run after writing the final .gnewsrc out.  (Thus, in
   tandem with gnews-ready-hook, the user can simulate an rn -r flag.)
 * <MODE>-hook is run upon entering <MODE>-mode, where <MODE> is one of
   article,group,news,index,roster,e-reply,n-reply,gnews-hook,gnews-edit.
 * article-header-hook is run after identifying headers but before displaying
   them.  It allows you to set up rather non-standard header displays, and
   it serves as an article-start hook.
 * group-reply-start-hook is run immediately upon starting a mail reply. This
   is where internal definitions involved in mail set up can be changed.
 * group-reply-hook is run just after setting up the default mail reply.
 * e-reply-send-hook is run just before sending a mail reply.
 * group-follow-start-hook, group-follow-hook, and n-reply-send-hook are
   the analogous hooks for a net reply.
 * article-cancel-hook is run to modify the default cancel message.
 * article-supersede-hook is run to modify the default supersession.\n
Moreover, there are hooks that can be run for individual newsgroups and
articles, conditionally activated by the detection of user-specified
regexps.  This is the Gnews analogue of rn KILL files.\n
If all this rn-style control isn't enough, you can rewrite code as needed.
Certain pieces of code have been isolated into functions just to make this
convenient:
 * reply-wide: dealing with > 80 column windows on posting
 * gnews-ready: Gnews' action upon startup with a numeric prefix
 * gnews-save-name: generate a save file name
 * index-sort-function: sort the index-buffer
 * article-header-tweak: fine tweak headers
 * news-next-unread-maybe: action after an implicit newsgroup quit
 * gnews-hook-junk-message: message to generate on automatic junking
Finally, set gnews-copyright-display to nil if you don't like it."
  t)

(load-library "mail-utils")
(load-library "mailalias")
(load-library "sendmail")
(load-library "rmail")
(load-library "rmailsum")

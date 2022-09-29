#
# |-----------------------------------------------------------|
# | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
# | All Rights Reserved                                       |
# |-----------------------------------------------------------|
# |          Restricted Rights Legend                         |
# | Use, duplication, or disclosure by the Government is      |
# | subject to restrictions as set forth in                   |
# | subparagraph (c)(1)(ii) of the Rights in Technical        |
# | Data and Computer Software Clause of DFARS 52.227-7013.   |
# |         MIPS Computer Systems, Inc.                       |
# |         950 DeGuigne Drive                                |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
#
# $Header: Makefile,v 1.31.1.9.1.1.1.5 90/11/02 17:48:29 beacker Exp $
#
SHELL	=/bin/sh

SUBDIRS	=acct awk badspots banner basename bc bdiff bfs cal calendar cat \
	cb cflow chgrp \
	chmod chown chroot ckbupscd cmp col comm cpio cpset crash cron crypt \
	csplit ctags ctrace cut cxref date dc dd decnet deroff device_error \
	devinfo df dict diff diff3 dircmp dirname dmodes du dvhtool echo \
	ed edquota egrep env expr \
	extcentry factor false fgrep file find flfo fold format fs.ffs \
	fs.gen fuser getopt getty grep grpck help hwconf id init initpkg \
	install.prog ipc join kill \
	killall lboot lex line link lint-libs logger login logname \
	lorder lp ls m4 mail \
	mailx make makekey mesg mipsinstall mkboot mkboottape mkdepend \
	mkdir mkf2c mklost+found mknod mkstr mt mv mvdir net new \
	newform newgrp news \
	nice nl nohup od pack passwd \
	passmgmt pwconv pwunconv paste pg pr profiler prtvtoc ps pwck \
	pwd quot quota quotacheck quotaon ranlib regcmp repquota \
	rm rmdir sa sadmin savecore sccs sdiff sed setpgrp settz sh \
	sleep sort spell split stty su sum sun swap sync \
	sysconinit tail tar tic \
	tee time touch tr true tsort tty uadmin ucb uname uniq units unlink \
	unpack uucp vi wall wc who whodo write xargs xstr yacc \
	rls_id uptime chrtbl MKDEV periodic devstr sendmail sendmail.nis \
	stamp_links test winsize zic zdump vdisk

all install clobber clean generic:
	@for dir in $(SUBDIRS) ;\
	{\
		if [ -d "$$dir" ] ;\
		then \
			echo "XXXXXXX $$dir XXXXXXX" ;\
			( cd $$dir ; $(MAKE) $(MAKEARGS) $@ ) ;\
		else \
			echo "******* No directory $$dir" ;\
		fi ;\
	}

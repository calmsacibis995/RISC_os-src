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
# |         928 Arques Avenue                                 |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
#
# $Header: Makefile,v 1.11.1.5.1.3 90/07/13 16:37:23 hawkes Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

WHEREIGO=$(DESTROOT)/usr/include

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

HDRDIRS= \
	bsd \
	bsd/arpa \
	bsd/protocols \
	bsd/sys \
	cmplrs \
	make \
	prom \
	rpc \
	rpcsvc \
	saio \
	sun \
	sun/sys

HDRFILES= \
	asm.h \
	assert.h \
	bsd/arpa/ftp.h \
	bsd/arpa/inet.h \
	bsd/arpa/nameser.h \
	bsd/arpa/resolv.h \
	bsd/arpa/telnet.h \
	bsd/arpa/tftp.h \
	bsd/dbm.h \
	bsd/ndbm.h \
	bsd/netdb.h \
	bsd/protocols/dumprestore.h \
	bsd/protocols/routed.h \
	bsd/protocols/rwhod.h \
	bsd/protocols/talkd.h \
	bsd/protocols/timed.h \
	bsd/resolv.h \
	bsd/sgtty.h \
	bsd/strings.h \
	bsd/sysexits.h \
	bsd/syslog.h \
	core.h \
	ctype.h \
	dial.h \
	dirent.h \
	dumprestor.h \
	errno.h \
	elf_abi.h \
	elf_mips.h \
	execargs.h \
	exportent.h \
	fatal.h \
	fcntl.h \
	fp_class.h \
	fpi.h \
	ftw.h \
	grp.h \
	machine_info.h \
	macros.h \
	make/c-depend.awk \
	make/commondefs \
	make/template \
	malloc.h \
	math.h \
	memory.h \
	mnttab.h \
	mon.h \
	nsaddr.h \
	nserve.h \
	pn.h \
	poll.h \
	prof.h \
	pwd.h \
	regdef.h \
	regexp.h \
	search.h \
	setjmp.h \
	sgtty.h \
	shadow.h \
	signal.h \
	stdio.h \
	string.h \
	stropts.h \
	strselect.h \
	sun/fstab.h \
	sun/mntent.h \
	sun/pwd.h \
	sun/sys/dk.h \
	symbol.h \
	sys.s \
	tar.h \
	termio.h \
	time.h \
	tiuser.h \
	tzfile.h \
	unistd.h \
	ustat.h \
	utmp.h \
	varargs.h \
	vers.c

all clean clobber:
	@echo "Nothing to do"

install:
	ID=`id +%U` ; \
	for dir in usr usr/net ; \
	{\
		[ -r $(DESTROOT)/$$dir ] || \
			( ( mkdir -p $(DESTROOT)/$$dir || exit 0) ; \
			  [ $$ID != 0 ] || \
			  ( chgrp $(GROUP) $(DESTROOT)/$$dir ; \
			    chmod 0775 $(DESTROOT)/$$dir ; \
			    chown $(OWNER) $(DESTROOT)/$$dir ) ) ; \
	} ; \
	[ -r $(WHEREIGO) ] || \
		( ( mkdir -p $(WHEREIGO) || exit 0) ; \
		  [ $$ID != 0 ] || \
		  	( chgrp $(GROUP) $(WHEREIGO) ; \
			  chmod 0775 $(WHEREIGO) ; \
			  chown $(OWNER) $(WHEREIGO) ) ) ; \
	for dir in $(HDRDIRS) ; \
	{\
		[ -d $(WHEREIGO)/$$dir ] || \
		    ( mkdir -p $(WHEREIGO)/$$dir ; \
		      [ $$ID != 0 ] || \
		      ( chmod 0775 $(WHEREIGO)/$$dir ; \
			chgrp $(GROUP) $(WHEREIGO)/$$dir ; \
			chown $(OWNER) $(WHEREIGO)/$$dir ) ) ; \
	} ; \
	for file in $(HDRFILES) ; \
	{\
		DIRN=`dirname $$file` ; \
		FILEN=`basename $$file` ; \
		$(INSTALL) $(INSTOPTS) $$file $(WHEREIGO)/$$DIRN ; \
	} ; \
	[ -r $(DESTROOT)/sysv ] || ln -s . $(DESTROOT)/sysv ; 
	[ -r $(WHEREIGO)/sysv ] || \
		ln -s . $(WHEREIGO)/sysv ;
	rm -rf $(WHEREIGO)/netdb.h
	ln -s bsd/netdb.h $(WHEREIGO)/netdb.h

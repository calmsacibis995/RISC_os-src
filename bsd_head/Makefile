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
# $Header: Makefile,v 1.17.1.10 90/05/07 20:07:14 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

WHEREIGO=$(DESTROOT)/usr/include/bsd43

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

HDRDIRS= arpa protocols rpc rpcsvc des \
	net netinet netns \
	mips mipsif mipsvme netimp nfs prom saio sys ufs
ARPAHDRFILES= \
	ftp.h \
	inet.h \
	nameser.h \
	resolv.h \
	telnet.h \
	tftp.h
PROTOCOLSHDRFILES= \
	routed.h \
	rwhod.h \
	timed.h \
	dumprestore.h \
	talkd.h
SYSHDRFILES= \
	syslog.h \
	sysmips.h \
	syscall.h \
	signal.h \
	errno.h
MIPSHDRFILES= \
	regdef.h \
	asm.h
BSDHDRFILES= \
	netdb.h \
	resolv.h \
	dbm.h \
	ndbm.h
SYSVHDRFILES= \
	exportent.h \
	symbol.h \
	tzfile.h \
	memory.h \
	assert.h \
	fp_class.h \
	fpi.h \
	math.h \
	tar.h \
	varargs.h \
	shadow.h
SUNHDRFILES= \
	mntent.h 
HDRFILES= \
	utmp.h \
	bsd43_.h \
	ctype.h \
	disktab.h \
	fstab.h \
	grp.h \
	lastlog.h \
	mp.h \
	mtab.h \
	pwd.h \
	setjmp.h \
	sgtty.h \
	stdio.h \
	string.h \
	strings.h \
	struct.h \
	sysexits.h \
	time.h \
	ttyent.h \
	vfont.h 

all clean clobber:
	@echo "Nothing to do"

install-headers:	install

install:
	ID=`id +%U` ; \
	for dir in bin usr usr/bsd43 usr/bsd43/bin usr/bsd43/usr/lib \
		   usr/bsd43/usr/lib/cmplrs \
		   bsd43 bsd43/usr usr/include ; \
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
		$(INSTALL) $(INSTOPTS) $$file $(WHEREIGO) ; \
	} ; \
	[ -r $(DESTROOT)/sysv ] || ln -s . $(DESTROOT)/sysv ; 
	[ -r $(DESTROOT)/usr/sysv ] || \
		ln -s ../sysv $(DESTROOT)/usr/sysv ; \
	[ -r $(DESTROOT)/bsd43/sysv ] || ln -s .. $(DESTROOT)/bsd43/sysv ; \
	[ -r $(DESTROOT)/bsd43/bsd43 ] || \
		ln -s . $(DESTROOT)/bsd43/bsd43 ; \
	[ -r $(DESTROOT)/bsd43/usr/include ] || \
		ln -s ../sysv/usr/include/bsd43 \
			$(DESTROOT)/bsd43/usr/include ; \
	[ -r $(DESTROOT)/bsd43/bin ] || \
		ln -s ../usr/bsd43/bin $(DESTROOT)/bsd43/bin ; \
	[ -r $(DESTROOT)/bsd43/usr/lib ] || \
		ln -s ../../usr/bsd43/usr/lib $(DESTROOT)/bsd43/usr/lib ; \
	[ -r $(WHEREIGO)/sysv ] || \
		ln -s .. $(WHEREIGO)/sysv ;
	[ -r $(DESTROOT)/usr/include/sysv ] || \
		ln -s . $(DESTROOT)/usr/include/sysv ; \
	[ -r $(WHEREIGO)/bsd43 ] || \
		ln -s . $(WHEREIGO)/bsd43 ; \
	rm -rf $(WHEREIGO)/fcntl.h ; \
	ln -s sys/file.h $(WHEREIGO)/fcntl.h ; 
	rm -rf $(WHEREIGO)/sys/varargs.h ; \
	ln -s ../varargs.h $(WHEREIGO)/sys/varargs.h ;
	for file in $(SYSHDRFILES) ; \
	{\
		rm -rf $(WHEREIGO)/$$file ; \
		ln -s sys/$$file $(WHEREIGO)/$$file ; \
	} ; \
	for file in $(MIPSHDRFILES) ; \
	{\
		rm -rf $(WHEREIGO)/$$file ; \
		ln -s mips/$$file $(WHEREIGO)/$$file ; \
	} ; \
	for file in $(BSDHDRFILES) ; \
	{\
		rm -rf $(WHEREIGO)/$$file ; \
		ln -s sysv/bsd/$$file $(WHEREIGO)/$$file ; \
	} ; \
	for file in $(SUNHDRFILES) ; \
	{\
		rm -rf $(WHEREIGO)/$$file ; \
		ln -s sysv/sun/$$file $(WHEREIGO)/$$file ; \
	} ; \
	for file in $(SYSVHDRFILES) ; \
	{\
		rm -rf $(WHEREIGO)/$$file ; \
		ln -s sysv/$$file $(WHEREIGO)/$$file ; \
	} ; \
	for file in $(ARPAHDRFILES) ; \
	{\
		rm -rf $(WHEREIGO)/arpa/$$file ; \
		ln -s ../sysv/bsd/arpa/$$file $(WHEREIGO)/arpa/$$file ; \
	} ; \
	for file in $(PROTOCOLSHDRFILES) ; \
	{\
		rm -rf $(WHEREIGO)/protocols/$$file ; \
		ln -s ../sysv/bsd/protocols/$$file $(WHEREIGO)/protocols/$$file ; \
	} ; 


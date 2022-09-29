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
# $Header: Makefile,v 1.7.1.6.1.1.1.2 90/10/05 10:11:45 beacker Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

WHEREIGO=$(DESTROOT)/usr/include/posix

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

HDRDIRS	= sys

HDRFILES= \
	asm.h \
	assert.h \
	ctype.h \
	dirent.h \
	errno.h \
	fcntl.h \
	fp_class.h \
	grp.h \
	limits.h \
	locale.h \
	math.h \
	nan.h \
	pwd.h \
	regdef.h \
	setjmp.h \
	signal.h \
	stamp.h \
	stddef.h \
	stdio.h \
	stdlib.h \
	string.h \
	sys.s \
	tar.h \
	termios.h \
	time.h \
	unistd.h \
	utime.h \
	varargs.h \
	values.h

all clean clobber:
	@echo "Nothing to do"

install:
	ID=`id +%U` ; \
	for dir in usr usr/posix usr/posix/bin usr/posix/usr/lib \
		   usr/posix/usr/lib/locale posix posix/usr posix/lib \
		   usr/include ; \
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
	echo "XXXXXXX setup /posix enviroment XXXXXXX" ; \
	[ -r $(DESTROOT)/sysv ] || ln -s . $(DESTROOT)/sysv ; \
	[ -r $(DESTROOT)/posix/bin ] || \
		ln -s ../usr/posix/bin $(DESTROOT)/posix/bin ; \
	[ -r $(DESTROOT)/usr/sysv ] || \
		ln -s ../sysv $(DESTROOT)/usr/sysv ; \
	[ -r $(DESTROOT)/posix/sysv ] || ln -s .. $(DESTROOT)/posix/sysv ; \
	[ -r $(DESTROOT)/posix/posix ] || \
		ln -s . $(DESTROOT)/posix/posix ; \
	[ -r $(DESTROOT)/posix/lib/locale ] || \
		ln -s ../../usr/posix/usr/lib/locale \
			$(DESTROOT)/posix/lib/locale; \
	[ -r $(DESTROOT)/posix/usr/include ] || \
		ln -s ../sysv/usr/include/posix \
			$(DESTROOT)/posix/usr/include ; \
	[ -r $(DESTROOT)/posix/usr/lib ] || \
		ln -s ../../usr/posix/usr/lib $(DESTROOT)/posix/usr/lib ; \
	[ -r $(DESTROOT)/posix/usr/lib/cmplrs ] || \
		ln -s ../../sysv/usr/lib/cmplrs \
			$(DESTROOT)/posix/usr/lib/cmplrs ; \
	[ -r $(WHEREIGO)/sysv ] || ln -s .. $(WHEREIGO)/sysv ; \
	[ -r $(DESTROOT)/usr/include/sysv ] || \
		ln -s . $(DESTROOT)/usr/include/sysv ; \
	[ -r $(WHEREIGO)/posix ] ||  ln -s . $(WHEREIGO)/posix ; \
	echo "XXXXXXX crosslink with other enviroments XXXXXXX" ; \
	[ -r $(DESTROOT)/usr/posix/sysv ] || \
		ln -sf ../../sysv $(DESTROOT)/usr/posix/sysv ; \
	[ -r $(DESTROOT)/usr/posix/bsd43 ] || \
		ln -sf ../../bsd43 $(DESTROOT)/usr/posix/bsd43 ; \
	[ -r $(DESTROOT)/usr/include/posix/sysv ] || \
		ln -sf .. $(DESTROOT)/usr/include/posix/sysv ; \
	[ -r $(DESTROOT)/usr/include/posix/bsd43 ] || \
		ln -sf ../bsd43 $(DESTROOT)/usr/include/posix/bsd43 ; \
	[ -r $(DESTROOT)/usr/bsd43/posix ] || \
		ln -sf ../../posix $(DESTROOT)/usr/bsd43/posix ; \
	[ -r $(DESTROOT)/usr/include/bsd43/sysv ] || \
		ln -sf .. $(DESTROOT)/usr/include/bsd43/sysv ; \
	[ -r $(DESTROOT)/usr/include/bsd43/posix ] || \
		ln -sf ../posix $(DESTROOT)/usr/include/bsd43/posix 

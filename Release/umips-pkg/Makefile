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
# $Header: Makefile,v 1.3.1.2.1.2.1.2 90/10/17 17:58:06 beacker Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

OSRELEASE=4.52
WHEREIGO=$(DESTROOT)/usr/pkg/lib/umips$(OSRELEASE)
WHEREIGOSRC=$(WHEREIGO)-src
WHEREIGOOEMSRC=$(WHEREIGO)-oem-src

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)
INSTSCOPTS=$(ICOPY) -m 0755 -o $(OWNER) -g $(GROUP)


all clean clobber:
	@echo "Nothing to do"

install:
	ID=`id +%U` ; \
	for dir in usr usr/pkg usr/pkg/lib ; \
	{\
		[ -r $(DESTROOT)/$$dir ] || \
			( ( mkdir -p $(DESTROOT)/$$dir || exit 0) ; \
			  [ $$ID != 0 ] || \
			  ( chgrp $(GROUP) $(DESTROOT)/$$dir ; \
			    chmod 0775 $(DESTROOT)/$$dir ; \
			    chown $(OWNER) $(DESTROOT)/$$dir ) ) ; \
	} ; \
	for dir in $(WHEREIGO) $(WHEREIGO)/conv $(WHEREIGO)/lib \
		$(WHEREIGOSRC) $(WHEREIGOSRC)/conv $(WHEREIGOSRC)/lib ; \
	{\
		[ -r $$dir ] || \
			( ( mkdir -p $$dir || exit 0) ; \
			  [ $$ID != 0 ] || \
			  ( chgrp $(GROUP) $$dir ; \
			    chmod 0775 $$dir ; \
			    chown $(OWNER) $$dir ) ) ; \
	} ; \
	$(INSTALL) $(INSTOPTS) pkginfo $(WHEREIGO) ; \
	$(INSTALL) $(INSTOPTS) -f pkginfo-src $(WHEREIGOSRC)/pkginfo ; \
	if [ -d $(WHEREIGOOEMSRC) ] ; \
	then \
		$(INSTALL) $(INSTOPTS) -f pkginfo-oem-src \
			$(WHEREIGOOEMSRC)/pkginfo ; \
	fi ; \
	$(INSTALL) $(INSTSCOPTS) m120.ctape $(WHEREIGO)/conv ; \
	$(INSTALL) $(INSTSCOPTS) root.fstab $(WHEREIGO)/conv ; \
	$(INSTALL) $(INSTSCOPTS) root.preserves $(WHEREIGO)/lib ; \
	$(INSTALL) $(INSTSCOPTS) usr.preserves $(WHEREIGO)/lib ;






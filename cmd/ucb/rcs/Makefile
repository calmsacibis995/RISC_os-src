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
# $Header: Makefile,v 1.2.1.2 90/05/10 00:22:05 wje Exp $
#
ETCDIR	=/etc

DESTROOT=

WHEREIGO=$(DESTROOT)/usr/lib

SRCS	=copyright

WHOAMI	=copyright

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=666
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)


SHELL	=/bin/sh

SUBDIRS	=include a_rcs bsd.diff bsd.diff3 ci co ident merge rcs rcsdiff \
	rcsmerge rlog sccstorcs

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

install:install_copyright
generic:generic_copyright

install_copyright: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)

generic_copyright:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

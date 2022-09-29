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
# $Header: Makefile,v 1.1.2.2 90/05/09 17:09:10 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

WHEREIGO=$(DESTROOT)/etc/named.d

NAMES   =named.boot named.boot.master root.cache \
	named.hosts named.local named.rev

CPIOFLAGS=-pdmu

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ISTRIP	=
ICOPY	=-c
LINK	=
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP) $(LINK)

all clean clobber:
	@echo "Nothing to do"

install:
	mkdir -p $(WHEREIGO) || echo $(WHEREIGO) "already exists."
	for i in $(NAMES) ;\
	{\
		$(INSTALL) $(INSTOPTS) $$i $(WHEREIGO) ;\
	}

install: $(WHOAMI)

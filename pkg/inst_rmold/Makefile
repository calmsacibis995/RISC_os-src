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
# $Header: Makefile,v 1.1.2.2 90/05/10 03:53:36 wje Exp $
#
#	DESTROOT	The root of the destination directory.
#

SHELL   =/bin/sh
ETCDIR  =/etc

DESTROOT=

WHEREIGO=$(DESTROOT)/usr/pkg/bin

SRCS	= inst_rmold.sh

WHOAMI	= inst_rmold

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

all: $(WHOAMI)

install: all
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)

$(WHOAMI): $(SRCS)
	rm -f $(WHOAMI)
	cp $(SRCS) $(WHOAMI)

clobber:
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) $(SRCS) -h $(HDRS) -p $(WHOAMI) $(INCLUDE)

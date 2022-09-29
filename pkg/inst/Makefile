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
# $Header: Makefile,v 1.1.2.2 90/05/10 03:48:23 wje Exp $
#
#	DESTROOT	The root of the destination directory.
#

SHELL   =/bin/sh
ETCDIR  =/etc

DESTROOT=

WHEREIGO=$(DESTROOT)/usr/pkg/bin

SRCS	= inst.sh ../ask/ask.sh

WHOAMI	= inst

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
	sed -e '/#%askgoeshere/r ../ask/ask.sh' <inst.sh >$(WHOAMI)
	chmod 555 $(WHOAMI)

clobber:
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) $(SRCS) -h $(HDRS) -p $(WHOAMI) $(INCLUDE)

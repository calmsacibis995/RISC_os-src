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
# $Header: Makefile,v 1.2.2.2 90/05/10 03:38:55 wje Exp $
#
# This makefile is written to be generic, but the following macros should
# generally be re-defined by the caller.
#
#	DESTROOT	The root of the destination directory.
#

DESTROOT=

ICOPY=-c
ISTRIP=-s
INSTALL=/etc/mipsinstall
WHEREIGO=$(DESTROOT)/usr/games

SRCS	=wargames.sh

WHOAMI	=wargames

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(WHOAMI) $(WHEREIGO)

$(WHOAMI): $(SRCS)
	rm -f $(WHOAMI)
	cp $(SRCS) $(WHOAMI)

clean:

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -p $(WHOAMI) $(INCLUDE)

clobber:	clean
	-rm -f $(WHOAMI)

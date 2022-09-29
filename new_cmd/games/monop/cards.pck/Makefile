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
# $Header: Makefile,v 1.3.2.2 90/05/10 03:15:20 wje Exp $
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
WHEREIGO=$(DESTROOT)/usr/games/lib
LOCAL_CC=cc

SRCS	=deck.h initdeck.c cards.inp

WHOAMI	=cards.pck

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(ICOPY) $(WHOAMI) $(WHEREIGO)

$(WHOAMI): initdeck cards.inp
	./initdeck

clean:
	-rm -f initdeck

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -p $(WHOAMI) $(INCLUDE)

initdeck: initdeck.c deck.h
	$(LOCAL_CC) -w -o initdeck initdeck.c

clobber:
	-rm -f $(WHOAMI)

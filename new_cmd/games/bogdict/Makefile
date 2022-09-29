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
# $Header: Makefile,v 1.2.2.2 90/05/10 03:02:38 wje Exp $
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
LOCAL_CC=cc

SRCS	=comp.c sfile

WHOAMI	=bogdict

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(ICOPY) $(WHOAMI) $(WHEREIGO)

$(WHOAMI): comp sfile
	sed -f sfile /usr/dict/words | uniq | ./comp > $(WHOAMI)

clean:
	-rm -f comp

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -p $(WHOAMI) $(INCLUDE)

comp: comp.c
	$(LOCAL_CC) -o comp comp.c

clobber:	clean
	-rm -f $(WHOAMI)

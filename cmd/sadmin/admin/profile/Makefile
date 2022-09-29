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
# $Header: Makefile,v 1.2.2.2 90/05/09 18:33:50 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

WHEREIGO=$(DESTROOT)/usr/admin

SRCS	=profile.dot

WHOAMI	=profile.dot

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=root
GROUP	=bin
ICOPY	=-c
LINK	=-L $(DESTROOT)/usr/admin/.profile
INSTOPTS=-f $(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP) $(LINK)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)

clean:

clobber:

generic:
	$(GENERIC) -s $(SRCS) -d $(WHOAMI) \
		-R$(DESTROOT) -D$(WHEREIGO) \
		-L $(DESTROOT)/usr/admin/.profile

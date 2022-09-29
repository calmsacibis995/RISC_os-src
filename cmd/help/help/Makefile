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
# $Header: Makefile,v 1.2.2.2 90/05/09 16:11:04 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

WHEREIGO=$(DESTROOT)/usr/bin

SRCS	=help.sh

WHOAMI	=help

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ICOPY	=-c
LINKS	=-L $(DESTROOT)/usr/bin/glossary -L $(DESTROOT)/usr/bin/locate \
	-L $(DESTROOT)/usr/bin/starter -L $(DESTROOT)/usr/bin/usage
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP) $(LINKS)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)

$(WHOAMI): $(SRCS)
	-rm -f $(WHOAMI)
	cp $(SRCS) $(WHOAMI)

clean:

clobber:
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -p $(WHOAMI) \
		-R$(DESTROOT) -D$(WHEREIGO) \
		-L /usr/bin/glossary -L /usr/bin/locate \
		-L /usr/bin/starter -L /usr/bin/usage

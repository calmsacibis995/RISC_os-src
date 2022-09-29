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
# $Header: Makefile,v 1.1.1.2 90/05/09 14:58:17 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

WHEREIGO=$(DESTROOT)/dev/DEV_DB

SRCS	=RC3230.system

WHOAREWE=RC3230.system

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
LINKS	=-l $(DESTROOT)/dev/DEV_DB/RS3230.system
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP) $(LINKS)

all: $(WHOAREWE)

install: $(WHOAREWE)
	for name in $(WHOAREWE) ;\
	{\
		$(INSTALL) $(INSTOPTS) "$$name" $(WHEREIGO) ;\
	}

clean:

clobber:

generic:
	$(GENERIC) -s $(SRCS) -d $(WHOAREWE) \
		-R$(DESTROOT) -D$(WHEREIGO) \
		$(LINKS)

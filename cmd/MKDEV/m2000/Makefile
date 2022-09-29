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
# $Header: Makefile,v 1.1.2.3.1.2.1.2 90/10/16 11:45:37 beacker Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

WHEREIGO=$(DESTROOT)/dev/DEV_DB

SRCS	=m2000.system m2000.ipc m2000.ijc

WHOAREWE=m2000.system m2000.ipc m2000.ijc
WHOAREWE2=m2000-25.system m2000-25.ipc m2000-25.ijc m2000-25.sdc
SUFFIXES=system ipc ijc

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
#LINKS	=-l $(DESTROOT)/dev/DEV_DB/m2000-8.system \
#	 -l $(DESTROOT)/dev/DEV_DB/m2000-6.system
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP) $(LINKS)

all: $(WHOAREWE) $(WHOAREWE2)

install: $(WHOAREWE) $(WHOAREWE2)
	for name in $(SUFFIXES) ;\
	{\
		$(INSTALL) $(INSTOPTS) \
			   -l $(DESTROOT)/dev/DEV_DB/m2000-8.$$name \
			   -l $(DESTROOT)/dev/DEV_DB/m2000-6.$$name \
			   "m2000.$$name" $(WHEREIGO) ;\
	}
	for name in $(WHOAREWE2) ;\
	{\
		$(INSTALL) $(INSTOPTS) "$$name" $(WHEREIGO) ;\
	}

clean:

clobber:

generic:
	$(GENERIC) -s $(SRCS) -d $(WHOAREWE) \
		-R$(DESTROOT) -D$(WHEREIGO) \
		-l $(DESTROOT)/dev/DEV_DB/m2000-8.system \
		-l $(DESTROOT)/dev/DEV_DB/m2000-6.system

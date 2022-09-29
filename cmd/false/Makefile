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
# $Header: Makefile,v 1.2.2.2 90/05/09 15:49:38 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

WHEREIGO=$(DESTROOT)/bin

SRCS	=false.sh

WHOAMI	=false

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ICOPY	=-c
LINK	=-L $(DESTROOT)/bin/u3b -L $(DESTROOT)/bin/u3b2 \
	-L $(DESTROOT)/bin/u3b5 -L $(DESTROOT)/bin/vax \
	-L $(DESTROOT)/bin/pdp11 -L $(DESTROOT)/bin/m68000

INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP) $(LINK)

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
		-L /bin/u3b -L /bin/u3b2 -L /bin/u3b5 \
		-L /bin/vax -L /bin/pdp11 -L /bin/m68000

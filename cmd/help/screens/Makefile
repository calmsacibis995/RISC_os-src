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
# $Header: Makefile,v 1.3.2.2 90/05/09 16:13:54 wje Exp $

SHELL	=/bin/sh
ETCDIR	=/etc

ULIBDIR	=/usr/lib
ARCHIVE	=$(ULIBDIR)/help/replace

DESTROOT=

WHEREIGO=$(DESTROOT)/usr/lib/help/db

OBJS	=

SRCS	=basiccmds documents education help local locate starter \
	teach usage

HDRS	=

WHOAMI	=screens.a

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=664
OWNER	=root
GROUP	=daemon
ICOPY	=-c
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)

$(WHOAMI): $(SRCS)
	$(ARCHIVE) $(WHOAMI) $(SRCS)

clean:

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

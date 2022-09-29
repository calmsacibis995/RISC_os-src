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
# $Header: Makefile,v 1.3.2.2 90/05/09 16:10:18 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

ULIBDIR	=/usr/lib
ARCHIVE	=$(ULIBDIR)/help/replace

DESTROOT=

WHEREIGO=$(DESTROOT)/usr/lib/help/db

OBJS	=

SRCS	=ar at basename batch bdiff cat cd chgrp chmod chown cmp comm cp \
	cpio crontab cut date diff diff3 dircmp dirname echo ed egrep env \
	expr false fgrep file find glossary grep help kill line ln locate \
	login ls mail mesg mkdir mv news nice nohup pack passwd pcat pdp11 \
	pr ps pwd red rm rmdir rsh sdiff sed sh shl sleep sort starter stty \
	sum tabs tail tee test time touch true tty u3b2 u3b5 umask uname \
	unpack usage vax wait wall wc who write xargs

HDRS	=

WHOAMI	=examples.a

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=664
OWNER	=root
GROUP	=daemon
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

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

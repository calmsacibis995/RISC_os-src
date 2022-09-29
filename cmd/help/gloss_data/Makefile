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
# $Header: Makefile,v 1.3.2.2 90/05/09 16:10:45 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

ULIBDIR	=/usr/lib
ARCHIVE	=$(ULIBDIR)/help/replace

DESTROOT=

WHEREIGO=$(DESTROOT)/usr/lib/help/db

OBJS	=

SRCS	=define1 define2 define4 define5 define6 define7 define8 define9 \
	define10 define11 define12 define13 define14 define15 define16 \
	define17 define18 define20 define21 define22 define23 define24 \
	define25 define26 define27 define28 define29 define31 define32 \
	define33 define34 define35 define36 define38 define39 define40 \
	define41 define42 define43 define44 define45 define48 define49 \
	define50 define51 define52 define53 define54 define55 define56 \
	define58 define59 define60 define61 define62 define63 define64 \
	define65 define66 define67 define68 define69 define70 define71 \
	define72 define73 define74 define75 define76 define77 define78 \
	define79 define80 define81 define82 define83 define84 define85 \
	define86 define87 define88 define89 define90 define91 define92 \
	define93 define94 define95 define96 define97 define98 define99 \
	define100 define101 define102 define103 define104 define105 \
	define106 define107 define108 define109 define110 define111 \
	define112 define113 define114 define115

HDRS	=

WHOAMI	=glossary.a

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

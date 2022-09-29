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
# $Header: Makefile,v 1.3.2.2 90/05/07 21:41:14 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

DEBUG	=
MAXOPT	=-O2
INCLUDE	=
CFLAGS	=$(DEBUG) -systype bsd43 $(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(DEBUG) -systype bsd43 $(GNUM) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

WHEREIGO=$(DESTROOT)/bsd43/usr/lib

OBJS	=allprint.o main.o reject.o yyless.o yywrap.o

SRCS	=allprint.c main.c reject.c yyless.c yywrap.c

HDRS	=

LIBSUFFIX=
WHOAMI	=libl$(LIBSUFFIX).a

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
LINK	=-L $(DESTROOT)/bsd43/usr/lib/libln.a
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP) $(LINK)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)

$(WHOAMI): $(OBJS)
	$(AR) $(ARFLAGS) $(WHOAMI) $(OBJS)

clean:
	-rm -f $(OBJS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO) \
		-L /bsd43/usr/lib/libln.a

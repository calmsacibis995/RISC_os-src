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
# $Header: Makefile,v 1.1.2.2 90/05/09 17:11:08 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc
INC	=$(DESTROOT)/usr/include/bsd43

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=$(MAXOPT) -DRISCOS -systype bsd43 $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=-ll
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/usr/etc

CSRCS=	main.c getinfo.c debug.c send.c skip.c list.c subr.c
SRCS=	${CSRCS} commands.c
OBJS=	main.o getinfo.o debug.o send.o skip.o list.o subr.o commands.o

WHOAMI	=nslookup

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
LINK	=
INSTOPTS=-o $(OWNER) -g $(GROUP) $(LINK)

all: $(WHOAMI)

$(WHOAMI): $(OBJS)
	$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) $(LOCAL_LIBS) $(LIBRARIES)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(ISTRIP) -m 755 $(WHOAMI) $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) $(ICOPY)  -m 444 nslookup.help $(WHEREIGO)

clean: 
	rm -f $(OBJS) core commands.c nslookup.0 lex.yy.c lex.yy.o

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO) -l /etc/named


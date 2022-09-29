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
# $Header: Makefile,v 1.2.1.3 90/05/07 17:54:54 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O1
INCLUDE	=
CFLAGS	=-systype bsd43 $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =-systype bsd43 $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/usr/ucb

OBJS	=aux.o cmd1.o cmd2.o cmd3.o cmdtab.o collect.o config.o edit.o fio.o flsbuf.o getname.o head.o lex.o list.o main.o names.o optim.o popen.o quit.o send.o sigretro.o strings.o temp.o tty.o v7.local.o vars.o version.o

SRCS	=aux.c cmd1.c cmd2.c cmd3.c cmdtab.c collect.c config.c edit.c fio.c flsbuf.c getname.c head.c lex.c list.c main.c names.c optim.c popen.c quit.c send.c sigretro.c strings.c temp.c tty.c v7.local.c vars.c version.c

HDRS	=configdefs.h def.h glob.h local.h rcv.h sigretro.h v7.local.h

WHOAMI	=Mail

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
LINKS	=-L $(WHEREIGO)/mail
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP) $(LINKS)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)

$(WHOAMI): $(OBJS) $(LOCAL_LIBS)
	$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) $(LOCAL_LIBS) $(LIBRARIES)

clean:
	-rm -f $(OBJS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

$(OBJS):	$(HDRS)

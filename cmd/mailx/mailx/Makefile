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
# $Header: Makefile,v 1.5.1.2.1.2 90/08/08 17:59:16 hawkes Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O1
INCLUDE	=-I../include
CFLAGS	=$(MAXOPT) -DUSG -DUSG_TTY -DLOCKS $(INCLUDE) $(ECFLAGS) -DVIS
ASFLAGS =$(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=-lPW
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/usr/bin

OBJS	=aux.o cmd1.o cmd2.o cmd3.o cmd4.o cmdtab.o collect.o config.o \
	edit.o extern.o fio.o getname.o head.o hostname.o lex.o list.o \
	lock.o lpaths.o main.o names.o optim.o popen.o quit.o send.o \
	sigretro.o stralloc.o temp.o tty.o usg.local.o vars.o version.o

SRCS	=aux.c cmd1.c cmd2.c cmd3.c cmd4.c cmdtab.c collect.c config.c \
	edit.c extern.c fio.c getname.c head.c hostname.c lex.c list.c \
	lock.c lpaths.c main.c names.c optim.c popen.c quit.c send.c \
	sigretro.c stralloc.c temp.c tty.c usg.local.c vars.c version.c

HDRS	=

WHOAMI	=mailx

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=2555
OWNER	=bin
GROUP	=mail
ISTRIP	=-s
ICOPY	=-c
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP)

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

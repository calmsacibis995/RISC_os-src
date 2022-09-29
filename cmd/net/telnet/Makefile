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
# $Header: Makefile,v 1.5.1.2 90/05/09 17:38:31 wje Exp $
#
# This makefile is written to be generic, but the following macros should
# generally be re-defined by the caller.
#
#	DESTDIR	The root of the destination directory.
#

DESTDIR=

DEBUG	=
INCLUDE	=
DEF_CFLAGS=-O2
CFLAGS	=$(DEF_CFLAGS) -systype bsd43 -DRISCOS $(DEBUG) $(INCLUDE)

LIBRARIES=

WHEREIGO=$(DESTROOT)/usr/ucb

OBJS=	commands.o main.o network.o ring.o sys_bsd.o sys_dos.o telnet.o \
	terminal.o tn3270.o utilities.o

SRCS=	commands.c main.c network.c ring.c sys_bsd.c sys_dos.c telnet.c \
	terminal.c tn3270.c utilities.c

HDRS	=

WHOAMI	=telnet

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=2555
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
LINK	=-l $(DESTROOT)/usr/net/telnet
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP) $(LINK)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)

$(WHOAMI): $(OBJS)
	$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) $(LIBRARIES)

clean:
	-rm -f $(OBJS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

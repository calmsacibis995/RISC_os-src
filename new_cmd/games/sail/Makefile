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
# $Header: Makefile,v 1.2.2.2 90/05/10 03:22:10 wje Exp $
#
# This makefile is written to be generic, but the following macros should
# generally be re-defined by the caller.
#
#	DESTROOT	The root of the destination directory.
#

DESTROOT=

DEBUG	=
INCLUDE	=
ICOPY=-c
ISTRIP=-s
INSTALL=/etc/mipsinstall
DEF_CFLAGS=-O1
CFLAGS	= -systype bsd43 $(DEF_CFLAGS) $(DEBUG) $(INCLUDE) -DRISCOS

LIBRARIES=-lcurses -ltermcap

WHEREIGO=$(DESTROOT)/usr/games

OBJS	=assorted.o dr_1.o dr_2.o dr_3.o dr_4.o dr_5.o dr_main.o game.o \
	globals.o lo_main.o main.o misc.o parties.o pl_1.o pl_2.o pl_3.o \
	pl_4.o pl_5.o pl_6.o pl_7.o pl_main.o sync.o version.o

SRCS	=assorted.c dr_1.c dr_2.c dr_3.c dr_4.c dr_5.c dr_main.c game.c \
	globals.c lo_main.c main.c misc.c parties.c pl_1.c pl_2.c pl_3.c \
	pl_4.c pl_5.c pl_6.c pl_7.c pl_main.c sync.c version.c

HDRS	=driver.h externs.h machdep.h player.h

WHOAMI	=sail

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(ICOPY) $(ISTRIP) -m 4555 -o daemon $(WHOAMI) $(WHEREIGO)

$(WHOAMI): $(OBJS)
	$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) $(LIBRARIES)

clean:
	-rm -f $(OBJS)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) $(INCLUDE)

$(OBJS):	$(HDRS)

clobber:	clean
	-rm -f $(WHOAMI)

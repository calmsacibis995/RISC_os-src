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
# $Header: Makefile,v 1.2.2.3 90/05/10 03:19:52 wje Exp $
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
CFLAGS	= -systype bsd43 $(DEF_CFLAGS) $(DEBUG) $(INCLUDE) -DMAX_PER_UID=5 -DRISCOS

LIBRARIES=-lcurses -ltermcap

WHEREIGO=$(DESTROOT)/usr/games

OBJS	=extern.o flush_in.o init_field.o main.o make_level.o move.o \
	move_robs.o play_level.o query.o rnd_pos.o score.o

SRCS	=extern.c flush_in.c init_field.c main.c make_level.c move.c \
	move_robs.c play_level.c query.c rnd_pos.c score.c

HDRS	=robots.h

WHOAMI	=robots

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

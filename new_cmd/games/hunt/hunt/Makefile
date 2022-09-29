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
# $Header: Makefile,v 1.2.2.3 90/05/10 03:11:51 wje Exp $
#
# This makefile is written to be generic, but the following macros should
# generally be re-defined by the caller.
#
#	DESTROOT	The root of the destination directory.
#

DESTROOT=

ETCDIR=/etc
DEBUG	=
INCLUDE	=-I../include
ICOPY=-c
ISTRIP=-s
INSTALL=${ETCDIR}/mipsinstall
DEFS	=-DBROADCAST -DRANDOM -DREFLECT -DMONITOR -DOOZE -DFLY -DVOLCANO \
	-DINTERNET -DRISCOS
DEF_CFLAGS=-O2
CFLAGS	= -systype bsd43 $(DEF_CFLAGS) $(DEBUG) $(INCLUDE) $(DEFS)

LOCAL_LIBS=../a_pathname/pathname.a
LIBRARIES=$(LOCAL_LIBS) -lcurses -ltermcap

WHEREIGO=$(DESTROOT)/usr/games

OBJS	=connect.o hunt.o playit.o

SRCS	=connect.c hunt.c playit.c

HDRS	=

WHOAMI	=hunt

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(ICOPY) $(ISTRIP) $(WHOAMI) $(WHEREIGO)

$(WHOAMI): $(OBJS) $(LOCAL_LIBS)
	$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) $(LIBRARIES)

clean:
	-rm -f $(OBJS)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) $(INCLUDE)

clobber:	clean
	-rm -f $(WHOAMI)

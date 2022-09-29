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
# $Header: Makefile,v 1.2.2.2 90/05/10 02:58:13 wje Exp $
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
DEF_CFLAGS=-O2
CFLAGS	= -systype bsd43 $(DEF_CFLAGS) $(DEBUG) $(INCLUDE)
# CFLAGS	=$(DEF_CFLAGS) $(DEBUG) $(INCLUDE) -G 129

LIBRARIES=-lcurses -ltermcap

WHEREIGO=$(DESTROOT)/usr/games

OBJS	=battlestar.o com1.o com2.o com3.o com4.o com5.o com6.o com7.o \
	cypher.o dayfile.o dayobjs.o fly.o getcom.o globals.o init.o misc.o \
	nightfile.o nightobjs.o parse.o room.o save.o words.o

SRCS	=battlestar.c com1.c com2.c com3.c com4.c com5.c com6.c com7.c \
	cypher.c dayfile.c dayobjs.c fly.c getcom.c globals.c init.c misc.c \
	nightfile.c nightobjs.c parse.c room.c save.c words.c

HDRS	=externs.h

WHOAMI	=battlestar

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(ICOPY) $(ISTRIP) $(WHOAMI) $(WHEREIGO)

$(WHOAMI): $(OBJS)
	$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) $(LIBRARIES)

clean:
	-rm -f $(OBJS)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) $(INCLUDE)

$(OBJS):	$(HDRS)

clobber:	clean
	-rm -f $(WHOAMI)

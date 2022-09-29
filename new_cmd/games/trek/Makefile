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
# $Header: Makefile,v 1.2.3.2 90/05/10 03:30:05 wje Exp $
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
CFLAGS	= -systype bsd43 $(DEF_CFLAGS) $(DEBUG) $(INCLUDE)

LIBRARIES=-lm

WHEREIGO=$(DESTROOT)/usr/games

OBJS	=abandon.o attack.o autover.o capture.o cgetc.o check_out.o \
	checkcond.o compkl.o computer.o damage.o damaged.o dcrept.o \
	destruct.o dock.o dumpgame.o dumpme.o dumpssradio.o events.o \
	externs.o getcodi.o getpar.o help.o impulse.o initquad.o kill.o \
	klmove.o lose.o lrscan.o main.o move.o nova.o out.o phaser.o play.o \
	ram.o ranf.o rest.o schedule.o score.o setup.o setwarp.o shell.o \
	shield.o snova.o srscan.o systemname.o torped.o utility.o visual.o \
	warp.o win.o

SRCS	=abandon.c attack.c autover.c capture.c cgetc.c check_out.c \
	checkcond.c compkl.c computer.c damage.c damaged.c dcrept.c \
	destruct.c dock.c dumpgame.c dumpme.c dumpssradio.c events.c \
	externs.c getcodi.c getpar.c help.c impulse.c initquad.c kill.c \
	klmove.c lose.c lrscan.c main.c move.c nova.c out.c phaser.c play.c \
	ram.c ranf.c rest.c schedule.c score.c setup.c setwarp.c shell.c \
	shield.c snova.c srscan.c systemname.c torped.c utility.c visual.c \
	warp.c win.c

HDRS	=getpar.h trek.h

WHOAMI	=trek

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

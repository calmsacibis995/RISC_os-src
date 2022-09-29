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
# $Header: Makefile,v 1.5.2.2 90/05/09 18:55:09 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O1
INCLUDE	=
DEFS	=-DRISCOS
CFLAGS	=$(MAXOPT) $(INCLUDE) $(DEFS) $(ECFLAGS)
ASFLAGS =$(DEFS) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

#
# NOTE: The following special definitions are available:
#
#	-DNICE		if defined then a backround process will be
#			niced down to NICEVAL.  Note if defined, NICEVAL
#			must be defined.
#	-DNICEVAL=XX	The value to nice backround processes to. The typical
#			value used is 4.
#	-DTIME_OUT	if defined the shell will timeout and log you off
#			if no input is typed after TIMEOUT seconds. Currently
#			0, and not defined.
#

LIBRARIES=-lbsd
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/bin

OBJS	=args.o blok.o cmd.o ctype.o defs.o echo.o error.o expand.o fault.o \
	func.o hash.o hashserv.o homedir.o io.o jobs.o macro.o main.o msg.o \
	name.o print.o profile.o pwd.o service.o setbrk.o signal.o stak.o \
	string.o test.o ulimit.o word.o xec.o

SRCS	=args.c blok.c cmd.c ctype.c defs.c echo.c error.c expand.c fault.c \
	func.c hash.c hashserv.c homedir.c io.c jobs.c macro.c main.c msg.c \
	name.c print.c profile.c pwd.c service.c setbrk.c signal.c stak.c \
	string.c test.c ulimit.c word.c xec.c

HDRS	=brkincr.h ctype.h defaults.h defs.h dup.h hash.h mac.h mode.h name.h \
	stak.h sym.h timeout.h

WHOAMI	=sh

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
LINK1	=-L $(DESTROOT)/bin/rsh
LINK2	=-L $(DESTROOT)/bin/jsh
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP) $(LINK1) $(LINK2)

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
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO) \
		$(LINK1) $(LINK2)

$(OBJS):	$(HDRS)

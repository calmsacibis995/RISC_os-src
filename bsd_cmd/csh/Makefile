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
# $Header: Makefile,v 1.5.3.2 90/05/07 18:12:34 wje Exp $
#
# This makefile is written to be generic, but the following macros should
# generally be re-defined by the caller.
#
#	DESTDIR	The root of the destination directory.
#

SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=
DESTDIR=$(DESTROOT)

DEBUG	=
MAXOPT	=-O1
INCLUDE	=
DEFS	=-DTELL -DVMUNIX -DFILEC -DTSH -DRISCOS -DTERMIO
# DEFS	=-DTELL -DVMUNIX -DVFORK -DFILEC -DTSH
DEF_CFLAGS=$(MAXOPT)
CFLAGS	=-systype bsd43 $(DEF_CFLAGS) $(DEBUG) $(INCLUDE) $(DEFS) $(ECFLAGS)
ASFLAGS =-systype bsd43 $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

WHEREIGO=$(DESTDIR)/bin

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP)

OBJS	=alloc.o complete.o \
	line_edit.o line_support.o printf.o sh.o sh.dir.o sh.dol.o \
	sh.err.o sh.exec.o sh.exp.o sh.func.o sh.glob.o sh.hist.o sh.init.o \
	sh.lex.o sh.misc.o sh.parse.o sh.print.o sh.proc.o sh.sem.o sh.set.o \
	sh.time.o tsh.o doprnt.o sh.char.o sh.file.o

SRCS	=alloc.c complete.c \
	line_edit.c line_support.c printf.c sh.c sh.dir.c sh.dol.c \
	sh.err.c sh.exec.c sh.exp.c sh.func.c sh.glob.c sh.hist.c sh.init.c \
	sh.lex.c sh.misc.c sh.parse.c sh.print.c sh.proc.c sh.sem.c sh.set.c \
	sh.time.c tsh.c doprnt.c sh.char.c sh.file.c

HDRS	=sh.dir.h sh.h sh.proc.h tsh.h sh.local.h sh.char.h

WHOAMI	=csh

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

$(OBJS) : $(HDRS)

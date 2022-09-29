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
# $Header: Makefile,v 1.4.1.4 90/05/11 00:29:57 wje Exp $
#
# This makefile is written to be generic, but the following macros should
# generally be re-defined by the caller.
#
#	DESTROOT	The root of the destination directory.
#

SHELL	=/bin/sh
ETCDIR	=/etc
DESTROOT=

DEBUG	=
INCLUDE	=
CFLAGS	=-systype bsd43 $(ECFLAGS) $(DEF_CFLAGS) $(DEBUG) $(INCLUDE)
ASFLAGS =-systype bsd $(EASFLAGS)
LFLAGS  =$(ELFLAGS)
YFLAGS  =-d

LOCAL_CC=cc -systype bsd43
LIBRARIES=-lm

WHEREIGO=$(DESTROOT)/bsd43/bin

OBJS	=awk.lx.o b.o main.o token.o tran.o lib.o run.o parse.o proctab.o \
	freeze.o awk.g.o

SRCS	=awk.lx.l b.c main.c tokenbase.c tran.c lib.c run.c parse.c \
	makeprctab.c freeze.c awk.g.y tokenscript

HDRS	=awk.def

WHOAMI	=awk

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ISTRIP  =-s
ICOPY	=-c
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)


$(WHOAMI): $(OBJS)
	$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) $(LIBRARIES)

clean:
	-rm -f $(OBJS) $(WHOAMI)
	-rm -f makeprctab.o temp awk.h token.c awk.g.c makeprctab \
		native_token.o lex.yy.c proctab.c native_token.c
clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)


$(OBJS):	$(HDRS) awk.h

token.c:	awk.h tokenscript tokenbase.c
	-rm -f token.c
	cat tokenbase.c > token.c
	ed - < tokenscript
	-rm -f native_token.c
	cp token.c native_token.c

native_token.c:	token.c
	-rm -f native_token.c
	cp token.c native_token.c

proctab.c:	./makeprctab
	./makeprctab > proctab.c

./makeprctab:	makeprctab.o native_token.o
	$(LOCAL_CC) -o ./makeprctab makeprctab.o native_token.o

native_token.o: native_token.c awk.h
	$(LOCAL_CC) -c native_token.c

makeprctab.o: makeprctab.c awk.h
	$(LOCAL_CC) -c makeprctab.c

awk.g.o:	awk.def awk.g.c

awk.h awk.g.c:	awk.g.y
	@echo "Expect 95 shift/reduce conflicts"
	-$(YACC) $(YFLAGS) awk.g.y
	mv -f y.tab.h awk.h
	mv -f y.tab.c awk.g.c

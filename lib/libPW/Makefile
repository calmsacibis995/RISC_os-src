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
# $Header: Makefile,v 1.6.2.2 90/05/10 01:07:11 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=$(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(GNUM) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

WHEREIGO=$(DESTROOT)/usr/lib

OBJS	=abspath.o any.o anystr.o bal.o cat.o clean.o curdir.o dname.o \
	fatal.o fdfopen.o giveup.o imatch.o index.o lockit.o logname.o \
	move.o patoi.o patol.o regcmp.o regex.o rename.o repeat.o repl.o \
	satoi.o setsig.o sname.o strend.o substr.o trnslat.o userdir.o \
	userexit.o username.o verify.o xalloc.o xcreat.o xlink.o xmsg.o \
	xopen.o xpipe.o xunlink.o xwrite.o zero.o zeropad.o

SRCS	=abspath.c any.c anystr.c bal.c cat.c clean.c curdir.c dname.c \
	fatal.c fdfopen.c giveup.c imatch.c index.c lockit.c logname.c \
	move.c patoi.c patol.c regcmp.c regex.c rename.c repeat.c repl.c \
	satoi.c setsig.c sname.c strend.c substr.c trnslat.c userdir.c \
	userexit.c username.c verify.c xalloc.c xcreat.c xlink.c xmsg.c \
	xopen.c xpipe.c xunlink.c xwrite.c zero.c zeropad.c

HDRS	=

LIBSUFFIX=
WHOAMI	=libPW$(LIBSUFFIX).a

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)

$(WHOAMI): $(OBJS)
	$(AR) $(ARFLAGS) $(WHOAMI) $(OBJS)

clean:
	-rm -f $(OBJS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

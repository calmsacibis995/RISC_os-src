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
# $Header: Makefile,v 1.2.2.3 90/05/09 15:05:40 wje Exp $
#	Copyright (c) 1984 AT&T
#	  All Rights Reserved

#	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T
#	The copyright notice above does not evidence any
#	actual or intended publication of such source code.


TESTDIR = .
LIB = a.a
CFLAGS = -O -I..
INCDIR = ..
FRC =

OBJS= \
	pnpsplit.o \
	copyn.o \
	devtolin.o \
	expand.o \
	lintodev.o \
	namtouid.o \
	tmless.o \
	tmsecs.o \
	uidtonam.o \
	substr.o \
	regex.o \
	regcmp.o

all:	$(LIB) $(FRC)

$(LIB):	$(OBJS)
	ar rv $(LIB) $(OBJS)

clean:
	-rm -f *.o

clobber:	clean
	-rm -f $(LIB)

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
# $Header: Makefile,v 1.6.2.2 90/05/09 16:18:46 wje Exp $
#
SHELL	=/bin/sh

MAXOPT	=-O1
INCLUDE	=-I../include
CFLAGS	=$(MAXOPT) -DEDTINIT -DATBUS -DMULTIPLE_MAJOR $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

ARFLAGS	=crs

OBJS	=error.o main.o master.o mkboot.o subr.o util.o

SRCS	=error.c main.c master.c mkboot.y subr.c util.c xmkboot.l

HDRS	=

WHOAMI	=libboot.a

all: $(WHOAMI)

install: $(WHOAMI)

$(WHOAMI): $(OBJS)
	$(AR) $(ARFLAGS) $(WHOAMI) $(OBJS)

clean:
	-rm -f $(OBJS) lex.yy.c

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

mkboot.o: lex.yy.c

lex.yy.c: xmkboot.l
	$(LEX) xmkboot.l

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
# $Header: Makefile,v 1.2.2.2 90/05/10 03:10:05 wje Exp $
#
DEBUG	=
INCLUDE	=
ICOPY=-c
ISTRIP=-s
INSTALL=/etc/mipsinstall
DEF_CFLAGS=-O2
CFLAGS	= -systype bsd43 $(DEF_CFLAGS) $(DEBUG) $(INCLUDE) -DINTERNET
ARFLAGS	=crs
AR = ar

OBJS	=pathname.o

SRCS	=pathname.c

HDRS	=

WHOAMI	=pathname.a

all: $(WHOAMI)

install: $(WHOAMI)

$(WHOAMI): $(OBJS)
	$(AR) $(ARFLAGS) $(WHOAMI) $(OBJS)

clean:
	-rm -f $(OBJS)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) $(INCLUDE)

clobber:	clean
	-rm -f $(WHOAMI)

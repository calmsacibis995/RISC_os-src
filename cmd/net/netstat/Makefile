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
# |         950 DeGuigne Drive                                |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
#
# $Header: Makefile,v 1.8.2.4.1.2 90/07/19 12:56:36 hawkes Exp $
#
SHELL=/bin/sh
ETCDIR=/etc

DESTROOT=

DEBUG	=
MAXOPT	=-O2
INCLUDE	= \
	 -I../../../lib/libmips
CFLAGS	=-systype bsd43 -Dmips -DRISCOS $(DEBUG) $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =-systype bsd43 $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=-lmld
LOCAL_LIBS=../../../lib/libmips/bsd43/libmips.a

WHEREIGO=$(DESTROOT)/usr/etc

OBJS	=host.o if.o inet.o main.o mbuf.o route.o ns.o unix.o

SRCS	=host.c if.c inet.c main.c mbuf.c route.c ns.c unix.c

HDRS	=

WHOAMI	=netstat

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=2555
OWNER	=bin
GROUP	=sys
ISTRIP	=-s
ICOPY	=-c
LINK	=-l $(DESTROOT)/etc/netstat
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP) $(LINK)

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
		-l /etc/netstat

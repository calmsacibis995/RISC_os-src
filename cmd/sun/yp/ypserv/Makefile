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
# $Header: Makefile,v 1.1.1.2 90/05/09 19:34:19 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=-I..
CFLAGS	=-systype bsd43 $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =-systype bsd43 $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=-ldbm
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/usr/etc/yp

OBJS	=ypserv.o ypserv_ancil.o ypserv_map.o ypserv_proc.o

SRCS	=ypserv.c ypserv_ancil.c ypserv_map.c ypserv_proc.c

HDRS	=

WHOAMI	=ypserv

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP)

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
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

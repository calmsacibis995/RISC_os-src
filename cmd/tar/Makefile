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
# $Header: Makefile,v 1.4.2.2 90/05/09 19:37:21 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=$(MAXOPT) $(INCLUDE) $(ECFLAGS)
BSD43_CFLAGS=-systype bsd43 $(CFLAGS)
ASFLAGS =$(EASFLAGS)
BSD43_ASFLAGS=-systype bsd43 $(ASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=-lbsd
LOCAL_LIBS=
BSD43_LIBRARIES=

WHEREIGO=$(DESTROOT)/bin
BSD43_WHEREIGO=$(DESTROOT)/bsd43/bin

OBJS	=tar.o rmttape.o
BSD43_OBJS = bsd43-tar.o bsd43-rmttape.o

SRCS	=tar.c rmttape.c

HDRS	=

WHOAMI	=tar
BSD43_WHOAMI = bsd43-tar

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP)

all: $(WHOAMI) $(BSD43_WHOAMI)

install: $(WHOAMI) $(BSD43_WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) -f $(BSD43_WHOAMI) $(BSD43_WHEREIGO)/$(WHOAMI)

$(WHOAMI): $(OBJS) $(LOCAL_LIBS)
	$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) $(LOCAL_LIBS) $(LIBRARIES)

bsd43-tar.o: tar.c
	$(CC) $(BSD43_CFLAGS) -c -o bsd43-tar.o tar.c

bsd43-rmttape.o: rmttape.c
	$(CC) $(BSD43_CFLAGS) -c -o bsd43-rmttape.o rmttape.c

$(BSD43_WHOAMI): $(BSD43_OBJS) $(LOCAL_LIBS)
	$(CC) $(BSD43_CFLAGS) -o $(BSD43_WHOAMI) $(BSD43_OBJS) $(LOCAL_LIBS) $(BSD43_LIBRARIES)

clean:
	-rm -f $(OBJS) $(BSD43_OBJS)

clobber: clean
	-rm -f $(WHOAMI) $(BSD43_WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(BSD43_OBJS) -p $(BSD43_WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(BSD43_WHEREIGO)

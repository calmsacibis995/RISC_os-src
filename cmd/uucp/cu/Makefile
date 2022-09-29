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
# $Header: Makefile,v 1.3.2.3 90/05/10 00:38:53 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=-I../include
CFLAGS	=$(MAXOPT) -DSTANDALONE -DRISCOS $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=
LOCAL_LIBS=../a_uucp/libuucp.a

WHEREIGO=$(DESTROOT)/usr/bin

OBJS	=altconn.o cu.o culine.o conn.o callers.o uucpdefs.o

SRCS	=altconn.c cu.c culine.c

HDRS	=

WHOAMI	=cu

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=4111
OWNER	=uucp
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

conn.o: ../a_uucp/conn.c
	$(CC) $(CFLAGS) -c ../a_uucp/conn.c

callers.o: ../a_uucp/callers.c
	$(CC) $(CFLAGS) -c ../a_uucp/callers.c

uucpdefs.o: ../a_uucp/uucpdefs.c
	$(CC) $(CFLAGS) -c ../a_uucp/uucpdefs.c

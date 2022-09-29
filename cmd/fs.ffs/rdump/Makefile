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
# $Header: Makefile,v 1.2.1.2 90/05/09 15:58:55 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=$(MAXOPT) -DRDUMP $(INCLUDE) $(ECFLAGS) -DRISCOS -systype bsd43
ASFLAGS =$(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/etc

OBJS	=dumpitime.o dumpmain.o dumpoptr.o dumptape.o dumptraverse.o \
	unctime.o dumprmt.o

SRCS	=dumprmt.c

HDRS	=dump.h

WHOAMI	=rdump.ffs

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

$(OBJS): $(HDRS)

dump.h:
	[ -r dump.h ] || ln -s ../dump/dump.h .

dumpitime.o:	../dump/dumpitime.c
	$(CC) $(CFLAGS) -c ../dump/dumpitime.c

dumpmain.o:	../dump/dumpmain.c
	$(CC) $(CFLAGS) -c ../dump/dumpmain.c

dumpoptr.o:	../dump/dumpoptr.c
	$(CC) $(CFLAGS) -c ../dump/dumpoptr.c

dumptape.o:	../dump/dumptape.c
	$(CC) $(CFLAGS) -c ../dump/dumptape.c

dumptraverse.o:	../dump/dumptraverse.c
	$(CC) $(CFLAGS) -c ../dump/dumptraverse.c

unctime.o:	../dump/unctime.c
	$(CC) $(CFLAGS) -c ../dump/unctime.c

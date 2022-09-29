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
# $Header: Makefile,v 1.4.1.4 90/05/10 20:16:25 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O1
INCLUDE	=
CFLAGS	=$(MAXOPT) -DRISCOS $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=-lmld
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/usr/etc

OBJS	=base.o buf.o callout.o dis.o fs.o init.o inode.o lck.o \
	main.o major.o map.o mount.o page.o proc.o search.o size.o \
	sizenet.o stat.o stream.o symtab.o u.o util.o var.o vtop.o

SRCS	=base.c buf.c callout.c dis.c fs.c init.c inode.c lck.c \
	main.c major.c map.c mount.c page.c proc.c search.c size.c \
	sizenet.c stat.c stream.c symtab.c u.c util.c var.c vtop.c

OBSOLETESRCS =bfs.c rfs.c

HDRS	=crash.h

WHOAMI	=crash

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

$(OBJS):	$(HDRS)

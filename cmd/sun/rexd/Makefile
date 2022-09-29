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
# $Header: Makefile,v 1.1.1.4 90/05/09 19:15:29 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=-systype bsd43 $(MAXOPT) $(INCLUDE) $(ECFLAGS) -DRISCOS
ASFLAGS =-systype bsd43 $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=-lrpcsvc
LOCAL_LIBS=


OBJS	=rexd.o mount_nfs.o unix_login.o on.o where.o under.o where_main.o

SRCS	=rexd.c mount_nfs.c unix_login.c on.c where.c under.c where_main.c

HDRS	=


INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
LINK  =-l $(DESTROOT)/etc/rpc.rexd
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP) $(LINK)

all: rexd on under where

install: all
	cp rexd rpc.rexd
	$(INSTALL) $(INSTOPTS) rpc.rexd $(DESTROOT)/usr/etc
	$(INSTALL) $(INSTOPTS) on $(DESTROOT)/usr/bin

rexd: rexd.o mount_nfs.o unix_login.o  $(LOCAL_LIBS)
	$(CC) $(CFLAGS) -o rexd rexd.o mount_nfs.o unix_login.o $(LOCAL_LIBS) $(LIBRARIES)

on: on.o where.o  $(LOCAL_LIBS)
	$(CC) $(CFLAGS) -o on on.o where.o $(LOCAL_LIBS) $(LIBRARIES)

under: under.o mount_nfs.o  $(LOCAL_LIBS)
	$(CC) $(CFLAGS) -o under under.o mount_nfs.o $(LOCAL_LIBS) $(LIBRARIES)

where: where_main.o where.o  $(LOCAL_LIBS)
	$(CC) $(CFLAGS) -o where where_main.o where.o $(LOCAL_LIBS) $(LIBRARIES)

clean:
	-rm -f $(OBJS)

clobber: clean
	-rm -f on rexd where under

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

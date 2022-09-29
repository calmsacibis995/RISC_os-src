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
# $Header: Makefile,v 1.3.1.1.1.1 90/10/05 09:48:52 beacker Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O1
INCLUDE	=-I.
CFLAGS	=-systype bsd43 -DVMUNIX -signed $(MAXOPT) $(INCLUDE) $(ECFLAGS) -DRISCOS
ASFLAGS =-systype bsd43 $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=-ldbm -lmld
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/usr/lib

OBJS	=alias.o arpadate.o clock.o collect.o conf.o daemon.o deliver.o domain.o envelope.o err.o headers.o main.o parseaddr.o queue.o readcf.o recipient.o remote.o savemail.o srvrsmtp.o stats.o usersmtp.o util.o version.o

OBJS2	= convtime.o macro.o stab.o sysexits.o trace.o

SRCS	=alias.c arpadate.c clock.c conf.c daemon.c deliver.c domain.c envelope.c err.c headers.c main.c parseaddr.c queue.c readcf.c recipient.c remote.c savemail.c srvrsmtp.c stats.c usersmtp.c util.c version.c

SRCS2	= convtime.c macro.c stab.c sysexits.c trace.c

HDRS	=conf.h mailstats.h sendmail.h

HDRS2	=useful.h

WHOAMI	=sendmail.nis

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=4555
OWNER	=root
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
LINKS	=
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP) $(LINKS)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)

$(WHOAMI): $(OBJS) $(OBJS2) $(LOCAL_LIBS)
	$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) $(OBJS2) $(LOCAL_LIBS) $(LIBRARIES)


	
clean:
	-rm -f $(OBJS)
	-rm -f $(OBJS2)
	-rm -f $(HDRS2)
	-rm -f $(SRCS2)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

$(HDRS2):
	ln -s ../sendmail/$@ $@

$(SRCS2):
	ln -s ../sendmail/$@ $@

$(OBJS):	$(HDRS) $(HDRS2)

$(OBJS2):	$(HDRS) $(HDRS2)


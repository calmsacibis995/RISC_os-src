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
# $Header: Makefile,v 1.3.2.4.1.2 90/08/23 09:44:21 hawkes Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O1
INCLUDE	=-I.
CFLAGS	=-systype bsd43 -DRISCOS -DVMUNIX -signed $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =-systype bsd43 $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=-ldbm -lmld
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/usr/lib

OBJS	=Version.o alias.o arpadate.o clock.o collect.o conf.o convtime.o envelope.o err.o headers.o macro.o parseaddr.o queue.o readcf.o recipient.o savemail.o srvrsmtp.o stab.o stats.o sysexits.o trace.o usersmtp.o util.o

NAMEDOBJS = daemon.o deliver.o domain.o main.o

NONAMEDOBJS = nnd_daemon.o nnd_deliver.o nnd_domain.o nnd_main.o

SRCS	=Version.c alias.c arpadate.c clock.c collect.c conf.c convtime.c envelope.c err.c headers.c macro.c parseaddr.c queue.c readcf.c recipient.c savemail.c srvrsmtp.c stab.c stats.c sysexits.c trace.c usersmtp.c util.c

HDRS	=conf.h mailstats.h sendmail.h useful.h

WHOAMI	=sendmail
WHOAMI_NND=sendmail.nonamed
INSTALL	=$(ETCDIR)/mipsinstall
MODE	=4555
OWNER	=root
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
LINKS	=-l $(DESTROOT)/usr/ucb/newaliases -l $(DESTROOT)/usr/ucb/mailq -L $(DESTROOT)/usr/lib/sendmail.named
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP) $(LINKS)

all: $(WHOAMI) $(WHOAMI_NND)

install: $(WHOAMI) $(WHOAMI_NND)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)
	$(INSTALL) $(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP) $(WHOAMI_NND) $(WHEREIGO)

$(WHOAMI): $(OBJS) $(NAMEDOBJS) $(LOCAL_LIBS)
	$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) $(NAMEDOBJS) $(LOCAL_LIBS) $(LIBRARIES)

$(WHOAMI_NND): $(OBJS) $(NONAMEDOBJS) $(LOCAL_LIBS)
	$(CC) $(CFLAGS) -o $(WHOAMI_NND) $(OBJS) $(NONAMEDOBJS) $(LOCAL_LIBS) $(LIBRARIES)

nnd_daemon.o: daemon.c $(HDRS)
	$(CC) $(CFLAGS) -UNAMED_BIND -o nnd_daemon.o -c daemon.c

nnd_deliver.o: deliver.c $(HDRS)
	$(CC) $(CFLAGS) -UNAMED_BIND -o nnd_deliver.o -c deliver.c

nnd_domain.o: domain.c $(HDRS)
	$(CC) $(CFLAGS) -UNAMED_BIND -o nnd_domain.o -c domain.c

nnd_main.o: main.c $(HDRS)
	$(CC) $(CFLAGS) -UNAMED_BIND -o nnd_main.o -c main.c

daemon.o: daemon.c $(HDRS)
	$(CC) $(CFLAGS) -DNAMED_BIND -o daemon.o -c daemon.c

deliver.o: deliver.c $(HDRS)
	$(CC) $(CFLAGS) -DNAMED_BIND -o deliver.o -c deliver.c

domain.o: domain.c $(HDRS)
	$(CC) $(CFLAGS) -DNAMED_BIND -o domain.o -c domain.c

main.o: main.c $(HDRS)
	$(CC) $(CFLAGS) -DNAMED_BIND -o main.o -c main.c

clean:
	-rm -f $(OBJS) $(NAMEDOBJS) $(NONAMEDOBJS)

clobber: clean
	-rm -f $(WHOAMI) $(WHOAMI_NND)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

$(OBJS):	$(HDRS)

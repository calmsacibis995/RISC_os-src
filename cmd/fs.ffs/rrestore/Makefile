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
# $Header: Makefile,v 1.3.1.2 90/05/09 16:00:47 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O1
INCLUDE	=
CFLAGS	=$(MAXOPT) -DRRESTORE $(INCLUDE) $(ECFLAGS) -DRISCOS -systype bsd43
ASFLAGS =$(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/etc

OBJS	=dirs.o interactive.o main.o restore.o symtab.o tape.o utilities.o \
	dumprmt.o

SRCS	=

HDRS	=restore.h

WHOAMI	=rrestore.ffs

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=4555
OWNER	=root
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

restore.h:
	[ -r restore.h ] || ln -s ../restore/restore.h .

dirs.o:		../restore/dirs.c
	$(CC) $(CFLAGS) -c ../restore/dirs.c

interactive.o:	../restore/interactive.c
	$(CC) $(CFLAGS) -c ../restore/interactive.c

main.o:		../restore/main.c
	$(CC) $(CFLAGS) -c ../restore/main.c

restore.o:	../restore/restore.c
	$(CC) $(CFLAGS) -c ../restore/restore.c

symtab.o:	../restore/symtab.c
	$(CC) $(CFLAGS) -c ../restore/symtab.c

tape.o:		../restore/tape.c
	$(CC) $(CFLAGS) -c ../restore/tape.c

utilities.o:	../restore/utilities.c
	$(CC) $(CFLAGS) -c ../restore/utilities.c

dumprmt.o:	../rdump/dumprmt.c
	$(CC) $(CFLAGS) -c ../rdump/dumprmt.c

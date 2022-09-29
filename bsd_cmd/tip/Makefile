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
# $Header: Makefile,v 1.2.2.3 90/05/07 19:38:47 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O1
INCLUDE	=
TIPDEFS =-DDEFBR=300 -DDEFFS=BUFSIZ -DMIPS -DPROTO_PKTSIZE -DDF02 -DDF03 \
	-DDN11 -DBIZ1031 -DBIZ1022 -DVENTEL -DV831 -DV3451 -DHAYES -DCDS \
	-DRISCOS
CFLAGS	=-systype bsd43 $(MAXOPT) $(INCLUDE) $(ECFLAGS) $(TIPDEFS)
ASFLAGS =-systype bsd43 $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=-lmld
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/bsd43/bin

SRCS	=acu.c acutab.c biz22.c biz31.c cds.c cmds.c cmdtab.c cu.c df.c dn11.c hayes.c hunt.c log.c partab.c protocol.c remcap.c remote.c tip.c tipout.c uucplock.c v3451.c v831.c value.c vars.c ventel.c clocal.c

OBJS	=$(SRCS:.c=.o)

HDRS	=protocol.h protoio.h tip.h

WHOAMI	=tip

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=4511
OWNER	=uucp
GROUP	=sys
ISTRIP	=-s
ICOPY	=-c
LINKS 	=-L $(DESTROOT)/bsd43/bin/cu 
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP) $(LINKS)

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
		-L /bsd43/bin/cu

$(OBJS):	$(HDRS)

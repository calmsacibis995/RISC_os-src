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
# $Header: Makefile,v 1.1.2.3 90/05/07 18:50:25 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=-systype bsd43 $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =-systype bsd43 $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/bsd43/bin

LPDSRCS	=common.c printcap.c rmjob.c startdaemon.c

LPDHDRS	=lp.h lp.local.h

OBJS	=common.o lprm.o printcap.o rmjob.o startdaemon.o

SRCS	=$(LPDSRCS) lprm.c

HDRS	=$(LPDHDRS)

WHOAMI	=lprm

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=6511
OWNER	=root
GROUP	=root
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
	-rm -f $(LPDSRCS) $(LPDHDRS)
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

$(OBJS):	$(HDRS)

$(LPDSRCS): ../lpd/$@
	[ ! -r $@ ] || rm -f $@
	ln -s ../lpd/$@ $@

$(LPDHDRS): ../lpd/$@
	[ ! -r $@ ] || rm -f $@
	ln -s ../lpd/$@ $@

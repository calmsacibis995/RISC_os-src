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
# $Header: Makefile,v 1.3.1.2 90/05/07 19:48:18 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=-systype sysv $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =-systype sysv $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/bsd43

OBJS	=regexp.o vfontedpr.o vgrindefs.o

SRCS	=regexp.c vfontedpr.c vgrindefs.c vgrind.sh vgrindefs.src tmac.vgrind

RSRCS=	retest.c regexp.c

ROBJS=	retest.o regexp.o

HDRS	=

WHOAMI	=vgrind

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)/bin
	$(INSTALL) $(ISTRIP) $(INSTOPTS) vfontedpr $(WHEREIGO)/usr/lib
	$(INSTALL) $(INSTOPTS) vgrindefs $(WHEREIGO)/usr/lib
	$(INSTALL) $(INSTOPTS) tmac.vgrind $(DESTROOT)/usr/lib/tmac

$(WHOAMI): retest vfontedpr vgrind.sh vgrindefs.src tmac.vgrind
	-rm -f $(WHOAMI)
	cp vgrind.sh $(WHOAMI)
	-rm -f vgrindefs
	cp vgrindefs.src vgrindefs

vfontedpr: $(OBJS) $(LOCAL_LIBS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LOCAL_LIBS) $(LIBRARIES)

retest: $(ROBJS) $(LOCAL_LIBS)
	$(CC) $(CFLAGS) -o $@ $(ROBJS) $(LOCAL_LIBS) $(LIBRARIES)

clean:
	-rm -f $(OBJS) $(ROBJS)

clobber: clean
	-rm -f vfontedpr retest vgrindefs $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

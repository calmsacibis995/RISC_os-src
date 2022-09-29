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
# $Header: Makefile,v 1.7.2.3 90/05/09 16:57:15 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O1
INCLUDE	=
CFLAGS	=$(MAXOPT) -DRISCOS -systype bsd43 -signed $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

#LIBRARIES=-lbsd
LIBRARIES=-lsec
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/usr/etc


OBJS	=ftpcmd.o ftpd.o glob.o

SRCS	=ftpcmd.y ftpd.c glob.c

HDRS	=

WHOAMI	=ftpd

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=554
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
LINK	=-l $(DESTROOT)/etc/ftpd
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP) $(LINK)

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
		-l $(DESTROOT)/etc/ftpd

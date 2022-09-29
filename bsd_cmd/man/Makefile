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
# $Header: Makefile,v 1.2.2.2 90/05/07 18:52:01 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=-systype bsd43 -DRISCOS $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =-systype bsd43 -DRISCOS $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)

OBJS	=man.o man-sysv.o

SRCS	=man.c

HDRS	=

WHOAMI	=man man-sysv

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) -L $(WHEREIGO)/bsd43/bin/whatis \
		-L $(WHEREIGO)/bsd43/bin/apropos \
		man $(WHEREIGO)/bsd43/bin 
	$(INSTALL) $(INSTOPTS) -f man-sysv $(WHEREIGO)/usr/bin/man

man: man.o $(LOCAL_LIBS)
	$(CC) $(CFLAGS) -o $@ man.o $(LOCAL_LIBS) $(LIBRARIES)

man-sysv: man-sysv.o $(LOCAL_LIBS)
	$(CC) $(CFLAGS) -o $@ man-sysv.o $(LOCAL_LIBS) $(LIBRARIES)

man-sysv.o: man.c
	$(CC) $(CFLAGS) -DMAN_SYSV_MODE -c -o $@ man.c

clean:
	-rm -f $(OBJS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

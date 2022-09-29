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
# $Header: Makefile,v 1.1.2.2 90/05/09 17:11:02 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc
INC	=$(DESTROOT)/usr/include/bsd43

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=$(MAXOPT) -DRISCOS -systype bsd43 $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/usr/etc

SRCS=	nsquery.c nstest.c
OBJS=	nsquery.o nstest.o

WHOAMI	=nsquery nstest

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
LINK	=
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP) $(LINK)

all: $(WHOAMI)
	cd nslookup; $(MAKE) $(MAKEARGS) $@

install:
	cd nslookup; $(MAKE) $(MAKEARGS) $@

clean:
	-rm -f $(OBJS)
	cd nslookup; $(MAKE) $(MAKEARGS) $@

clobber: clean
	-rm -f $(WHOAMI)
	cd nslookup; $(MAKE) $(MAKEARGS) $@

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO) -l /etc/named

nsquery: nsquery.o
	$(CC) $(CFLAGS) -o $@ $@.o $(LOCAL_LIBS) $(LIBRARIES)

# test programs and utilities
nstest: nstest.o
	@echo "The resolver routines in libc must be compiled with the -DDEBUG"
	@echo "option or nstest will not have any output."
	$(CC) $(CFLAGS) -o $@ $@.o $(LOCAL_LIBS) $(LIBRARIES)

nsquery: nsquery.c $(INC)/stdio.h $(INC)/sys/types.h
nsquery: $(INC)/arpa/nameser.h $(INC)/netdb.h
nsquery: $(INC)/sys/socket.h $(INC)/netinet/in.h
nsquery: $(INC)/resolv.h
nstest: nstest.c $(INC)/sys/types.h $(INC)/sys/socket.h
nstest: $(INC)/netinet/in.h $(INC)/stdio.h
nstest: $(INC)/arpa/nameser.h $(INC)/resolv.h

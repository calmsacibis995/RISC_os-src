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
# $Header: Makefile,v 1.5.1.2 90/05/07 20:45:26 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=-systype bsd43 $(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =-systype bsd43 $(GNUM) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

OBJS	=ht_getnamadr.o ht_service.o ht_gnbyaddr.o ht_gnbyname.o ht_gnetent.o \
	 ht_gngrent.o ht_innetgr.o ht_getproto.o ht_getpent.o ht_getpname.o \
	 ht_grpcent.o ht_gservent.o ht_gethent.o ht_getgrent.o ht_getgrnam.o \
	 ht_getgrgid.o ht_getpwent.o

SRCS	=ht_getnamadr.c ht_service.c ht_gnbyaddr.c ht_gnbyname.c ht_gnetent.c \
	 ht_gngrent.c ht_innetgr.c ht_getproto.c ht_getpent.c ht_getpname.c \
	 ht_grpcent.c ht_gservent.c ht_gethent.c ht_getgrent.c ht_getgrnam.c \
	 ht_getgrgid.c ht_getpwent.c

HDRS	=

WHOAMI	=libc_hosttable.port.a
INTEGDIR=../libc

all install: $(INTEGDIR)/$(WHOAMI)

$(INTEGDIR)/$(WHOAMI): $(WHOAMI)
	-rm -f $(INTEGDIR)/$(WHOAMI)
	cp $(WHOAMI) $(INTEGDIR)

$(WHOAMI): $(OBJS)
	-rm -f $(WHOAMI)
	$(AR) $(ARFLAGS) $(WHOAMI) $(OBJS)

clean:
	-rm -f $(OBJS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(INTEGDIR)

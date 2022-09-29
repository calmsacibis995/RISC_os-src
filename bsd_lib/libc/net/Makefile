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
# $Header: Makefile,v 1.11.1.2 90/05/07 20:50:48 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=-systype bsd43 -DRISCOS $(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =-systype bsd43 $(GNUM) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

OBJS	=bindresvport.o ether_addr.o getnetent.o getproto.o getprotoent.o \
	getprotoname.o getrpcent.o getservent.o herror.o \
	rcmd.o res_comp.o getnetgrent.o \
	res_debug.o res_init.o res_mkquery.o res_send.o rexec.o \
	ruserpass.o getnetbyaddr.o getnetbyname.o res_query.o \
	innetgr.o vis.o gethostnamadr.o sethostent.o

SRCS	=bindresvport.c ether_addr.c getnetent.c getproto.c getprotoent.c \
	getprotoname.c getrpcent.c getservent.c herror.c \
	rcmd.c res_comp.c getnetgrent.c \
	res_debug.c res_init.c res_mkquery.c res_send.c rexec.c \
	ruserpass.c getnetbyaddr.c getnetbyname.c res_query.c \
	innetgr.c vis.c gethostnamadr.c sethostent.c

HDRS	=

WHOAMI	=libc_net.port.a
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

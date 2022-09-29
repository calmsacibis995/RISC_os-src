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
# $Header: Makefile,v 1.1.1.3 90/05/10 01:22:54 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=-DRISCOS $(GNUM) -systype sysv $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(GNUM) -systype sysv $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

OBJS	=des_crypt.o des_soft.o 

SRCS	=des_crypt.c des_soft.c

HDRS	=des_crypt.h softdes.h

BSD43STUFF = $(SRCS)

WHOAMI	=libc_des.port.a
INTEGDIR=../libc

all install: $(INTEGDIR)/$(WHOAMI)

install-headers:
	[ ! -r softdes.h ] || rm -f softdes.h
	ln -s ../../../bsd_lib/libc/des/softdes.h softdes.h
	[ ! -r softdesdata.c ] || rm -f softdesdata.c
	ln -s ../../../bsd_lib/libc/des/softdesdata.c softdesdata.c

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

$(BSD43STUFF): ../../../bsd_lib/libc/des/$@
	[ ! -r $@ ] || rm -f $@
	ln -s ../../../bsd_lib/libc/des/$@ $@

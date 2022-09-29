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
# $Header: Makefile,v 1.2.1.3 90/05/10 04:11:14 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=-I../include
CFLAGS	=-systype posix $(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =-systype posix $(GNUM) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

OBJS	=cerror.o

SYSVCRTMIPSSRCS	=cerror.s

SYSVSRCS=$(SYSVCRTMIPSSRCS)

HDRS	=

WHOAMI	=libc_crt.mips.a
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
	-rm -f $(SYSVSRCS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(INTEGDIR)

$(SYSVSRCS): ../../../lib/libc/crt.mips/$@
	[ ! -r $@ ] || rm -f $@
	ln -s ../../../lib/libc/crt.mips/$@ $@

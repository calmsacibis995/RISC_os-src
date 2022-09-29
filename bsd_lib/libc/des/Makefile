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
# $Header: Makefile,v 1.4.1.2 90/05/07 20:27:34 wje Exp $
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

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

OBJS	=des_crypt.o des_soft.o 

SRCS	=des_crypt.c des_soft.c

HDRS	=des_crypt.h softdes.h

WHOAMI	=libc_des.port.a
INTEGDIR=../libc

all install: $(INTEGDIR)/$(WHOAMI)

install-headers:
	$(INSTALL) $(INSTOPTS) -l $(DESTROOT)/bsd43/usr/include/des_crypt.h des_crypt.h $(DESTROOT)/usr/include
	[ -r $(DESTROOT)/bsd43/usr/include/des ] || \
		( ( mkdir -p $(DESTROOT)/bsd43/usr/include/des || exit 0) ; \
		  	( chgrp bin $(DESTROOT)/bsd43/usr/include/des ; \
			  chmod 0775 $(DESTROOT)/bsd43/usr/include/des ; \
			  chown bin $(DESTROOT)/bsd43/usr/include/des ) ) ; 
	$(INSTALL) $(INSTOPTS) -l $(DESTROOT)/bsd43/usr/include/des/des.h des.h $(DESTROOT)/usr/include/des
	$(INSTALL) $(INSTOPTS) -l $(DESTROOT)/bsd43/usr/include/des/softdes.h softdes.h $(DESTROOT)/usr/include/des
	$(INSTALL) $(INSTOPTS) -l $(DESTROOT)/bsd43/usr/include/des/softdesdata.c softdesdata.c $(DESTROOT)/usr/include/des

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

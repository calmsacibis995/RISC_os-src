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
# $Header: Makefile,v 1.5.1.2 90/05/07 21:28:52 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=-systype bsd43 $(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS) -DRISCOS
ASFLAGS =-systype bsd43 $(GNUM) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

HDRWHEREIGO=$(DESTROOT)/usr/include/rpcsvc
BSDHDR=$(DESTROOT)/bsd43/usr/include/rpcsvc

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)
DEBUG	=


OBJS	=yp_all.o yp_bind.o yp_enum.o yp_master.o yp_match.o yp_order.o \
	 yp_update.o yperr_string.o ypmaint_xdr.o ypprot_err.o ypv1_xdr.o \
	 ypxdr.o ypupdate_xdr.o yp_service.o yp_getgrent.o yp_getpwent.o \
	 yp_getrpcent.o yp_getservent.o yp_getpent.o yp_gnetgr.o yp_innetgr.o \
	 yp_gethostent.o yp_gnetent.o

SRCS	=yp_all.c yp_bind.c yp_enum.c yp_master.c yp_match.c yp_order.c \
	 yp_update.c yperr_string.c ypmaint_xdr.c ypprot_err.c ypv1_xdr.c \
	 ypxdr.c ypupdate_xdr.c yp_service.c yp_getgrent.c yp_getpwent.c \
	 yp_getrpcent.c yp_getservent.c yp_getpent.o yp_gnetgr.c yp_innetgr.c \
	 yp_gethostent.c yp_tnetent.c

HDRS	=yp_prot.h ypclnt.h ypupdate_prot.h ypv1_prot.h

LIBSUFFIX=
WHOAMI	=libc.port.yp$(LIBSUFFIX).a

WHOAMI	=libc_yp.port.a
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
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

$(OBJS):	$(HDRS)

install-headers: $(HDRS)
	$(INSTALL) $(INSTOPTS) -l $(BSDHDR)/yp_prot.h yp_prot.h $(HDRWHEREIGO)
	$(INSTALL) $(INSTOPTS) -l $(BSDHDR)/ypclnt.h ypclnt.h $(HDRWHEREIGO)
	$(INSTALL) $(INSTOPTS) -l $(BSDHDR)/ypupdate_prot.h ypupdate_prot.h $(HDRWHEREIGO)
	$(INSTALL) $(INSTOPTS) -l $(BSDHDR)/ypv1_prot.h ypv1_prot.h $(HDRWHEREIGO)

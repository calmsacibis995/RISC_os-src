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
# $Header: Makefile,v 1.3.1.3 90/05/07 20:55:03 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=-systype bsd43 $(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS) -DRISCOS
ASFLAGS =-systype bsd43 $(GNUM) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

HDRWHEREIGO=$(DESTROOT)/usr/include/rpc
BSDHDR=$(DESTROOT)/bsd43/usr/include/rpc

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

SRCS	=auth_des.c auth_none.c auth_unix.c authdes_prot.c \
	authunix_prot.c clnt_generic.c \
	clnt_perror.c clnt_raw.c clnt_simple.c clnt_tcp.c clnt_udp.c \
	get_myaddress.c key_call.c key_prot.c netname.c \
	openchild.c pmap_clnt.c pmap_getmaps.c pmap_getport.c \
	pmap_prot.c pmap_prot2.c pmap_rmt.c rpc_callmsg.c rpc_prot.c \
	rpccommondata.c rpcdtablesize.c rtime.c \
	svc.c svc_auth.c svc_auth_unix.c svc_raw.c svc_run.c \
	svc_simple.c svc_tcp.c svc_udp.c svcauth_des.c \
	xdr.c xdr_array.c xdr_float.c xdr_mem.c \
	xdr_rec.c xdr_reference.c xdr_stdio.c

OBJS	=auth_des.o auth_none.o auth_unix.o authdes_prot.o \
	authunix_prot.o clnt_generic.o \
	clnt_perror.o clnt_raw.o clnt_simple.o clnt_tcp.o clnt_udp.o \
	get_myaddress.o key_call.o key_prot.o netname.o \
	openchild.o pmap_clnt.o pmap_getmaps.o pmap_getport.o \
	pmap_prot.o pmap_prot2.o pmap_rmt.o rpc_callmsg.o rpc_prot.o \
	rpccommondata.o rpcdtablesize.o rtime.o \
	svc.o svc_auth.o svc_auth_unix.o svc_raw.o svc_run.o \
	svc_simple.o svc_tcp.o svc_udp.o svcauth_des.o \
	xdr.o xdr_array.o xdr_float.o xdr_mem.o \
	xdr_rec.o xdr_reference.o xdr_stdio.o

HDRS	=auth.h auth_des.h auth_unix.h clnt.h key_prot.h pmap_clnt.h \
	pmap_prot.h pmap_rmt.h rpc.h rpc_msg.h svc.h svc_auth.h types.h xdr.h

WHOAMI	=libc_rpc.port.a
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

install-headers: $(HDRS)
	$(INSTALL) $(INSTOPTS) -l $(BSDHDR)/auth.h auth.h $(HDRWHEREIGO)
	$(INSTALL) $(INSTOPTS) -l $(BSDHDR)/auth_des.h auth_des.h $(HDRWHEREIGO)
	$(INSTALL) $(INSTOPTS) -l $(BSDHDR)/auth_unix.h auth_unix.h $(HDRWHEREIGO)
	$(INSTALL) $(INSTOPTS) -l $(BSDHDR)/clnt.h clnt.h $(HDRWHEREIGO)
	$(INSTALL) $(INSTOPTS) -l $(BSDHDR)/key_prot.h key_prot.h $(HDRWHEREIGO)
	$(INSTALL) $(INSTOPTS) -l $(BSDHDR)/pmap_clnt.h pmap_clnt.h $(HDRWHEREIGO)
	$(INSTALL) $(INSTOPTS) -l $(BSDHDR)/pmap_prot.h pmap_prot.h $(HDRWHEREIGO)
	$(INSTALL) $(INSTOPTS) -l $(BSDHDR)/pmap_rmt.h pmap_rmt.h $(HDRWHEREIGO)
	$(INSTALL) $(INSTOPTS) -l $(BSDHDR)/rpc.h rpc.h $(HDRWHEREIGO)
	$(INSTALL) $(INSTOPTS) -l $(BSDHDR)/rpc_msg.h rpc_msg.h $(HDRWHEREIGO)
	$(INSTALL) $(INSTOPTS) -l $(BSDHDR)/svc.h svc.h $(HDRWHEREIGO)
	$(INSTALL) $(INSTOPTS) -l $(BSDHDR)/svc_auth.h svc_auth.h $(HDRWHEREIGO)
	$(INSTALL) $(INSTOPTS) -l $(BSDHDR)/xdr.h xdr.h $(HDRWHEREIGO)
	$(INSTALL) $(INSTOPTS) -l $(BSDHDR)/types.h types.h $(HDRWHEREIGO)

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
# $Header: Makefile,v 1.2.1.3 90/05/10 05:39:53 wje Exp $
#$Header: Makefile,v 1.2.1.3 90/05/10 05:39:53 wje Exp $
DEPTH	=../..
LABEL	=klib_rpc
include $(DEPTH)/kcommondefs

HDRS	=auth.h auth_des.h auth_unix.h clnt.h key_prot.h pmap_clnt.h \
	pmap_prot.h pmap_rmt.h rpc.h rpc_msg.h svc.h svc_auth.h \
	types.h xdr.h

SRCS   =auth_des.c auth_kern.c auth_unix.c authdes_prot.c \
        authdes_subr.c authunix_prot.c clnt_perror.c clnt_kudp.c \
        key_call.c key_prot.c kudp_fastsend.c \
	pmap_kgetport.c pmap_prot.c pmap_rmt.c \
	rpc_callmsg.c rpc_prot.c subr_kudp.c \
	svc.c svc_auth.c svc_auth_unix.c svc_kudp.c svcauth_des.c \
	xdr.c xdr_array.c xdr_float.c xdr_mbuf.c xdr_mem.c xdr_reference.c 

LIBNAME	=$(BOOTAREA)/librpc.a

default:	all

rules:	$(MAKERULES)

$(MAKERULES):	$(MAKERULESMARK)

$(MAKERULESMARK): Makefile
	touch $(MAKERULESMARK) ; \
	rm -rf $(MKRULESTMP) ; \
	$(MKRULES) '$$(BOOTAREA)' '$$(CC) $$(CFLAGS)' OBJS $(SRCS) \
		> $(MKRULESTMP) && \
	mv $(MKRULESTMP) $(MAKERULES)

include $(MAKEDEPEND)

include $(MAKERULES)

LIBOBJS=$(OBJS)

all: $(LIBMARK)

$(SRCS): link-mark

source-links: link-mark

link-mark: Makefile
	for i in $(HDRS) $(SRCS) ;  \
	{ \
		rm -rf $$i ; \
		ln -s ../../../../bsd_lib/libc/rpc/$$i $$i ; \
	} ;
	touch link-mark

clean:
	-rm -f $(OBJS) $(HDRS) $(SRCS) link-mark

clobber: clean 

include $(KCOMMONRULES)

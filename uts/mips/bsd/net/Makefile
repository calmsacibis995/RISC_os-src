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
# $Header: Makefile,v 1.17.1.3 90/05/10 04:24:23 wje Exp $
#ident	"$Header: Makefile,v 1.17.1.3 90/05/10 04:24:23 wje Exp $"

DEPTH	=../..
LABEL	=bsd_net
include $(DEPTH)/kcommondefs

LIBNAME = $(BOOTAREA)/bsd.a
.PRECIOUS:$(LIBNAME)

#
# Special Flags for cc to allow BSD kernel code to compile.
#
INCLUDE= $(DESTROOT)/usr/include
BSD_FLAGS= -signed

CSRCS	=af.c if.c if_loop.c netisr.c netman.c raw_cb.c raw_usrreq.c route.c

SRCS=$(CSRCS)

default:	all

rules:	$(MAKERULES)

$(MAKERULES):	$(MAKERULESMARK)

$(MAKERULESMARK): Makefile
	touch $(MAKERULESMARK) ; \
	rm -rf $(MKRULESTMP) ; \
	$(MKRULES) '$$(BOOTAREA)' '$$(CC) $$(CFLAGS)' FILES $(CSRCS) \
		> $(MKRULESTMP) && \
	mv $(MKRULESTMP) $(MAKERULES)

include $(MAKEDEPEND)

include $(MAKERULES)

LIBOBJS=$(FILES)

all:$(LIBMARK)

include $(KCOMMONRULES)



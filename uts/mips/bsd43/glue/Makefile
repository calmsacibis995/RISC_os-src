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
# $Header: Makefile,v 1.3.1.3 90/05/10 04:38:56 wje Exp $
#ident	"$Header: Makefile,v 1.3.1.3 90/05/10 04:38:56 wje Exp $"

DEPTH	=../..
LABEL	=bsd43_glue
include $(DEPTH)/kcommondefs

LIBNAME = $(BOOTAREA)/bsd43.a
.PRECIOUS:$(LIBNAME)

CSRCS	= bsd_sysent.c misc.c kern_prot.c kern_proc.c \
	  bsd_ioctl.c bsd_sysmips.c kern_resource.c kern_sig.c

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

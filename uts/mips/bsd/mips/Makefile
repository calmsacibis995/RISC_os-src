# |-----------------------------------------------------------|
# | Copyright (c) 1989       MIPS Computer Systems, Inc.      |
# | All Rights Reserved                                       |
# |-----------------------------------------------------------|
# | Use, duplication, or disclosure by the Government is      |
# | subject to restrictions as set forth in                   |
# | subparagraph (c)(1)(ii) of the Rights in Technical        |
# | Data and Computer Software Clause of DFARS 52.227-7013.   |
# |         MIPS Computer Systems, Inc.                       |
# |         928 Arques Avenue                                 |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
#ident	"$Header: Makefile,v 1.15.1.5.1.2 90/07/11 18:25:57 hawkes Exp $"

# make mips-specific TCP stuff.

DEPTH	=../..
LABEL	=bsd_mips
include $(DEPTH)/kcommondefs

LIBNAME = $(BOOTAREA)/bsd.a
.PRECIOUS:$(LIBNAME)

#
# Special Flags for cc to allow BSD kernel code to compile.
#
INCLUDE= $(DESTROOT)/usr/include
BSD_FLAGS=-I$(INCLUDE)/bsd/net -I$(INCLUDE)/bsd

# sources for the bsd library
SSRCS	=cksum.s
CSRCS	=hostname.c kern_mbuf.c in_cksum.c remque.c bsdinit.c

# 'interfaces'
IFSRCS	=if_en.c if_enp.c if_lance.c if_la.c if_lacomm.c if_la_rb3125.c \
	if_egl.c

SRCS= $(CSRCS) $(SSRCS) 
DRVSRCS= $(IFSRCS)

default:	all

rules:	$(MAKERULES)

$(MAKERULES):	$(MAKERULESMARK)

$(MAKERULESMARK): Makefile
	touch $(MAKERULESMARK) ; \
	rm -rf $(MKRULESTMP) ; \
	$(MKRULES) '$$(BOOTAREA)' '$$(CC) $$(CFLAGS)' OBJS $(CSRCS) $(SSRCS) \
		> $(MKRULESTMP) && \
	$(MKRULESDRV) '$$(BOOTAREA)' '$$(CC) $$(CFLAGS)' IFOBJS $(IFSRCS) \
		>> $(MKRULESTMP) && \
	mv $(MKRULESTMP) $(MAKERULES)

include $(MAKEDEPEND)

include $(MAKERULES)

LIBOBJS = $(OBJS)
DRVOBJS = $(IFOBJS)

all: $(LIBMARK) $(DRVMARK)

$(SSRCS) $(CSRCS):

include $(KCOMMONRULES)



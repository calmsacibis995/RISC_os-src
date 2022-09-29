#
#ident	"$Header: Makefile,v 1.2.1.2 89/12/08 18:02:16 wje Exp $"
#
# Notes: Makefile for the Berkeley BSD 4.3 fast file system (FFS)
#

#
# Common Definitions
#
DEPTH=../..
LABEL=fs_specfs
include $(DEPTH)/kcommondefs

#
# Compile Time Options
#
BSD_FLAGS= -signed -I../../../../bsd_lib/libc/klm

#
# Local Definitions
#

CSRCS=	fifo_vnodeops.c spec_clone.c spec_subr.c spec_vfsops.c spec_vnodeops.c

SRCS=$(CSRCS)
MODULE=$(BOOTAREA)/specfs.o

#
# Targets/Rules
#

default:	all

rules:	$(MAKERULES)

$(MAKERULES):	$(MAKERULESMARK)

$(MAKERULESMARK): Makefile
	touch $(MAKERULESMARK) ; \
	rm -rf $(MKRULESTMP) ; \
	$(MKRULES) '$$(BOOTAREA)' '$$(CC) $$(CFLAGS)' OBJS $(CSRCS) \
		> $(MKRULESTMP) && \
	mv $(MKRULESTMP) $(MAKERULES)

include $(MAKEDEPEND)

include $(MAKERULES)

all:	$(MODULE)

include $(KCOMMONRULES)

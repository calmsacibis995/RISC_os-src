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
# $Header: Makefile,v 1.2.1.3 90/05/10 05:05:02 wje Exp $
#
#ident	"$Header: Makefile,v 1.2.1.3 90/05/10 05:05:02 wje Exp $"
#

#
# Common Definitions
#
DEPTH=../..
LABEL=fs_proc
include $(DEPTH)/kcommondefs

#
# Local Definitions
#

CSRCS	= proc_vfsops.c proc_vnodeops.c proc_subr.c

SRCS=$(CSRCS)
MODULE=$(BOOTAREA)/proc.o

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

all: $(MODULE)

include $(KCOMMONRULES)



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
# $Header: Makefile,v 1.2.1.3 90/05/10 05:07:05 wje Exp $
#
#ident	"$Header: Makefile,v 1.2.1.3 90/05/10 05:07:05 wje Exp $"
#
# Notes: Makefile for the Berkeley BSD 4.3 fast file system (FFS)
#

#
# Common Definitions
#
DEPTH=../..
LABEL=fs_ufs
include $(DEPTH)/kcommondefs

#
# Compile Time Options
#
BSD_FLAGS= -signed -I../../../../bsd_lib/libc/klm

#
# Local Definitions
#

CSRCS=	quota.c quota_syscalls.c quota_ufs.c \
	ufs_alloc.c ufs_bmap.c ufs_dir.c ufs_dsort.c ufs_inode.c \
	ufs_subr.c ufs_tables.c ufs_vnodeops.c ufs_vfsops.c

SRCS=$(CSRCS)
MODULE=$(BOOTAREA)/ufs.o
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

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
# $Header: Makefile,v 1.22.1.6 90/05/10 20:43:24 wje Exp $
#ident	"$Header: Makefile,v 1.22.1.6 90/05/10 20:43:24 wje Exp $"

DEPTH=..
LABEL=os
SRCDIR=$(DEPTH)/os
include $(DEPTH)/kcommondefs

LIBNAME =$(BOOTAREA)/lib.os

.PRECIOUS:$(LIBNAME)

# sources whose .o's live up one elsewhere 
DDCSRCS	=disp.c

CSRCS	= \
	acct.c \
	bitmasks.c \
	cache.c \
	clock.c \
	exec.c \
	exit.c \
	fault.c \
	fio.c \
	flock.c \
	fork.c \
	fstyp.c \
	getpages.c \
	getsizes.c \
	grow.c \
	kern_time.c \
	kmem.c \
	lock.c \
	machdep.c \
	main.c \
	malloc.c \
	mmap.c \
	move.c \
	page.c \
	physio.c \
	pipe.c \
	prf.c \
	probe.c \
	region.c \
	sched.c \
	sig.c \
	slp.c \
	space.c \
	startup.c \
	streamio.c \
	strcall.c \
	strcallsub.c \
	subr.c \
	swapalloc.c \
	swtch.c \
	sys2.c \
	sys3.c \
	sys4.c \
	sysent.c \
	sysmips.c \
	text.c \
	trap.c \
	utssys.c \
	leds.c \
	check_dbg.c \
	debug.c \
	mem_r3030.c
NSRCS	= \
	vfs.c \
	vfs_bio.c \
	vfs_conf.c \
	vfs_dev.c \
	vfs_dnlc.c \
	vfs_io.c \
	vfs_lookup.c \
	vfs_pathname.c \
	vfs_syscalls.c \
	vfs_vnode.c \
	vfs_xxx.c

default:	all

rules:	$(MAKERULES)

os-libs:
	$(MAKE) $(MAKEARGS) $(LIBNAME)

$(MAKERULES):	$(MAKERULESMARK)

$(MAKERULESMARK): Makefile
	touch $(MAKERULESMARK) ; \
	rm -rf $(MKRULESTMP) ; \
	$(MKRULES) '$$(BOOTAREA)' '$$(CC) $$(CFLAGS)' COBJS $(CSRCS) \
		> $(MKRULESTMP) && \
	$(MKRULES) '$$(BOOTAREA)' '$$(CC) -signed $$(CFLAGS)' NOBJS $(NSRCS) \
		>> $(MKRULESTMP) && \
	$(MKRULESDRV) '$$(BOOTAREA)' '$$(CC) $$(CFLAGS)' DDOBJS $(DDCSRCS) \
		>> $(MKRULESTMP) && \
	mv $(MKRULESTMP) $(MAKERULES)

include $(MAKEDEPEND)

include $(MAKERULES)

OBJS	=${COBJS} $(NOBJS)

SRCS	=${CSRCS} $(NSRCS)
DRVSRCS =$(DDCSRCS)
LIBOBJS=$(OBJS)

DRVOBJS=$(DDOBJS)

all:$(LIBMARK) $(DRVMARK)

include $(KCOMMONRULES)


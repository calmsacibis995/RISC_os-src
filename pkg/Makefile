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
# $Header: Makefile,v 1.7.1.2 90/05/10 03:42:18 wje Exp $
#
SHELL	=/bin/sh
MAKE	=/bin/make
SUBDIRS	= ask  \
	bom_to_tar \
	bomerge \
	bomit \
	comply \
	dates \
	devstat \
	extra \
	findmods \
	inst \
	inst_abort \
	inst_bsd_sysv \
	inst_clnup \
	inst_compl \
	inst_conv \
	inst_date \
	inst_env \
	inst_mount \
	inst_mkdev \
	inst_mkstuff \
	inst_netup \
	inst_newfs \
	inst_presv \
	inst_restr \
	inst_rmold \
	inst_sing \
	inst_space \
	inst_start \
	inst_stpln \
	inst_subsl \
	inst_swprq \
	inst_xtr \
	mkdevcmd \
	mksizes \
	netfeed \
	pkg \
	pkg_boms \
	pkg_comply \
	pkg_env \
	pkg_instd \
	pkg_miniroot \
	pkg_cpinstd \
	pkg_sizes \
	pkg_tapes \
	pkg_tapevol \
	pkginfo \
	cleannew \
	convttys \
	diskmap \
	filtfstab \
	getmem \
	preserve \
	rmpkg \
	space \
	stripln

all install clobber clean generic:
	@for dir in $(SUBDIRS) ;\
	{\
		if [ -d "$$dir" ] ;\
		then \
			echo "XXXXXXX $$dir XXXXXXX" ;\
			( cd $$dir ; $(MAKE) $(MAKEARGS) $@ ) ;\
		else \
			echo "******* No directory $$dir" ;\
		fi ;\
	}

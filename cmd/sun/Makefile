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
# |         950 DeGuigne Drive                                |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
#
# $Header: Makefile,v 1.6.1.6.1.1.1.2 90/10/05 10:06:50 beacker Exp $
#

SHELL	=/bin/sh

SUBDIRS	= \
	automount \
	biod \
	bootp \
	chkey \
	domainname \
	exportfs \
	havenfs \
	keylogin \
	keyserv \
	mkfile \
	mount \
	nfsd \
	nfsstat \
	pcnfsd \
	portmap \
	rexd \
	rpc.bootparamd \
	rpc.lockd \
	rpc.mountd \
	rpc.passwd \
	rpc.rquotad \
	rpc.rstatd \
	rpc.ruserd \
	rpc.rwalld \
	rpc.sprayd \
	rpc.statd \
	rpc.yppasswdd \
	rpc.ypupdated \
	rpcgen \
	rpcinfo \
	rup \
	rusers \
	rwall \
	showmount \
	spray \
	umount \
	yp

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

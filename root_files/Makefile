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
# $Header: Makefile,v 1.4.2.4 90/05/10 04:19:44 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

WHEREIGO=$(DESTROOT)

DIRS = \
	etc \
	etc/save.d \
	etc/zoneinfo \
	etc/zoneinfo/SOURCES \
	usr \
	usr/etc \
	usr/lib \
	usr/lib/cf \
	usr/lib/cf/cf \
	usr/lib/cf/m4 \
	usr/lib/cf/sitedep \
	usr/lib/cron \
	usr/lib/lex \
	usr/lib/uucp \
	usr/spool/cron/crontabs \
	usr/spool/lp

FILES = \
	.cshrc \
	.login \
	.profile \
	etc/TIMEZONE \
	etc/TIMEZONE.csh \
	etc/TZ \
	etc/automount.conf.sample \
	etc/bupsched \
	etc/coredirs \
	etc/cshrc \
	etc/disktab \
	etc/dumpdates \
	etc/fstab \
	etc/gettydefs \
	etc/group \
	etc/hosts \
	etc/hosts.slip \
	etc/inittab \
	etc/ioctl.syscon \
	etc/local_hostname \
	etc/lockd.conf.sample \
	etc/magic \
	etc/motd \
	etc/mtab \
	etc/networks \
	etc/passwd \
	etc/profile \
	etc/protocols \
	etc/publickey \
	etc/remote \
	etc/save.d/except \
	etc/savecpio \
	etc/services \
	etc/shells \
	etc/stdcshrc \
	etc/stdprofile \
	etc/sysdirlist \
	etc/syslist \
	etc/syslog.conf \
	etc/termcap \
	etc/ttytype \
	etc/zoneinfo/SOURCES/asia \
	etc/zoneinfo/SOURCES/australasia \
	etc/zoneinfo/SOURCES/etcetera \
	etc/zoneinfo/SOURCES/europe \
	etc/zoneinfo/SOURCES/northamerica \
	etc/zoneinfo/SOURCES/pacificnew \
	etc/zoneinfo/SOURCES/systemv \
	etc/zoneinfo/uncompiled \
	usr/etc/bootptab \
	usr/etc/ethers \
	usr/etc/exports \
	usr/etc/gated.conf.sample \
	usr/etc/inetd.conf \
	usr/etc/netgroup \
	usr/etc/routed.conf \
	usr/etc/rpc \
	usr/etc/timed.conf \
	usr/lib/aliases \
	usr/lib/cf/KEY \
	usr/lib/cf/M4_KEY \
	usr/lib/cf/README \
	usr/lib/cf/cf/Makefile \
	usr/lib/cf/cf/README \
	usr/lib/cf/cf/cad.mc \
	usr/lib/cf/cf/cadgroup.mc \
	usr/lib/cf/cf/cc.mc \
	usr/lib/cf/cf/cchem.mc \
	usr/lib/cf/cf/cogsci.mc \
	usr/lib/cf/cf/gen.mc \
	usr/lib/cf/cf/ic.mc \
	usr/lib/cf/cf/okeeffe.mc \
	usr/lib/cf/cf/proto.mc \
	usr/lib/cf/cf/tcpproto.cf \
	usr/lib/cf/cf/tcpproto.mc \
	usr/lib/cf/cf/tcpuucpproto.cf \
	usr/lib/cf/cf/tcpuucpproto.mc \
	usr/lib/cf/cf/ucbarpa.mc \
	usr/lib/cf/cf/ucbtcp.mc \
	usr/lib/cf/cf/ucbvax.mc \
	usr/lib/cf/cf/uucpproto.cf \
	usr/lib/cf/cf/uucpproto.mc \
	usr/lib/cf/m4/README \
	usr/lib/cf/m4/boilerplate.m4 \
	usr/lib/cf/m4/fake_domains.m4 \
	usr/lib/cf/m4/localm.m4 \
	usr/lib/cf/m4/nsclasses.m4 \
	usr/lib/cf/m4/nsmacros.m4 \
	usr/lib/cf/m4/nstcpldm.m4 \
	usr/lib/cf/m4/nstcpm.m4 \
	usr/lib/cf/m4/postwriterule.m4 \
	usr/lib/cf/m4/prewriterule.m4 \
	usr/lib/cf/m4/rule0.m4 \
	usr/lib/cf/m4/rule3.m4 \
	usr/lib/cf/m4/rule5.m4 \
	usr/lib/cf/m4/smtpuucpm.m4 \
	usr/lib/cf/m4/suucpm.m4 \
	usr/lib/cf/m4/uucpm.m4 \
	usr/lib/cf/m4/version.m4 \
	usr/lib/cf/sitedep/README \
	usr/lib/cf/sitedep/nicregistered.m4 \
	usr/lib/cf/sitedep/smtpuucp.cad.m4 \
	usr/lib/cf/sitedep/smtpuucp.ucbvax.m4 \
	usr/lib/cf/sitedep/uucp.cad.m4 \
	usr/lib/cf/sitedep/uucp.cogsci.m4 \
	usr/lib/cf/sitedep/uucp.ic.m4 \
	usr/lib/cf/sitedep/uucp.okeeffe.m4 \
	usr/lib/cf/sitedep/uucp.proto.m4 \
	usr/lib/cf/sitedep/uucp.ucbarpa.m4 \
	usr/lib/cf/sitedep/uucp.ucbvax.m4 \
	usr/lib/cron/.proto \
	usr/lib/cron/queuedefs \
	usr/lib/ex.rc \
	usr/lib/lex/ncform \
	usr/lib/lex/nrform \
	usr/lib/more.help \
	usr/lib/sendmail.cf \
	usr/lib/sendmail.hf \
	usr/lib/unittab \
	usr/lib/uucp/Devconfig \
	usr/lib/uucp/Devices \
	usr/lib/uucp/Dialcodes \
	usr/lib/uucp/Dialers \
	usr/lib/uucp/Permissions \
	usr/lib/uucp/Poll \
	usr/lib/uucp/Sysfiles \
	usr/lib/uucp/Systems \
	usr/spool/cron/crontabs/adm \
	usr/spool/cron/crontabs/periodic \
	usr/spool/cron/crontabs/root \
	usr/spool/cron/crontabs/sys \
	usr/spool/cron/crontabs/sysadm \
	usr/spool/cron/crontabs/uucp \
	usr/spool/lp/pstatus \
	usr/spool/lp/qstatus

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
DIRMODE	=775
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

all clean clobber:
	@echo "Nothing to do"

install:
	ID=`id +%U` ; \
	for dir in $(DIRS) ;\
	{\
		[ -r $(WHEREIGO)/$$dir ] || \
			( ( mkdir -p $(WHEREIGO)/$$dir || exit 0) ; \
			  [ $$ID != 0 ] || \
			  ( chgrp $(GROUP) $(WHEREIGO)/$$dir ; \
			    chmod $(DIRMODE) $(WHEREIGO)/$$dir ; \
			    chown $(OWNER) $(WHEREIGO)/$$dir ) ) ; \
	}
	for file in $(FILES) ;\
	{\
		DIRN=`dirname $$file` ; \
		FILEN=`basename $$file` ; \
		$(INSTALL) $(INSTOPTS) $$file $(WHEREIGO)/$$DIRN || true; \
	}


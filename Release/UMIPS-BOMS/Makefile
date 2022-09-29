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
# $Header: Makefile,v 1.4.1.13.1.3.1.2 90/10/17 17:57:10 beacker Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

OSRELEASE=4.52
WHEREIGO=$(DESTROOT)/usr/pkg/lib/umips$(OSRELEASE)
WHEREIGOSRC=$(WHEREIGO)-src
WHEREIGOOEMSRC=$(WHEREIGO)-oem-src

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

BINBOMS= \
	4.0compat-sysv \
	4.0compat-bsd43 \
	bsd43_root_cmd \
	bsd43_root_dirs \
	bsd43_root_dom \
	bsd43_usr_dom \
	bsd43_usr_include \
	bsd43_usr_lib \
	emacs_cmd \
	emacs_man \
	games_usr \
	m1000_dev \
	m120_dev \
	m2000_dev \
	m2000_dev_common \
	m2000_dev_ijc \
	m2000-25_dev_common \
	m2000-25_dev_ijc \
	m2000-25_dev_ipc \
	m2000-25_dev_sdc \
	RC3230_dev \
	RC6280_dev \
	RC6280_dev_common \
	RC6280_dev_ijc \
	man_dirs \
	man_files \
	man_files_bsd \
	man_files_posix \
	man_links \
	man_links_bsd \
	man_links_posix \
	mh_cmd \
	mh_man \
	news_rn \
	pkgtools.4.10.pkg \
	pkgtools.bin \
	pkgtools.man \
	posix_root_cmd \
	posix_root_dirs \
	posix_usr_include \
	posix_usr_lib \
	rc2030_dev \
	rn_man \
	root.i2000 \
	root.r2300 \
	root.r2400 \
	root.r3030 \
	root.r3200 \
	root.r3200_ijc \
	root.rb3125 \
	root.r6000 \
	root_cmd \
	root_dirs \
	root_dom \
	root_files \
	root_uts \
	sccs_files \
	sccs_man \
	sppbin_std \
	sppbin_2030 \
	usr_cmd \
	usr_dirs \
	usr_dom \
	usr_include \
	usr_include_uts \
	usr_lib \
	usr_term \
	usr_src_uts \
	uucp_bin_dirs \
	uucp_man \
	uucp_files 

SRCBOMS= \
	4.0compat-sysv \
	4.0compat-bsd43 \
	bom-src \
	bsd43_src_dirs \
	bsd43_src_files \
	crossenv \
	emacs_X11 \
	emacs_src_dirs \
	emacs_src_files \
	games_src \
	mansrc_bsd \
	mansrc_dirs \
	mansrc_files \
	mansrc_posix \
	mh_src_dirs \
	mh_src_files \
	pkgtools.pkg \
	pkgtools.src \
	pkgtools.man_src \
	posix_src_dirs \
	posix_src_files \
	rn_src_dirs \
	rn_src_files \
	sccs_mansrc \
	sccs_src_files \
        sccs_src_dirs \
	src_dirs \
	src_files \
	src_uts \
	umips-pkg-src \
        uucp_src_files \
	uucp_mansrc


all clean clobber:
	@echo "Nothing to do"

install:
	ID=`id +%U` ; \
	for dir in usr usr/pkg usr/pkg/lib ; \
	{\
		[ -r $(DESTROOT)/$$dir ] || \
			( ( mkdir -p $(DESTROOT)/$$dir || exit 0) ; \
			  [ $$ID != 0 ] || \
			  ( chgrp $(GROUP) $(DESTROOT)/$$dir ; \
			    chmod 0775 $(DESTROOT)/$$dir ; \
			    chown $(OWNER) $(DESTROOT)/$$dir ) ) ; \
	} ; \
	for dir in $(WHEREIGO) $(WHEREIGO)/boms \
		$(WHEREIGOSRC) $(WHEREIGOSRC)/boms ; \
	{\
		[ -r $$dir ] || \
			( ( mkdir -p $$dir || exit 0) ; \
			  [ $$ID != 0 ] || \
			  ( chgrp $(GROUP) $$dir ; \
			    chmod 0775 $$dir ; \
			    chown $(OWNER) $$dir ) ) ; \
	} ; \
	for file in $(BINBOMS) ; \
	{\
		$(INSTALL) $(INSTOPTS) $$file $(WHEREIGO)/boms ; \
	} ; \
	for file in $(SRCBOMS) ; \
	{\
		$(INSTALL) $(INSTOPTS) $$file $(WHEREIGOSRC)/boms ; \
	} ; \
	if [ -d $(WHEREIGOOEMSRC)/boms ] ; \
	then \
		for file in $(SRCBOMS) ; \
		{\
			$(INSTALL) $(INSTOPTS) $$file $(WHEREIGOOEMSRC)/boms ; \
		} ; \
	fi ;


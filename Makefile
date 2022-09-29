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
# $Header: Makefile,v 1.34.1.10 90/05/07 17:54:21 wje Exp $
SHELL	=/bin/sh

TOPDIRS	=`grep -v '^\#' build_pkgs`
DESTROOT=
CC_RELEASE=2.0

all clobber clean generic all_bsd install_bsd clobber_bsd install_umips clobber_umips clean_umips stand_clean stand_install:
	@for dir in $(TOPDIRS) ;\
	{\
		if [ -d "$$dir" ] ;\
		then \
			echo "XXXXXXX $$dir XXXXXXX" ;\
			( cd $$dir ; $(MAKE) $(MAKEARGS) $@ ) ;\
		else \
			echo "******* No directory $$dir" ;\
		fi ;\
	}

install:
	@for dir in $(TOPDIRS) ;\
	{\
		if [ -d "$$dir" ] ;\
		then \
			echo "XXXXXXX $$dir XXXXXXX" ;\
			( cd $$dir ; $(MAKE) $(MAKEARGS) $@ ) ;\
		else \
			echo "******* No directory $$dir" ;\
		fi ;\
	}

install-headers: install-setup
	(cd head ; $(MAKE) $(MAKEARGS) install )
	(cd bsd_head ; $(MAKE) $(MAKEARGS) install )
	(cd posix_head ; $(MAKE) $(MAKEARGS) install )
	(cd uts ; $(MAKE) $(MAKEARGS) $@ )

install-sys-headers: install-setup
	(cd uts ; $(MAKE) $(MAKEARGS) install-headers )

install-setup:	install-setup-crossenv	

install-setup-sysv:
	echo "XXXXXXX install-setup-sysv XXXXXXX"
	[ -d $(DESTROOT)/usr ] || mkdir -p $(DESTROOT)/usr 
	-chmod 0755 $(DESTROOT)/usr
	[ -d $(DESTROOT)/usr/include ] || mkdir -p $(DESTROOT)/usr/include
	-chmod 0755 $(DESTROOT)/usr/include
	[ -r $(DESTROOT)/usr/include/sysv ] || \
		/bin/ln -sf . $(DESTROOT)/usr/include/sysv
	[ -r $(DESTROOT)/sysv ] || ln -s . $(DESTROOT)/sysv

install-setup-bsd43:
	echo "XXXXXXX install-setup-bsd43 XXXXXXX"
	[ -d $(DESTROOT)/usr/bsd43 ] || mkdir -p $(DESTROOT)/usr/bsd43 
	-chmod 0755 $(DESTROOT)/usr/bsd43 $(DESTROOT)/usr
	[ -d $(DESTROOT)/bsd43 ] || mkdir -p $(DESTROOT)/bsd43
	-chmod 0755 $(DESTROOT)/bsd43
	[ -d $(DESTROOT)/bsd43/usr ] || mkdir $(DESTROOT)/bsd43/usr 
	-chmod 0755 $(DESTROOT)/bsd43/usr
	[ -r $(DESTROOT)/bsd43/bin ] || \
		/bin/ln -sf ../usr/bsd43/bin $(DESTROOT)/bsd43/bin
	[ -r $(DESTROOT)/bsd43/usr/include ] || \
		/bin/ln -sf ../../usr/include/bsd43 \
			$(DESTROOT)/bsd43/usr/include
	[ -r $(DESTROOT)/usr/include/bsd43/bsd43 ] || \
		/bin/ln -sf . $(DESTROOT)/usr/include/bsd43/bsd43
	[ -r $(DESTROOT)/bsd43/usr/lib ] || \
		/bin/ln -sf ../../usr/bsd43/usr/lib \
			$(DESTROOT)/bsd43/usr/lib
	[ -r $(DESTROOT)/bsd43/usr/games ] || \
		/bin/ln -sf ../../usr/bsd43/usr/games \
			$(DESTROOT)/bsd43/usr/games

install-setup-posix:
	echo "XXXXXXX install-setup-posix XXXXXXX"
	[ -d $(DESTROOT)/usr/posix ] || mkdir -p $(DESTROOT)/usr/posix 
	-chmod 0755 $(DESTROOT)/usr/posix $(DESTROOT)/usr
	[ -d $(DESTROOT)/posix ] || mkdir -p $(DESTROOT)/posix
	-chmod 0755 $(DESTROOT)/posix 
	[ -d $(DESTROOT)/posix/usr ] || mkdir $(DESTROOT)/posix/usr 
	-chmod 0755 $(DESTROOT)/posix/usr
	[ -r $(DESTROOT)/posix/bin ] || \
		/bin/ln -sf ../usr/posix/bin $(DESTROOT)/posix/bin
	[ -r $(DESTROOT)/posix/usr/include ] || \
		/bin/ln -sf ../usr/include/posix \
			$(DESTROOT)/posix/usr/include
	[ -r $(DESTROOT)/usr/include/posix/posix ] || \
		/bin/ln -sf . $(DESTROOT)/usr/include/posix/posix
	[ -r $(DESTROOT)/posix/usr/lib ] || \
		/bin/ln -sf ../../usr/posix/usr/lib \
			$(DESTROOT)/posix/usr/lib

install-setup-crossenv:	install-setup-sysv install-setup-bsd43 install-setup-posix
	echo "XXXXXXX install-setup-crossenv XXXXXXX"
	[ -r $(DESTROOT)/usr/posix/sysv ] || \
		/bin/ln -sf ../../sysv $(DESTROOT)/usr/posix/sysv
	[ -r $(DESTROOT)/usr/posix/bsd43 ] || \
		/bin/ln -sf ../../bsd43 $(DESTROOT)/usr/posix/bsd43
	[ -r $(DESTROOT)/usr/include/posix/sysv ] || \
		/bin/ln -sf .. $(DESTROOT)/usr/include/posix/sysv
	[ -r $(DESTROOT)/usr/include/posix/bsd43 ] || \
		/bin/ln -sf ../bsd43 $(DESTROOT)/usr/include/posix/bsd43
	[ -r $(DESTROOT)/usr/bsd43/sysv ] || \
		/bin/ln -sf ../../sysv $(DESTROOT)/usr/bsd43/sysv
	[ -r $(DESTROOT)/usr/bsd43/posix ] || \
		/bin/ln -sf ../../posix $(DESTROOT)/usr/bsd43/posix
	[ -r $(DESTROOT)/usr/include/bsd43/sysv ] || \
		/bin/ln -sf .. $(DESTROOT)/usr/include/bsd43/sysv
	[ -r $(DESTROOT)/usr/include/bsd43/posix ] || \
		/bin/ln -sf ../posix $(DESTROOT)/usr/include/bsd43/posix

install-unix install-lib: install-setup
	(cd uts ; $(MAKE) $(MAKEARGS) $@ )

depend: install-headers
	(cd uts ; $(MAKE) $(MAKEARGS) $@ )

################################################################################
#
# The following targets are set up the tree in ways useful for development.
#

#
# Set up the header environtment for builds using symlinks.
# Assumes that uts and head are both checked out.
#
HDRDIRS= arpa protocols rpc rpcsvc des 
ARPAHDRFILES= \
	ftp.h \
	inet.h \
	nameser.h \
	resolv.h \
	telnet.h \
	tftp.h
PROTOCOLSHDRFILES= \
	routed.h \
	rwhod.h \
	timed.h \
	dumprestore.h \
	talkd.h
SYSHDRFILES= sysmips.h syscall.h signal.h errno.h
MIPSHDRFILES= regdef.h asm.h
BSDHDRFILES= \
	netdb.h \
	resolv.h \
	dbm.h \
	ndbm.h
SYSVHDRFILES= \
	exportent.h \
	nan.h \
	symbol.h \
	unctrl.h \
	tzfile.h \
	values.h \
	memory.h \
	assert.h \
	fp_class.h \
	fpi.h \
	math.h \
	stdarg.h \
	tar.h \
	varargs.h \
	shadow.h
SUNHDRFILES= mntent.h 
INCHDRFILES= \
	nan.h \
	limits.h \
	values.h \
	stdarg.h
POSIXINC2_0LINKS= \
	a.out.h \
	alloca.h \
	aouthdr.h \
	ar.h \
	cmplrs \
	disassembler.h \
	exception.h \
	filehdr.h \
	float.h \
	isam.h \
	ldfcn.h \
	linenum.h \
	nan.h \
	nlist.h \
	opnames.h \
	reloc.h \
	scnhdr.h \
	sex.h \
	stamp.h \
	stdarg.h \
	storclass.h \
	sym.h \
	symconst.h \
	syms.h
BSDHDRDIRS= net netinet netns
BSD43HDRDIRS= mips mipsif mipsvme netimp nfs prom saio sys ufs

header_links:
	D=$${DESTROOT}/usr/src; \
	/bin/rm -rf ${DESTROOT}/usr/include; \
	/bin/ln -sf $$D/head ${DESTROOT}/usr/include; \
	/bin/rm -rf ${DESTROOT}/usr/include/sys; \
	/bin/ln -sf $$D/uts/mips/sys ${DESTROOT}/usr/include/sys; \
	/bin/rm -rf ${DESTROOT}/usr/include/bsd/mips; \
	/bin/ln -sf $$D/uts/mips/bsd/mips ${DESTROOT}/usr/include/bsd; \
	/bin/rm -rf ${DESTROOT}/usr/include/bsd/net; \
	/bin/ln -sf $$D/uts/mips/bsd/net ${DESTROOT}/usr/include/bsd; \
	/bin/rm -rf ${DESTROOT}/usr/include/bsd/netinet; \
	/bin/ln -sf $$D/uts/mips/bsd/netinet ${DESTROOT}/usr/include/bsd; \
	/bin/rm -rf ${DESTROOT}/usr/include/bsd/netns; \
	/bin/ln -sf $$D/uts/mips/bsd/netns ${DESTROOT}/usr/include/bsd; \
	/bin/rm -rf ${DESTROOT}/usr/include/bsd/sys; \
	/bin/ln -sf $$D/uts/mips/bsd/sys ${DESTROOT}/usr/include/bsd/.; \
	/bin/rm -rf ${DESTROOT}/usr/include/rpc; \
	[ ! -d ${DESTROOT}/usr/include/rpc ] && \
		mkdir -p ${DESTROOT}/usr/include/rpc || true ; \
	/bin/ln -sf $$D/bsd_lib/libc/rpc/*.h ${DESTROOT}/usr/include/rpc/.; \
	/bin/rm -rf ${DESTROOT}/usr/include/rpcsvc; \
	[ ! -d ${DESTROOT}/usr/include/rpcsvc ] && \
		mkdir -p ${DESTROOT}/usr/include/rpcsvc || true ; \
	/bin/ln -sf $$D/bsd_lib/librpcsvc/*.h ${DESTROOT}/usr/include/rpcsvc/.; \
	/bin/ln -sf $$D/bsd_lib/librpcsvc/*.x ${DESTROOT}/usr/include/rpcsvc/.; \
	/bin/ln -sf $$D/bsd_lib/libc/yp/*.h ${DESTROOT}/usr/include/rpcsvc/.; \
	/bin/rm -rf $$D/head/des ; \
        mkdir -p $$D/head/des || true ; \
        /bin/ln -sf $$D/bsd_lib/libc/des/*.h $$D/head/des/. ; \
        /bin/ln -sf $$D/bsd_lib/libc/des/softdesdata.c $$D/head/des/. ; \
        /bin/rm -f  $$D/head/des_crypt.h ; \
        /bin/ln -sf $$D/bsd_lib/libc/des/des_crypt.h $$D/head/. ; \
	/bin/rm -rf $$D/head/netdb.h ; \
	/bin/ln -sf bsd/netdb.h $$D/head/netdb.h ; \
	/bin/rm -rf ${DESTROOT}/usr/include/h ; \
 	/bin/ln -sf sys ${DESTROOT}/usr/include/h ; \
	/bin/rm -rf ${DESTROOT}/usr/include${CC_RELEASE}; \
	/bin/ln -sf /usr/include${CC_RELEASE} ${DESTROOT}/usr/. ; \
	for file in $(INCHDRFILES) ; \
	{\
		/bin/rm -rf $$D/head/$$file ; \
		ln -sf /usr/include/$$file $$D/head/$$file ; \
	} ; \
#
# header_links for bsd43 headers in /bsd43/xxx.
#
	(cd uts/mips ; $(MAKE) $(MAKEARGS) header-links )
	[ ! -d ${DESTROOT}/bsd43/usr ] && mkdir -p ${DESTROOT}/bsd43/usr || true
	D=$${DESTROOT}/usr/src; \
	/bin/rm -rf $$D/uts/mips/bsd/sys/un.h ; \
	/bin/ln -sf $$D/uts/mips/bsd/socket/un.h $$D/uts/mips/bsd/sys/. ; \
	/bin/rm -rf $$D/uts/mips/bsd/sys/unpcb.h ; \
	/bin/ln -sf $$D/uts/mips/bsd/socket/unpcb.h $$D/uts/mips/bsd/sys/. ; \
	/bin/rm -rf ${DESTROOT}/bsd43/usr/include; \
	/bin/ln -sf $$D/bsd_head ${DESTROOT}/bsd43/usr/include; \
	for dir in $(HDRDIRS) $(BSDHDRDIRS) $(BSD43HDRDIRS) ; \
	{\
		/bin/rm -rf $$D/bsd_head/$$dir ; \
		( mkdir -p $$D/bsd_head/$$dir || exit 0 ); \
	} ; \
	for dir in $(BSDHDRDIRS) ; \
	{\
		/bin/ln -sf $$D/uts/mips/bsd/$$dir/*.h $$D/bsd_head/$$dir/. ; \
	} ; \
	for dir in $(BSD43HDRDIRS) ; \
	{\
		/bin/ln -sf $$D/uts/mips/bsd43/$$dir/*.h $$D/bsd_head/$$dir/. ; \
	} ; \
	/bin/ln -sf $$D/bsd_lib/libc/rpc/*.h $$D/bsd_head/rpc/. ; \
	/bin/ln -sf $$D/bsd_lib/librpcsvc/*.h $$D/bsd_head/rpcsvc/. ; \
	/bin/ln -sf $$D/bsd_lib/librpcsvc/*.x $$D/bsd_head/rpcsvc/. ; \
	/bin/ln -sf $$D/bsd_lib/libc/yp/*.h $$D/bsd_head/rpcsvc/. ; \
	/bin/ln -sf $$D/bsd_lib/libc/des/*.h $$D/bsd_head/des/. ; \
	/bin/ln -sf $$D/bsd_lib/libc/des/softdesdata.c $$D/bsd_head/des/. ; \
	/bin/rm -f  $$D/bsd_head/des_crypt.h ; \
	/bin/ln -sf $$D/bsd_lib/libc/des/des_crypt.h $$D/bsd_head/. ; \
	/bin/rm -f  $$D/bsd_head/curses.h ; \
	/bin/ln -sf $$D/bsd_lib/libcurses/curses.h $$D/bsd_head/. ; \
	for file in $(SYSHDRFILES) ; \
	{\
		/bin/rm -rf $$D/bsd_head/$$file ; \
		ln -sf sys/$$file $$D/bsd_head/$$file ; \
	} ; \
	for file in $(MIPSHDRFILES) ; \
	{\
		/bin/rm -rf $$D/bsd_head/$$file ; \
		ln -sf mips/$$file $$D/bsd_head/$$file ; \
	} ; \
	for file in $(BSDHDRFILES) ; \
	{\
		/bin/rm -rf $$D/bsd_head/$$file ; \
		ln -sf sysv/bsd/$$file $$D/bsd_head/$$file ; \
	} ; \
	for file in $(SUNHDRFILES) ; \
	{\
		/bin/rm -rf $$D/bsd_head/$$file ; \
		ln -sf sysv/sun/$$file $$D/bsd_head/$$file ; \
	} ; \
	for file in $(SYSVHDRFILES) ; \
	{\
		/bin/rm -rf $$D/bsd_head/$$file ; \
		ln -sf sysv/$$file $$D/bsd_head/$$file ; \
	} ; \
	for file in $(ARPAHDRFILES) ; \
	{\
		ln -sf ../sysv/bsd/arpa/$$file $$D/bsd_head/arpa/$$file ; \
	} ; \
	for file in $(PROTOCOLSHDRFILES) ; \
	{\
		ln -sf ../sysv/bsd/protocols/$$file $$D/bsd_head/protocols/$$file ; \
	} ; \
	/bin/rm -rf $$D/bsd_head/h ; \
	/bin/ln -sf sys $$D/bsd_head/h ; \
	/bin/rm -rf $$D/bsd_head/machine; \
	/bin/ln -sf mips $$D/bsd_head/machine; \
	/bin/rm -rf $$D/bsd_head/fcntl.h ; \
	/bin/ln -sf sys/file.h $$D/bsd_head/fcntl.h ; \
	/bin/rm -rf $$D/bsd_head/sys/regdef.h ; \
	/bin/ln -sf ../regdef.h $$D/bsd_head/sys/regdef.h ; \
	/bin/rm -rf $$D/bsd_head/sys/varargs.h ; \
	/bin/ln -sf ../varargs.h $$D/bsd_head/sys/varargs.h ; \
	/bin/rm -rf $$D/uts/mips/bsd43/mips/inst.h ; \
	/bin/ln -sf ../../sys/inst.h $$D/uts/mips/bsd43/mips/inst.h ; \
	/bin/rm -rf ${DESTROOT}/bsd43/usr/include${CC_RELEASE}; \
	/bin/ln -sf /usr/include${CC_RELEASE} ${DESTROOT}/bsd43/usr/. ;
#
# header_links for posix headers in /posix/xxx.
#
	[ ! -d ${DESTROOT}/posix/usr ] && mkdir -p ${DESTROOT}/posix/usr || true
	D=$${DESTROOT}/usr/src; \
	/bin/rm -rf ${DESTROOT}/posix/usr/include; \
	/bin/ln -sf $$D/posix_head ${DESTROOT}/posix/usr/include; \
	/bin/rm -rf $$D/posix_head/sys ; \
	/bin/ln -sf $$D/uts/mips/posix/sys $$D/posix_head/. ; \
	/bin/rm -rf ${DESTROOT}/posix/usr/include${CC_RELEASE}; 
	[ ! -d ${DESTROOT}/posix/usr/include${CC_RELEASE} ] && \
		mkdir -p ${DESTROOT}/posix/usr/include${CC_RELEASE} || true
	for file in $(POSIXINC2_0LINKS) ; \
	{\
		ln -sf /usr/include${CC_RELEASE}/$$file \
			$${DESTROOT}/posix/usr/include${CC_RELEASE}/$$file; \
	} ; \
#
# Cross links from SYSV to BSD headers.
#
	D=$${DESTROOT}/usr/src; \
	/bin/rm -rf $$D/head/bsd43; \
	/bin/ln -sf $$D/bsd_head $$D/head/bsd43; \
	/bin/rm -rf $$D/head/posix; \
	/bin/ln -sf $$D/posix_head $$D/head/posix; \
	/bin/rm -rf $$D/head/sysv; \
	/bin/ln -sf . $$D/head/sysv; \
	/bin/rm -rf $$D/bsd_head/sysv; \
	/bin/ln -sf $$D/head $$D/bsd_head/sysv; \
	/bin/rm -rf $$D/bsd_head/bsd43; \
	/bin/ln -sf . $$D/bsd_head/bsd43 ; \
	/bin/rm -rf $$D/bsd_head/posix; \
	/bin/ln -sf $$D/posix_head $$D/bsd_head/posix ; \
	/bin/rm -rf $$D/posix_head/sysv; \
	/bin/ln -sf $$D/head $$D/posix_head/sysv; \
	/bin/rm -rf $$D/posix_head/bsd43; \
	/bin/ln -sf $$D/bsd_head $$D/posix_head/bsd43 ; \
	/bin/rm -rf $$D/posix_head/posix; \
	/bin/ln -sf . $$D/posix_head/posix ; \
	/bin/rm -rf ${DESTROOT}/sysv ; \
	/bin/ln -sf . ${DESTROOT}/sysv;\
	/bin/rm -rf ${DESTROOT}/bsd43/sysv ; \
	/bin/ln -sf .. ${DESTROOT}/bsd43/sysv;
	/bin/rm -rf ${DESTROOT}/bsd43/posix ; \
	/bin/ln -sf ../posix ${DESTROOT}/bsd43/posix;
	/bin/rm -rf ${DESTROOT}/posix/sysv ; \
	/bin/ln -sf .. ${DESTROOT}/posix/sysv;
	/bin/rm -rf ${DESTROOT}/posix/bsd43 ; \
	/bin/ln -sf ../bsd43 ${DESTROOT}/posix/bsd43;


bsd_header_links:

cross_links:

posix_header_links:

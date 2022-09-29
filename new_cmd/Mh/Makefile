#
# $Header: Makefile,v 1.2.1.6 90/02/05 12:59:47 wje Exp $
# ---------------------------------------------------
# | Copyright (c) 1986 MIPS Computer Systems, Inc.  |
# | All Rights Reserved.                            |
# ---------------------------------------------------
#
# This makefile is written to be generic, but the following macros should
# generally be re-defined by the caller.
#
#	DESTDIR	The root of the destination directory.
#

SHELL	=/bin/sh
ETCDIR	=/etc

CC=cc
AS=as
LD=ld

DESTROOT=
DESTDIR=$(DESTROOT)

CROSSROOT=
EXPAND=$(CROSSROOT)/bsd43/bin/expand
UNEXPAND=$(CROSSROOT)/bsd43/bin/unexpand
GETNAME=$(CROSSROOT)/usr/lib/getNAME
MAKEWHATIS=$(CROSSROOT)/usr/lib/makewhatis
ETAGS=$(CROSSROOT)/usr/new/etags
CTAGS=$(CROSSROOT)/usr/new/ctags

LIBPARENTDIR=/usr/new/lib
LIBDIR=$(LIBPARENTDIR)/mh
BINDIR=/usr/new/mh
MANDIR=/usr/man/catman/u_man
CATDIR=/usr/man/catman/u_man
MANSECTS=man1 man5 man8

DESTLIBPARENTDIR=$(DESTROOT)$(LIBPARENTDIR)
DESTLIBDIR=$(DESTROOT)$(LIBDIR)
DESTBINDIR=$(DESTROOT)$(BINDIR)
DESTMANDIR=$(DESTROOT)$(MANDIR)
DESTCATDIR=$(DESTROOT)$(CATDIR)

MIPSSUBDIRS= \
	papers \
	papers/beginners \
	papers/bboards \
	papers/mh4 \
	papers/mh5 \
	papers/mh6 \
	papers/mh6.5 \
	papers/multifarious \
	papers/mznet \
	papers/realwork \
	papers/trusted \
	papers/tutorial \
	papers/mh4mm \
	conf \
	conf/config \
	conf/doc \
	conf/examples \
	conf/makefiles \
	conf/makefiles/mts \
	conf/makefiles/support \
	conf/makefiles/zotnet \
	conf/tailor \
	config \
	dist \
	doc \
	h \
	miscellany/ \
	miscellany/convert \
	miscellany/emh \
	miscellany/less \
	miscellany/libndir \
	miscellany/mem \
	miscellany/mh-e \
	miscellany/mhe \
	miscellany/mtrenv \
	miscellany/mtrenv/bin \
	miscellany/mtrenv/mhbox \
	miscellany/netnews \
	miscellany/netnews/Mail \
	miscellany/netnews/bin \
	miscellany/patch \
	miscellany/rcvtrip \
	miscellany/replies \
	miscellany/replies/Mail \
	miscellany/replies/bin \
	miscellany/scripts \
	miscellany/tools \
	mts \
	mts/mh \
	mts/mmdf \
	mts/sendmail \
	sbr \
	support \
	support/bboards \
	support/bboards/mmdfI \
	support/bboards/mmdfI/bboards \
	support/bboards/mmdfII \
	support/bboards/mmdfII/bboards \
	support/general \
	support/pop \
	support/pop/mmdfII \
	support/pop/mmdfII/pop \
	uip \
	zotnet \
	zotnet/bboards \
	zotnet/mf \
	zotnet/mf/mmdfI \
	zotnet/mf/mmdfI/src \
	zotnet/mf/mmdfI/src/uucp \
	zotnet/mf/mmdfI/table \
	zotnet/mts \
	zotnet/tws \
	zotnet/tws/phoon 

DEBUG	=
MAXOPT	=-O2
INCLUDE	=
DEFS	=-DRISCOS
CFLAGS	=-systype bsd43 $(MAXOPT) $(DEBUG) $(INCLUDE) $(DEFS) $(ECFLAGS)
ASFLAGS =-systype bsd43 $(EASFLAGS)
LDFLAGS =-systype bsd43
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)
MFLAGS	=-k

TMACDIR =/usr/lib/tmac
TMACFILE=$(TMACDIR)/tmac.an
DESTTMACFILE=$(DESTROOT)$(TMACFILE)

NROFF	=cat $(DESTTMACFILE) - | tbl | nroff | col
TROFF	=cat $(DESTTMACFILE) - | tbl | troff -Ti10 | lpr -n

WHEREIGO=$(DESTROOT)/usr/new/mh
WHEREIGOMAN=$(DESTROOT)/usr/man

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
DIRMODE =775
MANMODE =444
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP)
BASICINSTOPTS=$(ICOPY) $(ISTRIP) -o $(OWNER) 
MANINSTOPTS=$(ICOPY) -m $(MANMODE) -o $(OWNER) -g $(GROUP) -f

OBJS	=

SRCS	=

HDRS	=

WHOAMI	=

SUBDIRS = 

SUBMAKE = COMP_TARGET_ROOT=$(DESTROOT) ; export COMP_TARGET_ROOT ; \
	$(MAKE) $(MAKEARGS) CC='$(CC)' LD='$(LD)' AS='$(AS)' \
	NROFF='$(NROFF)' \
	LIBDIR='$(DESTLIBDIR)' \
	BINDIR='$(DESTBINDIR)' MANDIR='$(DESTMANDIR)' \
	DESTDIR='$(DESTROOT)' $(MFLAGS)

LOCALSUBMAKE = COMP_TARGET_ROOT=/ ; export COMP_TARGET_ROOT ; \
	$(MAKE) $(MAKEARGS) CC='$(CC)' LD='$(LD)' AS='$(AS)' \
	NROFF='$(NROFF)' \
	LIBDIR='$(DESTLIBDIR)' \
	BINDIR='$(DESTBINDIR)' MANDIR='$(DESTMANDIR)' \
	DESTDIR='$(DESTROOT)' \
	CFLAGS='$(CFLAGS)' ASFLAGS='$(ASFLAGS)' \
	LDFLAGS='$(LDFLAGS)' $(MFLAGS)

all: 	default

default: mipsdirs srcfiles mips/conf/mhconfig config build

mips/conf/mhconfig: mipsdirs srcfiles
	(cd mips/conf ; $(LOCALSUBMAKE) "MAKE=$(LOCALSUBMAKE)" mhconfig)

config:	mips/conf/mhconfig
	(cd mips/conf; PATH=$${PATH}: ./mhconfig examples/mips-riscos)

build: mipsdirs srcfiles
	(cd mips; PATH=$${PATH}: ; export PATH ; $(SUBMAKE) "MAKE=$(SUBMAKE)" )

install: all install-only

install-only: install-files install-whatis

install-files: mkdir
	(cd mips; PATH=$${PATH}: ; export PATH ; $(SUBMAKE) "MAKE=$(SUBMAKE)" inst-all)

install-whatis: 
	$(MAKEWHATIS) -EXPAND $(EXPAND) -UNEXPAND $(UNEXPAND) \
		-GETNAME $(GETNAME) $(WHEREIGOMAN)/catman
	$(INSTALL) $(MANINSTOPTS) $(WHEREIGOMAN)/catman/whatis $(WHEREIGOMAN)
	[ ! -r $(WHEREIGOMAN)/catman/whatis ] || \
		rm -f $(WHEREIGOMAN)/catman/whatis

mkdir: FRC
	ID=`id +%U` ; \
	for dir in $(DESTBINDIR) $(DESTLIBPARENTDIR) $(DESTLIBDIR) ; \
	{\
		[ -r $$dir ] || \
			( ( mkdir -p $$dir || exit 0) ; \
			  [ $$ID != 0 ] || \
			  ( chgrp $(GROUP) $$dir ; \
			    chmod $(DIRMODE) $$dir ; \
			    chown $(OWNER) $$dir ) ) ; \
	} ; \
	for section in $(MANSECTS); \
	{ \
		[ -r $(DESTCATDIR)/$$section ] || \
			( ( mkdir -p $(DESTCATDIR)/$$section || exit 0) ; \
			  [ $$ID != 0 ] || \
			  ( chgrp $(GROUP) $(DESTCATDIR)/$$section ; \
			    chmod $(DIRMODE) $(DESTCATDIR)/$$section ; \
			    chown $(OWNER) $(DESTCATDIR)/$$section ) ) ; \
	} ;

clean:
	[ ! -r mips/Makefile ] || \
	    (cd mips ; PATH=$${PATH}: ; export PATH ; $(SUBMAKE) "MAKE=$(SUBMAKE)" clean )

clobber: 
	rm -f srcfiles-mark mipsdirs-mark
	[ ! -r mips/Makefile ] || \
	    (cd mips ; PATH=$${PATH}: ; export PATH ; $(SUBMAKE) "MAKE=$(SUBMAKE)" distribution )
	rm -f mips/MAKE || true
	$(SHELL) -f ./rm-links.sh dist mips

rm-links:
	rm -f srcfiles-mark
	$(SHELL) -f ./rm-links.sh dist mips

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

mipsdirs: mipsdirs-mark

mipsdirs-mark:
	ID=`id +%U` ; \
	for i in $(MIPSSUBDIRS) ; do \
		if [ ! -r mips/$$i ] ; \
		then \
			( ( mkdir -p mips/$$i || exit 0) ; \
			  [ $$ID != 0 ] || \
			  	( chgrp $(GROUP) mips/$$i ; \
				  chmod $(DIRMODE) mips/$$i ; \
				  chown $(OWNER) mips/$$i ) ) ; \
		fi ; \
	done
	touch mipsdirs-mark

srcfiles: srcfiles-mark

srcfiles-mark:
	$(SHELL) -f ./make-links.sh dist mips
	touch srcfiles-mark

FRC:

TAGS:	FRC
	$(MAKE) $(MAKEARGS) mipsdirs srcfiles
	[ ! -r TAGS ] || rm -f TAGS
	touch TAGS
	( find dist \( -name \*.c -o -name \*.h \) -print | \
		xargs -n100 -t $(ETAGS) -a ) || true

tags:   FRC
	$(MAKE) $(MAKEARGS) mipsdirs srcfiles
	[ ! -r tags ] || rm -f tags
	touch tags
	( find dist \( -name \*.c -o -name \*.h \) -print | \
		xargs -n100 -t $(CTAGS) -a ) || true
	mv tags tag.nosort
	sort tag.nosort > tags
	rm -f tag.nosort


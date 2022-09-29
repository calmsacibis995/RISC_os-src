#
# $Header: Makefile,v 1.2.1.5 90/01/11 17:55:37 wje Exp $
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

DESTROOT=
DESTDIR=$(DESTROOT)

CROSSROOT=
EXPAND=$(CROSSROOT)/bsd43/bin/expand
UNEXPAND=$(CROSSROOT)/bsd43/bin/unexpand
GETNAME=$(CROSSROOT)/usr/lib/getNAME
MAKEWHATIS=$(CROSSROOT)/usr/lib/makewhatis

LIBDIR=/usr/new/lib
BINDIR=/usr/new/
MANDIR=/usr/man/u_man/man1
CATDIR=/usr/man/catman/u_man/man1

DESTLIBDIR=$(DESTROOT)$(LIBDIR)
DESTBINDIR=$(DESTROOT)$(BINDIR)
DESTMANDIR=$(DESTROOT)$(MANDIR)
DESTCATDIR=$(DESTROOT)$(CATDIR)

DEBUG	=
MAXOPT	=-O2
INCLUDE	=
DEFS	=
DEF_CFLAGS=$(MAXOPT) 
CFLAGS	=-systype bsd43 $(DEF_CFLAGS) $(DEBUG) $(INCLUDE) $(DEFS) $(ECFLAGS)
ASFLAGS =-systype bsd43 $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

TMACDIR =/usr/lib/tmac
TMACFILE=$(TMACDIR)/tmac.an
DESTTMACFILE=$(DESTROOT)$(TMACFILE)

NROFF	=cat $(DESTTMACFILE) - | tbl | nroff | col
TROFF	=cat $(DESTTMACFILE) - | tbl | troff -Ti10 | lpr -n

WHEREIGO=$(DESTDIR)/usr/new/
WHEREIGOMAN=$(DESTROOT)/usr/man

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP)
BASICINSTOPTS=$(ICOPY) $(ISTRIP) -o $(OWNER) 
MANINSTOPTS=$(ICOPY) -m 444 -o $(OWNER) -g $(GROUP) -f

OBJS	=

SRCS	=

HDRS	=

WHOAMI	=rn

SUBDIRS = 

all: 	default

default:	srcfiles $(WHOAMI)

configure:	srcfiles mips/config.sh

mips/config.sh: 
	(cd mips; PATH=$${PATH}: $(SHELL) ./Configure bsd43)

mips/Makefile: mips/Makefile.proto
	[ ! -r mips/Makefile ] || rm -f mips/Makefile
	cat mips/Makefile.proto > mips/Makefile

$(WHOAMI): mips/Makefile
	if [ ! -r mips/Makefile ] ; then \
		echo "You must run \"make configure\" before proceeding" ; \
		exit 1 ; fi 
	(cd mips ; COMP_TARGET_ROOT=$(DESTROOT) ; export COMP_TARGET_ROOT ; \
	 PATH=$${PATH}: \
	 $(MAKE) $(MAKEARGS) LIBDIR=$(LIBDIR) BINDIR=$(BINDIR) \
		MANDIR=$(MANDIR) MAKE=$(MAKE) INSTALL=$(INSTALL) \
		mansrc=$(DESTCATDIR) \
		NROFF='$(NROFF)' \
		DESTDIR=$(DESTROOT) )

install: all install-only

install-only: install-files install-whatis

install-files: 
	(cd mips; COMP_TARGET_ROOT=$(DESTROOT) ; export COMP_TARGET_ROOT ; \
	 $(MAKE) $(MAKEARGS) LIBDIR=$(LIBDIR) BINDIR=$(BINDIR) \
		MANDIR=$(MANDIR) MAKE=$(MAKE) INSTALL=$(INSTALL) \
		mansrc=$(DESTCATDIR) \
		NROFF='$(NROFF)' \
		DESTDIR=$(DESTROOT) install )

install-whatis:	
	$(MAKEWHATIS) -EXPAND $(EXPAND) -UNEXPAND $(UNEXPAND) \
		-GETNAME $(GETNAME) $(WHEREIGOMAN)/catman
	$(INSTALL) $(MANINSTOPTS) $(WHEREIGOMAN)/catman/whatis \
		 $(WHEREIGOMAN)/whatis
	[ ! -r $(WHEREIGOMAN)/catman/whatis ] || \
		rm -f $(WHEREIGOMAN)/catman/whatis

clean:
	[ ! -r mips/Makefile ] || \
	    (cd mips ; COMP_TARGET_ROOT=$(DESTROOT) ; \
		export COMP_TARGET_ROOT ; \
	        PATH=$${PATH}: \
		$(MAKE) $(MAKEARGS) LIBDIR=$(LIBDIR) BINDIR=$(BINDIR) \
			MANDIR=$(MANDIR) MAKE=$(MAKE) INSTALL=$(INSTALL) \
			DESTDIR=$(DESTROOT) clean )

clobber: clean
	[ ! -r mips/Makefile ] || rm -f mips/Makefile
	$(MAKE) $(MAKEARGS) rm-links

rm-links:
	$(SHELL) -f ./rm-links.sh dist mips

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

mipsdirs:
	ID=`id +%U` ; \
	for dir in $(MIPSSUBDIRS) ; \
	{\
		[ -r mips/$$dir ] || \
			( ( mkdir -p mips/$$dir || exit 0) ; \
			  [ $$ID != 0 ] || \
			  ( chgrp $(GROUP) mips/$$dir ; \
			    chmod 0775 mips/$$dir ; \
			    chown $(OWNER) mips/$$dir ) ) ; \
	} ;

srcfiles:
	$(SHELL) -f ./make-links.sh dist mips

FRC:

TAGS:	mipsdirs
	(cd mips ; COMP_TARGET_ROOT=$(DESTROOT) ; export COMP_TARGET_ROOT ; \
		$(MAKE) $(MAKEARGS) LIBDIR=$(LIBDIR) \
		BINDIR=$(BINDIR) MANDIR=$(MANDIR) MAKE=$(MAKE) \
		INSTALL=$(INSTALL) DESTDIR=$(DESTROOT) tags )

tags: /tmp
	-rm -f tags
	touch tags
	ctags -a `find dist \( -name \*.c -o -name \*.h \) -print` || true 
	mv tags tag.nosort
	sort tag.nosort > tags
	rm -f tag.nosort


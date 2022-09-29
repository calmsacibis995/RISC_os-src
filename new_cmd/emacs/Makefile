#
# $Header: Makefile,v 1.6.2.2 90/01/11 17:54:21 wje Exp $
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

LIBPARENTDIR=/usr/new/lib
LIBDIR=$(LIBPARENTDIR)/emacs
BINDIR=/usr/new
MANDIR=/usr/man/u_man/man1
CATDIR=/usr/man/catman/u_man/man1

DESTLIBPARENTDIR=$(DESTROOT)$(LIBPARENTDIR)
DESTLIBDIR=$(DESTROOT)$(LIBDIR)
DESTBINDIR=$(DESTROOT)$(BINDIR)
DESTMANDIR=$(DESTROOT)$(MANDIR)
DESTCATDIR=$(DESTROOT)$(CATDIR)

MIPSSUBDIRS= cpp etc gdb info lisp man shortnames src lisp/gnews-2.0 lisp/term

DEBUG	=
MAXOPT	=
INCLUDE	=
DEFS	=
DEF_CFLAGS=$(MAXOPT) -g
CFLAGS	=-systype bsd43 $(DEF_CFLAGS) $(DEBUG) $(INCLUDE) $(DEFS) $(ECFLAGS)
ASFLAGS =-systype bsd43 $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

TMACDIR =/usr/lib/tmac
TMACFILE=$(TMACDIR)/tmac.an
DESTTMACFILE=$(DESTROOT)$(TMACFILE)

NROFF	=cat $(DESTTMACFILE) - | tbl | nroff | col
TROFF	=cat $(DESTTMACFILE) - | tbl | troff -Ti10 | lpr -n

WHEREIGO=$(DESTDIR)/usr/new
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

WHOAMI	=emacs

SUBDIR= src etc

LIBSUBDIRS= etc info lisp lisp/gnews-2.0 lisp/term

DISTLIBSRCFILES= \
	etc/3B-MAXMEM \
	etc/APOLLO \
	etc/APPLE \
	etc/CCADIFF \
	etc/COOKIES \
	etc/COPYING \
	etc/ChangeLog \
	etc/DEBUG \
	etc/DIFF \
	etc/DISTRIB \
	etc/FTP \
	etc/GNU \
	etc/GOSDIFF \
	etc/INTERVIEW \
	etc/LEDIT \
	etc/MACHINES \
	etc/MAILINGLISTS \
	etc/MOTIVATION \
	etc/Makefile \
	etc/NEWS \
	etc/ONEWS \
	etc/OONEWS \
	etc/RCP \
	etc/README \
	etc/SERVICE \
	etc/SUN-SUPPORT \
	etc/SUNBUG \
	etc/TERMS \
	etc/TO-DO \
	etc/TUTORIAL \
	etc/XDOC \
	etc/XENIX \
	etc/Xkeymap.txt \
	etc/celibacy.1 \
	etc/complink.com \
	etc/cvtmail.c \
	etc/digest-doc.c \
	etc/echo.msg \
	etc/emacs.1 \
	etc/emacs.icon \
	etc/emacs.names \
	etc/emacsclient.c \
	etc/emacstool.1 \
	etc/emacstool.c \
	etc/env.c \
	etc/etags-vmslib.c \
	etc/etags.c \
	etc/etags.vms \
	etc/fakemail.c \
	etc/ledit.l \
	etc/leditcfns.c \
	etc/loadst.c \
	etc/make-docfile.c \
	etc/makedoc.com \
	etc/movemail.c \
	etc/qsort.c \
	etc/refcard.tex \
	etc/server.c \
	etc/sex.6 \
	etc/sorted-doc.c \
	etc/spook.lines \
	etc/termcap.dat \
	etc/termcap.ucb \
	etc/test-distrib.c \
	etc/testfile \
	etc/ulimit.hack \
	etc/vipcard.tex \
	etc/vmslink.opt \
	etc/xmouse.doc \
	etc/yow.c \
	etc/yow.lines \
	info/COPYING \
	info/dir \
	info/emacs \
	info/emacs-1 \
	info/emacs-10 \
	info/emacs-11 \
	info/emacs-12 \
	info/emacs-13 \
	info/emacs-2 \
	info/emacs-3 \
	info/emacs-4 \
	info/emacs-5 \
	info/emacs-6 \
	info/emacs-7 \
	info/emacs-8 \
	info/emacs-9 \
	info/gdb \
	info/gdb-1 \
	info/gdb-2 \
	info/gdb-3 \
	info/info \
	info/termcap \
	info/termcap-1 \
	info/termcap-2 \
	info/termcap-3 \
	info/texinfo \
	info/texinfo-1 \
	info/texinfo-2 \
	info/texinfo-3 \
	info/texinfo-4 \
	info/vip \
	lisp/COPYING \
	lisp/ChangeLog \
	lisp/abbrev.el \
	lisp/abbrev.elc \
	lisp/abbrevlist.el \
	lisp/abbrevlist.elc \
	lisp/ada.el \
	lisp/ada.elc \
	lisp/add-log.el \
	lisp/add-log.elc \
	lisp/autoinsert.el \
	lisp/backquote.el \
	lisp/backquote.elc \
	lisp/bibtex.el \
	lisp/bibtex.elc \
	lisp/blackbox.el \
	lisp/blackbox.elc \
	lisp/buff-menu.el \
	lisp/buff-menu.elc \
	lisp/bytecomp.el \
	lisp/bytecomp.elc \
	lisp/c-fill.el \
	lisp/c-fill.elc \
	lisp/c-mode.el \
	lisp/c-mode.elc \
	lisp/cal.el \
	lisp/chistory.el \
	lisp/chistory.elc \
	lisp/cl-indent.el \
	lisp/cl-indent.elc \
	lisp/cl.el \
	lisp/cl.elc \
	lisp/cmacexp.el \
	lisp/compare-w.el \
	lisp/compare-w.elc \
	lisp/compile.el \
	lisp/compile.elc \
	lisp/dabbrev.el \
	lisp/dabbrev.elc \
	lisp/dbx.el \
	lisp/dbx.elc \
	lisp/debug.el \
	lisp/debug.elc \
	lisp/dired.el \
	lisp/dired.elc \
	lisp/disass.el \
	lisp/disass.elc \
	lisp/dissociate.el \
	lisp/dissociate.elc \
	lisp/doctex.el \
	lisp/doctor.el \
	lisp/doctor.elc \
	lisp/ebuff-menu.el \
	lisp/ebuff-menu.elc \
	lisp/echistory.el \
	lisp/echistory.elc \
	lisp/edt-doc.el \
	lisp/edt.el \
	lisp/edt.elc \
	lisp/ehelp.el \
	lisp/ehelp.elc \
	lisp/electric.el \
	lisp/electric.elc \
	lisp/emacsbug.el \
	lisp/files.el \
	lisp/files.elc \
	lisp/fill.el \
	lisp/fill.elc \
	lisp/flame.el \
	lisp/flame.elc \
	lisp/float.el \
	lisp/float.elc \
	lisp/fortran.el \
	lisp/fortran.elc \
	lisp/ftp.el \
	lisp/ftp.elc \
	lisp/gdb.el \
	lisp/gdb.elc \
	lisp/gnews-2.0/==README \
	lisp/gnews-2.0/=CHANGES-1 \
	lisp/gnews-2.0/=MANUAL \
	lisp/gnews-2.0/=RN-5.0 \
	lisp/gnews-2.0/=WISHES \
	lisp/gnews-2.0/Help.el \
	lisp/gnews-2.0/Help.elc \
	lisp/gnews-2.0/Init.el \
	lisp/gnews-2.0/Init.elc \
	lisp/gnews-2.0/NNTP.el \
	lisp/gnews-2.0/NNTP.elc \
	lisp/gnews-2.0/Roster.el \
	lisp/gnews-2.0/Roster.elc \
	lisp/gnews-2.0/Spool.el \
	lisp/gnews-2.0/Spool.elc \
	lisp/gnews-2.0/art.el \
	lisp/gnews-2.0/art.elc \
	lisp/gnews-2.0/digest.el \
	lisp/gnews-2.0/digest.elc \
	lisp/gnews-2.0/entry.el \
	lisp/gnews-2.0/entry.elc \
	lisp/gnews-2.0/group.el \
	lisp/gnews-2.0/group.elc \
	lisp/gnews-2.0/hook.el \
	lisp/gnews-2.0/hook.elc \
	lisp/gnews-2.0/index.el \
	lisp/gnews-2.0/index.elc \
	lisp/gnews-2.0/mail.el \
	lisp/gnews-2.0/mail.elc \
	lisp/gnews-2.0/news.el \
	lisp/gnews-2.0/news.elc \
	lisp/gnews-2.0/prims.el \
	lisp/gnews-2.0/prims.elc \
	lisp/gnews-2.0/reply.el \
	lisp/gnews-2.0/reply.elc \
	lisp/gnews-2.0/utils.el \
	lisp/gnews-2.0/utils.elc \
	lisp/gosmacs.el \
	lisp/grow-vers.el \
	lisp/hanoi.el \
	lisp/hanoi.elc \
	lisp/help.el \
	lisp/help.elc \
	lisp/helper.el \
	lisp/helper.elc \
	lisp/hideif.el \
	lisp/hideif.elc \
	lisp/icon.el \
	lisp/icon.elc \
	lisp/inc-vers.el \
	lisp/indent.el \
	lisp/indent.elc \
	lisp/info.el \
	lisp/info.elc \
	lisp/informat.el \
	lisp/informat.elc \
	lisp/isearch.el \
	lisp/isearch.elc \
	lisp/kermit.el \
	lisp/keypad.el \
	lisp/keypad.elc \
	lisp/ledit.el \
	lisp/life.el \
	lisp/life.elc \
	lisp/lisp-mode.el \
	lisp/lisp-mode.elc \
	lisp/lisp.el \
	lisp/lisp.elc \
	lisp/loaddefs.el \
	lisp/loadup.el \
	lisp/lpr.el \
	lisp/lpr.elc \
	lisp/macros.el \
	lisp/macros.elc \
	lisp/mail-utils.el \
	lisp/mail-utils.elc \
	lisp/mailalias.el \
	lisp/mailalias.elc \
	lisp/mailpost.el \
	lisp/makesum.el \
	lisp/makesum.elc \
	lisp/man.el \
	lisp/man.elc \
	lisp/medit.el \
	lisp/medit.elc \
	lisp/meese.el \
	lisp/mh-e.el \
	lisp/mh-e.elc \
	lisp/mim-mode.el \
	lisp/mim-mode.elc \
	lisp/mim-syntax.el \
	lisp/mim-syntax.elc \
	lisp/mlconvert.el \
	lisp/mlconvert.elc \
	lisp/mlsupport.el \
	lisp/mlsupport.elc \
	lisp/modula2.el \
	lisp/modula2.elc \
	lisp/novice.el \
	lisp/nroff-mode.el \
	lisp/nroff-mode.elc \
	lisp/options.el \
	lisp/options.elc \
	lisp/outline.el \
	lisp/outline.elc \
	lisp/page.el \
	lisp/page.elc \
	lisp/paragraphs.el \
	lisp/paragraphs.elc \
	lisp/paths.el \
	lisp/picture.el \
	lisp/picture.elc \
	lisp/prolog.el \
	lisp/prolog.elc \
	lisp/rect.el \
	lisp/rect.elc \
	lisp/register.el \
	lisp/register.elc \
	lisp/replace.el \
	lisp/replace.elc \
	lisp/rfc822.el \
	lisp/rfc822.elc \
	lisp/rmail.el \
	lisp/rmail.elc \
	lisp/rmailedit.el \
	lisp/rmailedit.elc \
	lisp/rmailkwd.el \
	lisp/rmailkwd.elc \
	lisp/rmailmsc.el \
	lisp/rmailmsc.elc \
	lisp/rmailout.el \
	lisp/rmailout.elc \
	lisp/rmailsum.el \
	lisp/rmailsum.elc \
	lisp/rnews.el \
	lisp/rnews.elc \
	lisp/rnewspost.el \
	lisp/rnewspost.elc \
	lisp/saveconf.el \
	lisp/scheme.el \
	lisp/scheme.elc \
	lisp/scribe.el \
	lisp/scribe.elc \
	lisp/sendmail.el \
	lisp/sendmail.elc \
	lisp/server.el \
	lisp/server.elc \
	lisp/shell.el \
	lisp/shell.elc \
	lisp/simple.el \
	lisp/simple.elc \
	lisp/simula.defns \
	lisp/simula.el \
	lisp/simula.elc \
	lisp/sort.el \
	lisp/sort.elc \
	lisp/spell.el \
	lisp/spell.elc \
	lisp/spook.el \
	lisp/startup.el \
	lisp/startup.elc \
	lisp/studly.el \
	lisp/studly.elc \
	lisp/subr.el \
	lisp/subr.elc \
	lisp/sun-cursors.el \
	lisp/sun-fns.el \
	lisp/sun-fns.elc \
	lisp/sun-keys.el \
	lisp/sun-mouse.el \
	lisp/sun-mouse.elc \
	lisp/sup-mouse.el \
	lisp/sup-mouse.elc \
	lisp/tabify.el \
	lisp/tabify.elc \
	lisp/tags.el \
	lisp/tags.elc \
	lisp/telnet.el \
	lisp/telnet.elc \
	lisp/term/COPYING \
	lisp/term/apollo.el \
	lisp/term/bobcat.el \
	lisp/term/s4.el \
	lisp/term/sun.el \
	lisp/term/supdup.el \
	lisp/term/unixpc.el \
	lisp/term/vt100.el \
	lisp/term/vt125.el \
	lisp/term/vt200.el \
	lisp/term/vt220.el \
	lisp/term/vt240.el \
	lisp/term/x-win.el \
	lisp/terminal.el \
	lisp/terminal.elc \
	lisp/tex-mode.el \
	lisp/tex-mode.elc \
	lisp/tex-start.el \
	lisp/texinfmt.el \
	lisp/texinfmt.elc \
	lisp/texinfo.el \
	lisp/texinfo.elc \
	lisp/text-mode.el \
	lisp/text-mode.elc \
	lisp/time.el \
	lisp/time.elc \
	lisp/uncompress.el \
	lisp/underline.el \
	lisp/underline.elc \
	lisp/undigest.el \
	lisp/undigest.elc \
	lisp/unused.el \
	lisp/userlock.el \
	lisp/userlock.elc \
	lisp/version.el \
	lisp/vi.el \
	lisp/vi.elc \
	lisp/view.el \
	lisp/view.elc \
	lisp/vip.el \
	lisp/vip.elc \
	lisp/vms-patch.el \
	lisp/vms-patch.elc \
	lisp/vmsproc.el \
	lisp/vmsproc.elc \
	lisp/window.el \
	lisp/window.elc \
	lisp/x-menu.el \
	lisp/x-menu.elc \
	lisp/x-mouse.el \
	lisp/x-mouse.elc \
	lisp/xscheme.el \
	lisp/xscheme.elc \
	lisp/yow.el \
	lisp/yow.elc

MIPSLIBSRCFILES= \
	lisp/background.el \
	lisp/background.elc \
	lisp/pascal.el \
	lisp/pascal.elc \
	lisp/site-load.el

MIPSLIBGENFILES= \
	etc/DOC \
	etc/DOC-18.52.1

MIPSLIBBINFILES= \
	etc/ctags \
	etc/cvtmail \
	etc/digest-doc \
	etc/emacsclient \
	etc/env \
	etc/etags \
	etc/fakemail \
	etc/make-docfile \
	etc/movemail \
	etc/server \
	etc/sorted-doc \
	etc/test-distrib \
	etc/yow 

MIPSLIBPRIVFILES= \
	etc/loadst

all: 	default

default: mips/lisp/version.el \
		srcfiles mips/src/paths.h mips/src/config.h $(SUBDIR) 

mips/lisp/version.el:
	cp dist/lisp/version.el mips/lisp/version.el
	chmod +w mips/lisp/version.el

mips/src/paths.h: mips/src/paths.h-dist
	/bin/sed 's;/usr/local/emacs;${LIBDIR};' < mips/src/paths.h-dist > mips/src/paths.h

mips/src/s-riscos-release.h: mips/src/s-riscos.h
	sed -e "s,/bsd43,$$COMP_TARGET_ROOT/bsd43,g" < mips/src/s-riscos.h \
		> mips/src/s-riscos-release.h

mips/src/config.h: mips/src/config.h-riscos mips/src/s-riscos-release.h
#	cp mips/src/config.h-riscos mips/src/config.h
	sed -e 's/s-riscos.h/s-riscos-release.h/' < mips/src/config.h-riscos \
		> mips/src/config.h

src:	etc

src: FRC
	(cd mips/$@ ; COMP_TARGET_ROOT=$(DESTROOT) ; export COMP_TARGET_ROOT ; \
		$(MAKE) $(MAKEARGS) LIBDIR=$(LIBDIR) \
		BINDIR=$(BINDIR) MANDIR=$(MANDIR) MAKE=$(MAKE) INSTALL=$(INSTALL) all )

mips/etc/test-distrib: mips/etc/test-distrib.c
	(cd mips/etc ; $(CC) -systype bsd43 -o test-distrib test-distrib.c)
	(cd mips/etc ; ./test-distrib)

etc: FRC mips/etc/test-distrib
	(cd mips/$@ ; COMP_TARGET_ROOT=$(DESTROOT) ; export COMP_TARGET_ROOT ; \
		$(MAKE) $(MAKEARGS) LIBDIR=$(LIBDIR) \
		BINDIR=$(BINDIR) MANDIR=$(MANDIR) MAKE=$(MAKE) INSTALL=$(INSTALL) CFLAGS="$(CFLAGS)" ASFLAGS="$(ASFLAGS)" all )

install: all install-only

install-files: mkdir lockdir
	for i in ${DISTLIBSRCFILES} ; do \
		$(INSTALL) ${MANINSTOPTS} dist/$$i ${DESTLIBDIR}/$$i \
			|| true; \
	done
	for i in ${MIPSLIBSRCFILES} ${MIPSLIBGENFILES} ; do \
		$(INSTALL) ${MANINSTOPTS} mips/$$i ${DESTLIBDIR}/$$i \
			|| true; \
	done
	for i in ${MIPSLIBBINFILES} ; do \
		$(INSTALL) ${INSTOPTS} -f mips/$$i ${DESTLIBDIR}/$$i \
			|| true; \
	done
	for i in ${MIPSLIBPRIVFILES} ; do \
		$(INSTALL) $(BASICINSTOPTS) -g sys -m 2755 -f mips/$$i ${DESTLIBDIR}/$$i \
			|| true; \
	done
	$(INSTALL) $(INSTOPTS) mips/etc/etags ${DESTBINDIR}
	$(INSTALL) $(INSTOPTS) mips/etc/ctags ${DESTBINDIR}
	$(INSTALL) $(INSTOPTS) -f mips/src/xemacs ${DESTBINDIR}/emacs

install-man:
	if [ -d $(DESTCATDIR)/. ] ; \
	then \
		rm -f nroff.tmp ;\
		cat mips/etc/emacs.1 \
			| sed -e "s,/usr/local/emacs,$(LIBDIR),g" \
			| $(NROFF) > nroff.tmp ;\
		$(INSTALL) $(MANINSTOPTS) nroff.tmp $(DESTCATDIR)/emacs.1 ;\
		rm -f nroff.tmp ;\
	fi
	if [ -d $(DESTMANDIR)/. ] ; \
	then \
	    if [ ! -d $(DESTCATDIR)/. ] || \
	       [ `(cd $(DESTCATDIR) ; /bin/pwd )` != `(cd $(DESTMANDIR)/. ; /bin/pwd )` ] ; \
	    then \
		rm -f nroff.tmp ;\
		cat mips/etc/emacs.1 | sed -e "s,/usr/local/emacs,$(LIBDIR),g" > nroff.tmp ;\
		$(INSTALL) $(MANINSTOPTS) nroff.tmp $(DESTMANDIR)/emacs.1 ;\
		rm -f nroff.tmp ;\
	    fi ; \
	fi

mkdir: FRC
	[ -d $(DESTBINDIR)/. ] || mkdir -p $(DESTBINDIR) ; \
		( chown bin $(DESTBINDIR) ; \
		chgrp bin $(DESTBINDIR) ; \
		chmod 0775 $(DESTBINDIR) ) || true
	[ -d ${DESTLIBPARENTDIR}/. ] || mkdir -p ${DESTLIBPARENTDIR} ; \
		( chown bin ${DESTLIBPARENTDIR} ; \
		chgrp bin ${DESTLIBPARENTDIR} ; \
		chmod 0775 ${DESTLIBPARENTDIR} ) || true
	[ -d $(DESTLIBDIR)/. ] || mkdir -p $(DESTLIBDIR) ; \
		( chown bin $(DESTLIBDIR) ; \
		chgrp bin $(DESTLIBDIR) ; \
		chmod 0775 $(DESTLIBDIR) ) || true
	for i in ${LIBSUBDIRS} ; do \
		[ -d ${DESTLIBDIR}/$$i/. ] || mkdir -p ${DESTLIBDIR}/$$i ; \
			( chown bin ${DESTLIBDIR}/$$i ; \
			chgrp bin ${DESTLIBDIR}/$$i ; \
			chmod 0775 ${DESTLIBDIR}/$$i ) || true ; \
	done

lockdir:
	[ -d $(DESTLIBDIR)/lock/. ] || mkdir $(DESTLIBDIR)/lock ; \
		(chown bin $(DESTLIBDIR)/lock ; \
		chgrp bin $(DESTLIBDIR)/lock ; \
		chmod 0777 $(DESTLIBDIR)/lock ) || true 

clean:
	[ ! -r mips/Makefile ] || \
	    (cd mips ; COMP_TARGET_ROOT=$(DESTROOT) ; export COMP_TARGET_ROOT ; \
		$(MAKE) $(MAKEARGS) LIBDIR=$(LIBDIR) \
		BINDIR=$(BINDIR) MANDIR=$(MANDIR) MAKE=$(MAKE) INSTALL=$(INSTALL) clean )

clobber: 
	[ ! -r mips/Makefile ] || \
	    (cd mips ; COMP_TARGET_ROOT=$(DESTROOT) ; export COMP_TARGET_ROOT ; \
		$(MAKE) $(MAKEARGS) LIBDIR=$(LIBDIR) \
		BINDIR=$(BINDIR) MANDIR=$(MANDIR) MAKE=$(MAKE) INSTALL=$(INSTALL) distclean )
	rm -f mips/src/s-riscos-release.h || true
	rm -f mips/lisp/version.el || true
	$(SHELL) -f ./rm-links.sh dist mips

rm-links:
	$(SHELL) -f ./rm-links.sh dist mips

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

srcfiles: 
	for i in $(MIPSSUBDIRS) ; do \
		if [ ! -r mips/$$i ] ; \
		then \
			mkdir mips/$$i ; \
			chmod 0775 mips/$$i ; \
			chgrp bin mips/$$i || true ; \
			chown bin mips/$$i || true ; \
		fi ; \
	done
	$(SHELL) -f ./make-links.sh dist mips

FRC:

TAGS:	srcfiles
	(cd mips ; COMP_TARGET_ROOT=$(DESTROOT) ; export COMP_TARGET_ROOT ; \
		$(MAKE) $(MAKEARGS) LIBDIR=$(LIBDIR) \
		BINDIR=$(BINDIR) MANDIR=$(MANDIR) MAKE=$(MAKE) INSTALL=$(INSTALL) tags )

install-whatis: install-man
	$(MAKEWHATIS) -EXPAND $(EXPAND) -UNEXPAND $(UNEXPAND) \
		-GETNAME $(GETNAME) $(WHEREIGOMAN)/catman
	[ ! -r $(WHEREIGOMAN)/whatis ] || rm -f $(WHEREIGOMAN)/whatis
	mv $(WHEREIGOMAN)/catman/whatis $(WHEREIGOMAN)/whatis
	chmod 0444 $(WHEREIGOMAN)/whatis
	-chgrp bin $(WHEREIGOMAN)/whatis
	-chown bin $(WHEREIGOMAN)/whatis

install-only: install-files install-whatis

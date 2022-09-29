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
# $Header: Makefile,v 1.3.1.5 90/05/09 17:23:24 wje Locked $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

LINTF=-DLANGUAGE_C -C -E -Dlint

SRCS=llib-lbsd llib-lc llib-lcrypt llib-lcurses llib-ldbm llib-ll llib-lmall.c llib-lsun llib-port

WHEREIGO=$(DESTROOT)/usr/lib/lint

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ISTRIP	=
ICOPY	=-c
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP)


all: llib-lbsd.ln llib-lc.ln llib-lcrypt.ln llib-lcurses.ln llib-ldbm.ln llib-ll.ln llib-lmalloc.ln llib-port.ln llib-lsun.ln

install: all
	ID=`id +%U` ; \
	{\
	    [ -r $(WHEREIGO) ] || ( ( mkdir -p $(WHEREIGO)  || exit 0) ; \
	    [ $$ID != 0 ] || \
		( chgrp $(GROUP) $(WHEREIGO) ; \
		  chmod 0755 $(WHEREIGO) ; \
		  chown $(OWNER) $(WHEREIGO) ) ) ; \
	}
	cp llib-lmall.c llib-lmalloc
	$(INSTALL) $(INSTOPTS) llib-lbsd $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-lc $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-lcrypt $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-lcurses $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-ldbm $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-ll $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-lmalloc $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-port $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-lsun $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-lbsd.ln $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-lc.ln $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-lcrypt.ln $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-lcurses.ln $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-ldbm.ln $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-ll.ln $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-lmalloc.ln $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-port.ln $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-lsun.ln $(WHEREIGO)

llib-lbsd.ln : llib-lbsd
	$(CC)  $(LINTF) llib-lbsd | $(CROSSENV)/usr/lib/cmplrs/cc/lint1 -vx -Hhlint > llib-lbsd.ln
	rm -f hlint

llib-lc.ln : llib-lc
	$(CC)  $(LINTF) llib-lc | $(CROSSENV)/usr/lib/cmplrs/cc/lint1 -vx -Hhlint > llib-lc.ln
	rm -f hlint

llib-lcrypt.ln : llib-lcrypt
	$(CC)  $(LINTF) llib-lcrypt | $(CROSSENV)/usr/lib/cmplrs/cc/lint1 -vx -Hhlint > llib-lcrypt.ln
	rm -f hlint

llib-lcurses.ln : llib-lcurses
	$(CC)  $(LINTF) llib-lcurses | $(CROSSENV)/usr/lib/cmplrs/cc/lint1 -vx -Hhlint > llib-lcurses.ln
	rm -f hlint

llib-ldbm.ln : llib-ldbm
	$(CC)  $(LINTF) llib-ldbm | $(CROSSENV)/usr/lib/cmplrs/cc/lint1 -vx -Hhlint > llib-ldbm.ln
	rm -f hlint

llib-ll.ln : llib-ll
	$(CC)  $(LINTF) llib-ll | $(CROSSENV)/usr/lib/cmplrs/cc/lint1 -vx -Hhlint > llib-ll.ln
	rm -f hlint

llib-lmalloc.ln : llib-lmall.c
	$(CC)  $(LINTF) llib-lmall.c | $(CROSSENV)/usr/lib/cmplrs/cc/lint1 -vx -Hhlint > llib-lmalloc.ln
	rm -f hlint

llib-port.ln : llib-port
	$(CC)  $(LINTF) llib-port | $(CROSSENV)/usr/lib/cmplrs/cc/lint1 -vxp -Hhlint > llib-port.ln
	rm -f hlint

llib-lsun.ln : llib-lsun
	$(CC)  $(LINTF) llib-lsun | $(CROSSENV)/usr/lib/cmplrs/cc/lint1 -vx -Hhlint > llib-lsun.ln
	rm -f hlint

clean clobber :
	rm -f *.ln hlint

llib-ldbm: ../../bsd_cmd/lint-libs/llib-ldbm
	[ ! -r $@ ] || rm -f $@
	ln -s ../../bsd_cmd/lint-libs/$@ $@

llib-ll: ../../bsd_cmd/lint-libs/llib-ll
	[ ! -r $@ ] || rm -f $@
	ln -s ../../bsd_cmd/lint-libs/$@ $@

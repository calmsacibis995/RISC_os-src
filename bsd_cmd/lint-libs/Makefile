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
# $Header: Makefile,v 1.4.1.4 90/05/10 20:10:08 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

LINTF=-DLANGUAGE_C -C -E -Dlint

SRCS=llib-lc llib-lcrypt llib-lcurses llib-ldbm llib-ll llib-lm -llib-lmp llib-lplot llib-ltermcap llib-port

WHEREIGO=$(DESTROOT)/bsd43/usr/lib/lint

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ISTRIP	=
ICOPY	=-c
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP)


all: llib-lc.ln llib-lcrypt.ln llib-lcurses.ln llib-ldbm.ln llib-ll.ln llib-lm.ln llib-lmp.ln llib-lplot.ln llib-ltermcap.ln llib-port.ln

install: all
	ID=`id +%U` ; \
	{\
	    [ -r $(WHEREIGO) ] || ( ( mkdir -p $(WHEREIGO)  || exit 0) ; \
	    [ $$ID != 0 ] || \
		( chgrp $(GROUP) $(WHEREIGO) ; \
		  chmod 0755 $(WHEREIGO) ; \
		  chown $(OWNER) $(WHEREIGO) ) ) ; \
	}
	$(INSTALL) $(INSTOPTS) llib-lc.ln $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-lcrypt.ln $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-lcurses.ln $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-ldbm.ln $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-ll.ln $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-lm.ln $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-lmp.ln $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-lplot.ln $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-ltermcap.ln $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-port.ln $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-lc $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-lcrypt $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-lcurses $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-ldbm $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-ll $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-lm $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-lmp $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-lplot $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-ltermcap $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) llib-port $(WHEREIGO)

llib-lc.ln : llib-lc
	$(CC) -systype bsd43 $(LINTF) llib-lc | $(CROSSENV)/usr/lib/cmplrs/cc/lint1 -vx -Hhlint > llib-lc.ln
	rm -f hlint

llib-lcrypt.ln : llib-lcrypt
	$(CC) -systype bsd43 $(LINTF) llib-lcrypt | $(CROSSENV)/usr/lib/cmplrs/cc/lint1 -vx -Hhlint > llib-lcrypt.ln
	rm -f hlint

llib-lcurses.ln : llib-lcurses
	$(CC) -systype bsd43 $(LINTF) llib-lcurses | $(CROSSENV)/usr/lib/cmplrs/cc/lint1 -vx -Hhlint > llib-lcurses.ln
	rm -f hlint

llib-ldbm.ln : llib-ldbm
	$(CC) -systype bsd43 $(LINTF) llib-ldbm | $(CROSSENV)/usr/lib/cmplrs/cc/lint1 -vx -Hhlint > llib-ldbm.ln
	rm -f hlint

llib-ll.ln : llib-ll
	$(CC) -systype bsd43 $(LINTF) llib-ll | $(CROSSENV)/usr/lib/cmplrs/cc/lint1 -vx -Hhlint > llib-ll.ln
	rm -f hlint

llib-lm.ln : llib-lm
	$(CC) -systype bsd43 $(LINTF) llib-lm | $(CROSSENV)/usr/lib/cmplrs/cc/lint1 -vx -Hhlint > llib-lm.ln
	rm -f hlint

llib-lmp.ln : llib-lmp
	$(CC) -systype bsd43 $(LINTF) llib-lmp | $(CROSSENV)/usr/lib/cmplrs/cc/lint1 -vx -Hhlint > llib-lmp.ln
	rm -f hlint

llib-lplot.ln : llib-lplot
	$(CC) -systype bsd43 $(LINTF) llib-lplot | $(CROSSENV)/usr/lib/cmplrs/cc/lint1 -vx -Hhlint > llib-lplot.ln
	rm -f hlint

llib-ltermcap.ln : llib-ltermcap
	$(CC) -systype bsd43 $(LINTF) llib-ltermcap | $(CROSSENV)/usr/lib/cmplrs/cc/lint1 -vx -Hhlint > llib-ltermcap.ln
	rm -f hlint

llib-port.ln : llib-port
	$(CC) -systype bsd43 $(LINTF) llib-port | $(CROSSENV)/usr/lib/cmplrs/cc/lint1 -vxp -Hhlint > llib-port.ln
	rm -f hlint

clean clobber :
	rm -f *.ln hlint

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
# $Header: Makefile,v 1.1.1.2 90/05/09 19:39:16 wje Exp $
#
SHELL	=/bin/sh

HDRS	=compiler.h object.h print.h otermcap.h
GETHDRS =curses.h unctrl.h curses_inc.h curshdr.h term.h uparm.h

all: $(HDRS) $(GETHDRS)

install:

clean:

clobber:

generic:
	$(GENERIC) -h $(HDRS)

$(GETHDRS): ../../../lib/libcurses/setup/$@
	[ ! -r $@ ] || rm -f $@
	cp ../../../lib/libcurses/setup/$@ .

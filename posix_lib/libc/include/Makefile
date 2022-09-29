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
# $Header: Makefile,v 1.3.1.2 90/05/10 04:15:57 wje Exp $
#
SHELL	=/bin/sh

SYSVINCLUDEHDRS	=shlib.h

all install:	$(SYSVINCLUDEHDRS)

clean:
	rm -f $(SYSVINCLUDEHDRS)

clobber:

generic:
	$(GENERIC) -h $(HDRS)

$(SYSVINCLUDEHDRS): ../../../lib/libc/include/$@
	[ ! -r $@ ] || rm -f $@
	ln -s ../../../lib/libc/include/$@ $@

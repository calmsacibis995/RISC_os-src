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
# $Header: Makefile,v 1.3.1.3 90/05/10 04:19:32 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

WHEREIGO=$(DESTROOT)/posix/lib/locale/C/


SRCS	=ASCII cftime

WHOAMI	=LC_CTYPE LC_NUMERIC LC_TIME

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

all: $(WHOAMI)

install: $(WHOAMI)
	-mkdir -p $(WHEREIGO) >/dev/null 2>&1 || exit 0
	for i in $(WHOAMI); do \
		$(INSTALL) $(INSTOPTS) $$i $(WHEREIGO) ; \
	done

#
# NOTE: we must use a version of chrtbl built on the current system,
#	not one from the build tree...
#
LC_CTYPE:	ASCII
	$(COMP_HOST_ROOT)/posix/bin/chrtbl ASCII
	-cp ascii LC_CTYPE

LC_NUMERIC:	ASCII
	$(COMP_HOST_ROOT)/posix/bin/chrtbl ASCII
	-cp num_ascii LC_NUMERIC

LC_TIME:	cftime
	-cp cftime LC_TIME

clean:
	rm -f ctype.c num_ascii ascii

clobber:	clean
	rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -R$(DESTROOT) -D$(WHEREIGO)

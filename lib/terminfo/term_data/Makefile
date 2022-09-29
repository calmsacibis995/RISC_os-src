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
# $Header: Makefile,v 1.5.2.2 90/05/10 02:54:14 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

WHEREIGO=$(DESTROOT)/usr/lib/terminfo

#
# NOTE: Source order is important here with respect to header and trailer.
#

SRCS	=header adds.ti annarbor.ti ansi.ti att.ti beehive.ti cdc.ti \
	colorscan.ti datamedia.ti dec.ti diablo.ti \
	emacsvirtual.ti \
	fortune.ti general.ti \
	hardcopy.ti hazeltine.ti hds.ti heath.ti homebrew.ti hp.ti lsi.ti \
	microterm.ti misc.ti pc.ti print.ti sgi.ti special.ti sperry.ti \
	tektronix.ti teleray.ti televideo.ti ti.ti tymshare.ti v50am.ti \
	visual.ti window.ti wyse.ti xterm.ti trailer

WHOAMI	=terminfo.src

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

all: $(WHOAMI)

install: $(WHOAMI)
	-mkdir -p $(WHEREIGO) >/dev/null 2>&1 || exit 0
	TERMINFO=$(WHEREIGO) tic terminfo.src

$(WHOAMI): $(SRCS)
	cat $(SRCS) > terminfo.src

clean:

clobber:
	rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -R$(DESTROOT) -D$(WHEREIGO)

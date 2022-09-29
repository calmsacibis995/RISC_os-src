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
# $Header: Makefile,v 1.3.2.2 90/05/07 19:15:37 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

WHEREIGO=$(DESTROOT)/usr/lib/tmac

SRCS	=\
	tmac.a tmac.an tmac.an.new tmac.an6n tmac.an6t tmac.bib \
	tmac.cp tmac.e tmac.os tmac.r tmac.s tmac.scover tmac.sdisp \
	tmac.skeep tmac.srefs tmac.vcat tmac.vgrind

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

all: $(SRCS)

install: $(SRCS)
	for name in $(SRCS) ;\
	{\
		$(INSTALL) $(INSTOPTS) $$name $(WHEREIGO) ;\
	}
	-chmod 0775 $(WHEREIGO)
	-chgrp $(GROUP) $(WHEREIGO)
	-chown $(OWNER) $(WHEREIGO)

clean:

clobber:

generic:
	$(GENERIC) -s $(SRCS) -d $(SRCS) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)


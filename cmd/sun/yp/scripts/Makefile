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
# $Header: Makefile,v 1.1.1.5 90/05/09 19:30:34 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

WHEREIGO=$(DESTROOT)/usr/etc/yp
WHEREIGO2=$(DESTROOT)/etc/yp

SRCS	=ypinit.sh ypxfr_1perday.sh ypxfr_1perhour.sh ypxfr_2perday.sh

WHOAMI	=

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=550
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

all:

install:
	$(INSTALL) $(INSTOPTS) updaters $(WHEREIGO2)
	cp make.script $(WHEREIGO2)/Makefile
	chmod 550 $(WHEREIGO2)/Makefile
	chown bin $(WHEREIGO2)/Makefile
	chgrp bin $(WHEREIGO2)/Makefile
	cp ypinit.sh ypinit
	$(INSTALL) $(INSTOPTS) ypinit $(WHEREIGO)
	cp ypxfr_1perday.sh ypxfr_1perday
	$(INSTALL) $(INSTOPTS) ypxfr_1perday $(WHEREIGO)
	cp ypxfr_1perhour.sh ypxfr_1perhour
	$(INSTALL) $(INSTOPTS) ypxfr_1perhour $(WHEREIGO)
	cp ypxfr_2perday.sh ypxfr_2perday
	$(INSTALL) $(INSTOPTS) ypxfr_2perday $(WHEREIGO)

clean:

clobber:
	-rm -f ypinit ypxfr_1perday ypxfr_1perhour ypxfr_2perday

generic:
	$(GENERIC) -s $(SRCS) -p $(WHOAMI) \
		-R$(DESTROOT) -D$(WHEREIGO)

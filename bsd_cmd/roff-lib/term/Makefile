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
# $Header: Makefile,v 1.3.2.2 90/05/07 19:15:31 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

WHEREIGO=$(DESTROOT)/usr/lib/term

SRCS	=\
	example tab300 tab300-12 tab300X tab300s tab300s-12 \
	tab302 tab302-12 tab37 tab382 tab382-12 tab450 tab450-12 \
	tab450-12-8 tab450X tab833 tab833-12 tabdtc tabdtc12 tabipsi \
	tabipsi12 tabitoh tabitoh12 tablpr tabnec tabnec-t tabnec12 \
	tabqume tabqume12 tabtn300 tabx-ecs tabx-ecs12 tabx1700 \
	tabxerox tabxerox12

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


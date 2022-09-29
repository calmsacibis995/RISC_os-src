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
# $Header: Makefile,v 1.3.2.2 90/05/07 19:15:25 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

WHEREIGO=$(DESTROOT)/usr/lib/ms

MSSRCS	=\
	README end.awk s.acc s.cov s.eqn s.ref s.tbl s.ths s.toc
CMDSRCS =\
	endnote
SRCS	= $(MSSRCS) $(CMDSRCS)

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)
CMDINSTOPTS=$(ICOPY) -m 555 -o $(OWNER) -g $(GROUP)

all: $(SRCS)

install: $(SRCS)
	for name in $(MSSRCS) ;\
	{\
		$(INSTALL) $(INSTOPTS) $$name $(WHEREIGO) ;\
	}
	for name in $(CMDSRCS) ; \
	{\
		$(INSTALL) $(CMDINSTOPTS) $$name $(WHEREIGO) ;\
	}
	-chmod 0775 $(WHEREIGO)
	-chgrp $(GROUP) $(WHEREIGO)
	-chown $(OWNER) $(WHEREIGO)

clean:

clobber:

generic:
	$(GENERIC) -s $(SRCS) -d $(SRCS) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)


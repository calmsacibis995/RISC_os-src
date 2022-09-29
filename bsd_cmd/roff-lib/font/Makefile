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
# $Header: Makefile,v 1.2.2.2 90/05/07 19:15:13 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

WHEREIGO=$(DESTROOT)/usr/lib/font

SRCS	=\
	ftB ftBC ftC ftCE ftCI ftCK ftCW ftFD ftG ftGI ftGM ftGR ftH ftHI \
	ftHM ftI ftL \
	ftLI ftPA ftPB ftPI ftR ftS ftSB ftSI ftSM ftSb ftSi ftSr ftTB \
	ftTI ftTR ftTS ftUD ftap ftbb ftbi ftbk ftbr ftcb ftch ftci ftcl ftco \
	ftcr ftct ftcy ftdb ftdi ftdr ftfx ftgb ftgf ftgi ftgk ftgr fthb fthn \
	ftm ftmb ftmi ftmn ftmr ftnb ftni ftnr ftob ftoe ftoi ftor ftpb ftpp \
	ftsb ftsc ftsg ftsh ftsi ftsl ftsr fttB fttI fttR fttS fttb ftti fttr \
	ftts ftwp ftxx

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


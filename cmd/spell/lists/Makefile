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
# $Header: Makefile,v 1.3.2.2 90/05/09 19:04:22 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc
ULIBDIR	=/usr/lib

HASHMAKE=$(ULIBDIR)/spell/hashmake
SPELLIN	=$(ULIBDIR)/spell/spellin

DESTROOT=

WHEREIGO=$(DESTROOT)/usr/lib/spell

SRCS	=american british extra list local stop

WHOAREWE=hlista hlistb hstop

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

all: $(WHOAREWE)

install: $(WHOAREWE)
	for name in $(WHOAREWE) ;\
	{\
		$(INSTALL) $(INSTOPTS) "$$name" $(WHEREIGO) ;\
	}

clean:

clobber:
	-rm -f $(WHOAREWE)

generic:
	$(GENERIC) -s $(SRCS) -d $(WHOAREWE) \
		-R$(DESTROOT) -D$(WHEREIGO)

hlista: list local extra american
	-rm -f htemp
	cat list local extra american | $(HASHMAKE) | sort -u > htemp
	$(SPELLIN) `wc htemp | sed -n 's/\([^ ]\) .*/\1/p'` < htemp > hlista
	-rm -f htemp

hlistb: list local extra british
	-rm -f htemp
	cat list local extra british | $(HASHMAKE) | sort -u > htemp
	$(SPELLIN) `wc htemp | sed -n 's/\([^ ]\) .*/\1/p'` < htemp > hlistb
	-rm -f htemp

hstop: stop
	-rm -f htemp
	cat stop | $(HASHMAKE) | sort -u > htemp
	$(SPELLIN) `wc htemp | sed -n 's/\([^ ]\) .*/\1/p'` < htemp > hstop
	-rm -f htemp

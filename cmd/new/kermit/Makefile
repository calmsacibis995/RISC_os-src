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
# $Header: Makefile,v 1.2.1.3 90/05/09 17:45:27 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O1
INCLUDE	=
CFLAGS	=$(MAXOPT) $(INCLUDE) $(ECFLAGS) -DSVR3 -DUXIII -DDEBUG -DTLOG"
LNKFLAGS=
ASFLAGS =$(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/usr/new
DOCSGO=$(DESTROOT)/usr/new/lib

SRCS	=

HDRS	=

WHOAMI	=kermit

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
DOCMODE =444
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP)
DOCOPTS=$(ICOPY) -m $(DOCMODE) -o $(OWNER) -g $(GROUP)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)
	$(INSTALL) $(DOCOPTS) kermit.doc $(DOCSGO)

$(WHOAMI): wermit
	cp wermit $(WHOAMI)
	cp ckuker.doc kermit.doc

wermit:
	$(MAKE) -f ckuker.mak wermit CFLAGS="$(CFLAGS)" LNKFLAGS="$(LNKFLAGS)"

clean:
	$(MAKE) -f ckuker.mak clean CFLAGS="$(CFLAGS)"

clobber: clean
	-rm -f $(WHOAMI) wart kermit.doc

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)


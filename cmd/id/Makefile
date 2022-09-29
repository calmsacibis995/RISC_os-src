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
# $Header: Makefile,v 1.3.2.3 90/05/09 16:14:30 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=$(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/usr/bin

OBJS	=id.o

SRCS	=id.c

HDRS	=

WHOAMI	=id

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
LINK	=-l $(DESTROOT)/usr/ucb/whoami
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP) $(LINK)

all: $(WHOAMI)

install: $(WHOAMI)
	ID=`id +%U` ; \
	for dir in usr usr/ucb ; \
	{\
		[ -r $(DESTROOT)/$$dir ] || \
			( ( mkdir -p $(DESTROOT)/$$dir || exit 0) ; \
			  [ $$ID != 0 ] || \
			  ( chgrp $(GROUP) $(DESTROOT)/$$dir ; \
			    chmod 0775 $(DESTROOT)/$$dir ; \
			    chown $(OWNER) $(DESTROOT)/$$dir ) ) ; \
	} ; \
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)

$(WHOAMI): $(OBJS) $(LOCAL_LIBS)
	$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) $(LOCAL_LIBS) $(LIBRARIES)

clean:
	-rm -f $(OBJS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO) \
		-l /usr/ucb/whoami

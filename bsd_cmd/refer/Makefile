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
# $Header: Makefile,v 1.1.2.3 90/05/07 19:09:14 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=-systype bsd43 $(MAXOPT) $(INCLUDE) $(ECFLAGS) -w -DRISCOS
ASFLAGS =-systype bsd43 $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/bsd43/bin

OBJS	=deliv2.o glue1.o glue2.o glue3.o glue4.o glue5.o hunt2.o hunt3.o hunt5.o hunt6.o hunt7.o hunt8.o hunt9.o mkey3.o refer..o refer0.o refer1.o refer2.o refer4.o refer5.o refer6.o refer7.o refer8.o shell.o

SRCS	=deliv2.c glue1.c glue2.c glue3.c glue4.c glue5.c hunt2.c hunt3.c hunt5.c hunt6.c hunt7.c hunt8.c hunt9.c mkey3.c refer..c refer0.c refer1.c refer2.c refer4.c refer5.c refer6.c refer7.c refer8.c shell.c

HUNTSRCS = deliv2.c glue4.c glue5.c hunt1.c hunt2.c hunt3.c hunt5.c hunt6.c \
	hunt7.c hunt8.c hunt9.c refer3.c shell.c tick.c
HUNTOBJS = $(HUNTSRCS:.c=.o)

INVSRCS	=inv1.c inv2.c inv3.c inv5.c inv6.c deliv2.c
INVOBJS =$(INVSRCS:.c=.o)

MKEYSRCS=mkey1.c mkey2.c mkey3.c deliv2.c
MKEYOBJS=$(MKEYSRCS:.c=.o)

HDRS	=

WHOAMI	=refer

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP)
REFERLIB=$(DESTROOT)/bsd43/usr/lib/refer

all: $(WHOAMI) hunt inv mkey

install: $(WHOAMI) hunt inv mkey
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)
	ID=`id +%U`; \
	[ -d $(REFERLIB) ] || ( ( mkdir -p $(REFERLIB) || exit 0) ;\
	[ $$ID != 0 ] || \
	( chgrp $(GROUP) $(REFERLIB) ; \
	  chmod 0775 $(REFERLIB) ; \
	  chown $(OWNER) $(REFERLIB) ) )
	$(INSTALL) $(INSTOPTS) hunt $(REFERLIB)
	$(INSTALL) $(INSTOPTS) inv $(REFERLIB)
	$(INSTALL) $(INSTOPTS) mkey $(REFERLIB)

$(WHOAMI): $(OBJS) $(LOCAL_LIBS)
	$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) $(LOCAL_LIBS) $(LIBRARIES)

hunt: $(HUNTOBJS)
	$(CC) $(CFLAGS) -o hunt $(HUNTOBJS)

inv: $(INVOBJS)
	$(CC) $(CFLAGS) -o inv $(INVOBJS)

mkey: $(MKEYOBJS)
	$(CC) $(CFLAGS) -o mkey $(MKEYOBJS)
clean:
	-rm -f $(OBJS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

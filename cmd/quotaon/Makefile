#
# $Header: Makefile,v 1.3.1.3.1.2 90/07/11 18:08:27 hawkes Exp $
# ---------------------------------------------------
# | Copyright (c) 1987 MIPS Computer Systems, Inc.  |
# | All Rights Reserved.                            |
# ---------------------------------------------------
#

SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=$(MAXOPT) $(INCLUDE) $(ECFLAGS) -DRISCOS -systype bsd43
ASFLAGS =$(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/usr/etc

OBJS	=quotaon.o

SRCS	=quotaon.c

HDRS	=

WHOAMI	=quotaon

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
LINK	=-l $(DESTROOT)/etc/quotaon
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP) $(LINK)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)
	-rm -f $(WHEREIGO)/quotaoff
	-ln $(WHEREIGO)/quotaon $(WHEREIGO)/quotaoff
	-rm -f $(DESTROOT)/etc/quotaoff
	-ln -s ../usr/etc/quotaoff $(DESTROOT)/etc/quotaoff

$(WHOAMI): $(OBJS) $(LOCAL_LIBS)
	$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) $(LOCAL_LIBS) $(LIBRARIES)

clean:
	-rm -f $(OBJS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO) \
		-l $(DESTROOT)/etc/quotaon $(DESTROOT)/etc/quotaoff

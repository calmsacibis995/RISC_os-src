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
# $Header: Makefile,v 1.3.1.2 90/05/07 19:49:34 wje Exp $

SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=-I$(DESTROOT)/usr/include/sun -I$(DESTROOT)/usr/include/bsd \
	 -I../../lib/libmips
CFLAGS	=-systype sysv $(MAXOPT) $(INCLUDE) -DRISCOS $(ECFLAGS)
ASFLAGS =-systype sysv $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=-lsun -lbsd -lmld -lm
LOCAL_LIBS=../../lib/libmips/sysv/libmips.a

WHEREIGO=$(DESTROOT)/usr/ucb

SRCS=	vmstat.c

OBJS	=$(SRCS:.c=.o)

HDRS	=

WHOAMI	=vmstat

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=2555
OWNER	=bin
GROUP	=sys
ISTRIP	=-s
ICOPY	=-c
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP)

CTAGS	=/usr/new/ctags
ETAGS	=/usr/new/etags

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)

$(WHOAMI): $(OBJS) $(LOCAL_LIBS)
	$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) $(LOCAL_LIBS) $(LIBRARIES)

# $(OBJS): $(HDRS)

clean:
	-rm -f $(OBJS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

FRC:

tags:	FRC
	[ ! -r tags ] || rm -f tags
	$(CTAGS) $(SRCS) $(HDRS) 
	
TAGS:	FRC
	[ ! -r TAGS ] || rm -f TAGS
	$(ETAGS) $(SRCS) $(HDRS) 
	
	
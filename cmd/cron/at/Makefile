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
# $Header: Makefile,v 1.4.2.2 90/05/09 15:29:44 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=-I../include
CFLAGS	=$(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=-d $(EYFLAGS)

LIBRARIES=
LOCAL_LIBS=../a_cron/libcron.a ../a_elm/libelm.a

WHEREIGO=$(DESTROOT)/usr/bin

OBJS	=at.o att1.o att2.o

SRCS	=at.c att1.y att2.l att2.ed

HDRS	=

WHOAMI	=at

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=4555
OWNER	=root
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)

$(WHOAMI): $(OBJS) $(LOCAL_LIBS)
	$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) $(LOCAL_LIBS) $(LIBRARIES)

clean:
	-rm -f $(OBJS) att1.c att1.h att2.c

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

att1.c att1.h:	att1.y
	-rm -f att1.c att1.h
	$(YACC) $(YFLAGS) -d att1.y
	mv y.tab.c att1.c
	mv y.tab.h att1.h

att2.c:	att2.l
	rm -f att2.c
	lex att2.l
	ed - lex.yy.c < att2.ed >/dev/null
	mv lex.yy.c att2.c

att2.o:	att1.h

at.o:	at.c ../include/cron.h

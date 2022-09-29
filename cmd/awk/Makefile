#
# $Header: Makefile,v 1.7.2.2 90/01/11 17:50:33 wje Exp $
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
CFLAGS	=$(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=-d $(EYFLAGS)

LOCAL_CC=cc

LIBRARIES=-lm
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/bin

OBJS	=awk.g.o awk.lx.o b.o lib.o main.o parse.o proctab.o \
	run.o tran.o

SRCS	=awk.g.y awk.lx.l b.c lib.c main.c makeprctab.c \
	parse.c run.c tran.c

HDRS	=awk.h

WHOAMI	=awk

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
LINK	=-l $(DESTROOT)/usr/bin/awk
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP) $(LINK)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)

$(WHOAMI): $(OBJS)
	$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) $(LOCAL_LIBS) $(LIBRARIES)

clean:
	-rm -f $(OBJS)
	-rm -f makeprctab makeprctab.o y.tab.h awk.g.c lex.yy.c proctab.c temp

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO) -l /usr/bin/awk

$(OBJS):	$(HDRS) y.tab.h

proctab.c:	./makeprctab
	./makeprctab > proctab.c

./makeprctab: makeprctab.o
	$(LOCAL_CC) -o ./makeprctab makeprctab.o

makeprctab.o: makeprctab.c y.tab.h
	$(LOCAL_CC) -c makeprctab.c

awk.g.o:	awk.h awk.g.c

y.tab.h awk.g.c: awk.g.y
	@echo "Expect 38 shift/reduce conflicts"
	-$(YACC) $(YFLAGS) awk.g.y
	-rm -f awk.g.c
	mv y.tab.c awk.g.c

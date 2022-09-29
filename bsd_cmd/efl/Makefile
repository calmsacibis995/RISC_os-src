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
# $Header: Makefile,v 1.1.1.2 90/05/07 18:21:43 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=-systype bsd43 -w $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =-systype bsd43 $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/bsd43/bin

OBJS	=addr.o alloc.o blklab.o dcl.o dclgen.o error.o exec.o field.o free.o gram.o icfile.o init.o io.o lex.o main.o misc.o mk.o namgen.o pass2.o print.o simple.o struct.o symtab.o tailor.o temp.o

SRCS	=addr.c alloc.c blklab.c dcl.c dclgen.c error.c exec.c field.c free.c gram.c icfile.c init.c io.c lex.l main.c misc.c mk.c namgen.c pass2.c print.c simple.c struct.c symtab.c tailor.c temp.c

HDRS	=

WHOAMI	=efl

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=555
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)

$(WHOAMI): $(OBJS) $(LOCAL_LIBS)
	$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) $(LOCAL_LIBS) $(LIBRARIES)

# gram.c can no longer be made on a pdp11 because of yacc limits
gram.c: gram.head gram.dcl gram.expr gram.exec tokdefs
	(sed < tokdefs "s/#define/%token/" ; \
	cat gram.head gram.dcl gram.expr gram.exec) > gram.in
	${YACC} ${YFLAGS} gram.in
	(echo "#define YYDEBUG 1"; cat y.tab.c) > gram.c
	rm -f gram.in y.tab.c

lex.c: fixuplex
	lex lex.l
	sh fixuplex
	mv lex.yy.c lex.c

tokdefs: tokens
	grep -n . < tokens | sed "s/\([^:]*\):\(.*\)/#define \2 \1/" > tokdefs

clean:
	-rm -f $(OBJS) efl gram.c lex.c tokdefs

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

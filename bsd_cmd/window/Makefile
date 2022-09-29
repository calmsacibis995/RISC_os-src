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
# $Header: Makefile,v 1.2.2.2 90/05/07 19:51:16 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=-signed -systype bsd43 -DRISCOS $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =-systype bsd43 $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=-ltermlib
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/bsd43/bin

OBJS	=char.o cmd.o cmd1.o cmd2.o cmd3.o cmd4.o cmd5.o cmd6.o cmd7.o context.o error.o lcmd.o lcmd1.o lcmd2.o main.o mloop.o parser1.o parser2.o parser3.o parser4.o parser5.o scanner.o startup.o string.o ttf100.o ttgeneric.o tth19.o tth29.o ttinit.o ttoutput.o tttermcap.o tttvi925.o ttwyse60.o ttwyse75.o ttzentec.o var.o win.o wwadd.o wwalloc.o wwbox.o wwchild.o wwclose.o wwclreol.o wwclreos.o wwcursor.o wwdata.o wwdelchar.o wwdelete.o wwdelline.o wwdump.o wwend.o wwenviron.o wwerror.o wwflush.o wwframe.o wwgets.o wwinit.o wwinschar.o wwinsline.o wwiomux.o wwlabel.o wwmisc.o wwmove.o wwopen.o wwprintf.o wwpty.o wwputc.o wwputs.o wwredraw.o wwredrawwin.o wwrint.o wwscroll.o wwsize.o wwspawn.o wwsuspend.o wwtty.o wwunframe.o wwupdate.o wwwrite.o

SRCS	=char.c cmd.c cmd1.c cmd2.c cmd3.c cmd4.c cmd5.c cmd6.c cmd7.c context.c error.c lcmd.c lcmd1.c lcmd2.c main.c mloop.c parser1.c parser2.c parser3.c parser4.c parser5.c scanner.c startup.c string.c ttf100.c ttgeneric.c tth19.c tth29.c ttinit.c ttoutput.c tttermcap.c tttvi925.c ttwyse60.c ttwyse75.c ttzentec.c var.c win.c wwadd.c wwalloc.c wwbox.c wwchild.c wwclose.c wwclreol.c wwclreos.c wwcursor.c wwdata.c wwdelchar.c wwdelete.c wwdelline.c wwdump.c wwend.c wwenviron.c wwerror.c wwflush.c wwframe.c wwgets.c wwinit.c wwinschar.c wwinsline.c wwiomux.c wwlabel.c wwmisc.c wwmove.c wwopen.c wwprintf.c wwpty.c wwputc.c wwputs.c wwredraw.c wwredrawwin.c wwrint.c wwscroll.c wwsize.c wwspawn.c wwsuspend.c wwtty.c wwunframe.c wwupdate.c wwwrite.c

HDRS	=alias.h char.h context.h defs.h lcmd.h local.h parser.h string.h token.h tt.h value.h var.h ww.h

WHOAMI	=window

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

clean:
	-rm -f $(OBJS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

$(OBJS):	$(HDRS)

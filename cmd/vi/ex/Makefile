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
# $Header: Makefile,v 1.7.1.2 90/05/10 00:48:04 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O1
INCLUDE	=-I../include
DEFS	=-DCRYPT -DUSG -DSTDIO -DSINGLE -DTABS=8 -DVMUNIX -DCHDIR -DRISCOS
CFLAGS	=$(MAXOPT) $(DEFS) $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=-lcurses -lgen -lcrypt
LOCAL_LIBS=../a_extern/libextern.a

WHEREIGO=$(DESTROOT)/usr/bin

OBJS	=ex_extern.o \
	bcopy.o ex.o ex_addr.o ex_cmds.o ex_cmds2.o ex_cmdsub.o ex_data.o \
	ex_get.o ex_io.o ex_put.o ex_re.o ex_set.o ex_subr.o ex_temp.o \
	ex_tty.o ex_unix.o ex_v.o ex_vadj.o ex_vget.o ex_vmain.o ex_voper.o \
	ex_vops.o ex_vops2.o ex_vops3.o ex_vput.o ex_vwind.o printf.o \
	ex_fstk.o

SRCS	=ex_extern.c \
	bcopy.c ex.c ex_addr.c ex_cmds.c ex_cmds2.c ex_cmdsub.c ex_data.c \
	ex_get.c ex_io.c ex_put.c ex_re.c ex_set.c ex_subr.c ex_temp.c \
	ex_tty.c ex_unix.c ex_v.c ex_vadj.c ex_vget.c ex_vmain.c ex_voper.c \
	ex_vops.c ex_vops2.c ex_vops3.c ex_vput.c ex_vwind.c printf.c \
	ex_fstk.c

HDRS	=

WHOAMI	=ex

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=1555
OWNER	=bin
GROUP	=bin
ISTRIP	=-s
ICOPY	=-c
LINKS	=-L $(DESTROOT)/usr/bin/vi -L $(DESTROOT)/usr/bin/view
INSTOPTS=$(ICOPY) $(ISTRIP) -m $(MODE) -o $(OWNER) -g $(GROUP) $(LINKS)

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
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO) \
		-L /usr/bin/vi -L /usr/bin/view

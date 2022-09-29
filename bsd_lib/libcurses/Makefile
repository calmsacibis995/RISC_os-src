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
# $Header: Makefile,v 1.3.1.3 90/05/07 21:33:11 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

DEBUG	=
MAXOPT	=-O2
INCLUDE	=
CFLAGS	=$(DEBUG) -systype bsd43 -DNOBLIT $(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(DEBUG) -systype bsd43 $(GNUM) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

WHEREIGO=$(DESTROOT)/bsd43/usr/lib
HDRWHEREIGO=$(DESTROOT)/bsd43/usr/include

OBJS	=addbytes.o addch.o addstr.o box.o clear.o clrtobot.o clrtoeol.o cr_put.o cr_tty.o curses.o delch.o deleteln.o delwin.o endwin.o erase.o fullname.o getch.o getstr.o id_subwins.o idlok.o initscr.o insch.o insertln.o longname.o move.o mvprintw.o mvscanw.o mvwin.o newwin.o overlay.o overwrite.o printw.o putchar.o refresh.o scanw.o scroll.o standout.o toucholap.o touchwin.o tstp.o unctrl.o

SRCS	=addbytes.c addch.c addstr.c box.c clear.c clrtobot.c clrtoeol.c cr_put.c cr_tty.c curses.c delch.c deleteln.c delwin.c endwin.c erase.c fullname.c getch.c getstr.c id_subwins.c idlok.c initscr.c insch.c insertln.c longname.c move.c mvprintw.c mvscanw.c mvwin.c newwin.c overlay.c overwrite.c printw.c putchar.c refresh.c scanw.c scroll.c standout.c toucholap.c touchwin.c tstp.c unctrl.c

# files from ../libtermlib, here for backwards compatibility
# previous versions of libcurses had these.
TERMSRCS = termcap.c tgoto.c tputs.c
TERMOBJS = termcap.o tgoto.o tputs.o

HDRS	=curses.h curses.ext

LIBSUFFIX=
WHOAMI	=libcurses$(LIBSUFFIX).a

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)
	$(INSTALL) $(INSTOPTS) curses.h $(DESTROOT)/bsd43/usr/include

install-headers: $(HDRS)
	$(INSTALL) $(INSTOPTS) curses.h $(HDRWHEREIGO)

$(WHOAMI): $(OBJS) $(TERMOBJS)
	$(AR) $(ARFLAGS) $(WHOAMI) $(OBJS) $(TERMOBJS)

clean:
	-rm -f $(OBJS) $(TERMOBJS)
	-rm -f $(TERMSRCS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

$(OBJS):	$(HDRS)

$(TERMOBJS):	$(TERMSRCS)

$(TERMSRCS): ../libtermlib/$@
	[ ! -r $@ ] || rm -f $@
	ln -s ../libtermlib/$@ $@

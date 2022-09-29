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
# $Header: Makefile,v 1.2.1.2 90/05/09 19:40:32 wje Exp $
#
# WARNING:  This make will fail if sources and include files in
#           ../../../lib/libcurses/{include,screen} are not present.
#           If libcurses has not yet built curses.h and the 
#           source files required by this build (i.e., TSRCS),
#           you will have to make them by running the make in the
#	    libcurses/setup source directory.  Then rerun this
#	    make.

SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=-I../include -I../../../lib/libcurses/include
CFLAGS	=$(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

LIBRARIES=
LOCAL_LIBS=

WHEREIGO=$(DESTROOT)/usr/bin

OBJS	=otic.o tnames.o tinames.o tifnames.o unctrl.o

SRCS	=otic.c

TSRCS	=tnames.c tinames.c tifnames.c unctrl.c

HDRS	=

WHOAMI	=otic

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
	-rm -f $(TSRCS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

$(TSRCS): ../../../lib/libcurses/screen/$@
	[ ! -r $@ ] || rm -f $@
	ln -s ../../../lib/libcurses/screen/$@ .

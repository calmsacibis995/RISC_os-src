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
# $Header: Makefile,v 1.4.1.4 90/05/10 04:16:45 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=-I../include
CFLAGS	=-systype posix $(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =-systype posix $(GNUM) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

SYSVPRINTSRCS	=doprnt.c fprintf.c printf.c sprintf.c vprintf.c vfprintf.c \
		vsprintf.c

SYSVPRINTHDRS	=print.h

SYSVPRINTOBJS =$(SYSVPRINTSRCS:.c=.o)

REMOTESRCS=$(SYSVPRINTSRCS)

REMOTEOBJS=$(SYSVPRINTOBJS)

SRCS= $(LOCALSRCS) $(REMOTESRCS)

OBJS= $(LOCALOBJS) $(REMOTEOBJS)

HDRS	= $(SYSVPRINTHDRS)

NAMEMAPPING=../include/name_mapping.h

WHOAMI	=libc_print.port.a
INTEGDIR=../libc

all install: $(INTEGDIR)/$(WHOAMI)

$(INTEGDIR)/$(WHOAMI): $(WHOAMI)
	-rm -f $(INTEGDIR)/$(WHOAMI)
	cp $(WHOAMI) $(INTEGDIR)

$(WHOAMI): $(OBJS)
	-rm -f $(WHOAMI)
	$(AR) $(ARFLAGS) $(WHOAMI) $(OBJS)

clean:
	-rm -f $(OBJS)
	-rm -f $(REMOTESRCS)

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(INTEGDIR)

$(SYSVPRINTHDRS): ../../../lib/libc/print/$@
	[ ! -r $@ ] || rm -f $@
	ln -s ../../../lib/libc/print/$@ $@

$(SYSVPRINTSRCS):
	[ ! -r $@ ] || rm -f $@
	echo '#include "$(NAMEMAPPING)"' > $@
	echo '#include "../../../lib/libc/print/$@"' >> $@

$(REMOTESRCS): Makefile

$(REMOTEOBJS): $(NAMEMAPPING)

$(SYSVPRINTOBJS):
	$(CC) $(CFLAGS) -c $<

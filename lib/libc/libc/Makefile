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
# $Header: Makefile,v 1.6.2.2 90/05/10 01:42:21 wje Exp $
#
SHELL	=/bin/sh
ETCDIR	=/etc

DESTROOT=

MAXOPT	=-O2
INCLUDE	=
CFLAGS	=$(GNUM) $(MAXOPT) $(INCLUDE) $(ECFLAGS)
ASFLAGS =$(GNUM) $(EASFLAGS)
LFLAGS	=$(ELFLAGS)
YFLAGS	=$(EYFLAGS)

AR = ar
ARFLAGS	=crs

WHEREIGO=$(DESTROOT)/usr/lib

OBJS	=

SRCS	=

HDRS	=

LIBSUFFIX=
WHOAMI	=libc$(LIBSUFFIX).a

INSTALL	=$(ETCDIR)/mipsinstall
MODE	=444
OWNER	=bin
GROUP	=bin
ICOPY	=-c
INSTOPTS=$(ICOPY) -m $(MODE) -o $(OWNER) -g $(GROUP)

all: $(WHOAMI)

install: $(WHOAMI)
	$(INSTALL) $(INSTOPTS) $(WHOAMI) $(WHEREIGO)

#
# As nice as it would be, it is not possible to only extract the
# members from the changed archives, since the order is important.
#
# The reason for the dependency is just to make sure that if nothing
# has changed, the archive isn't rebuilt.
#

$(WHOAMI):  libc_*.port.a libc_*.mips.a
	-rm -f *.o
	for i in libc_*.port.a libc_*.mips.a ;\
	{\
		$(AR) x $$i ;\
		rm -f _____* ;\
	}
	$(AR) $(ARFLAGS) $(WHOAMI) *.o

clean:
	-rm -f libc_*.a *.o

clobber: clean
	-rm -f $(WHOAMI)

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) \
		$(INCLUDE) -R$(DESTROOT) -D$(WHEREIGO)

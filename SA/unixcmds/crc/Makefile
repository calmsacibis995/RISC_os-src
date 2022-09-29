#ident "$Header: Makefile,v 1.6.10.1 90/07/18 17:14:55 huang Exp $"
#
# $Header: Makefile,v 1.6.10.1 90/07/18 17:14:55 huang Exp $
# ------------------------------------------------------------------
# | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights |
# | Reserved.  This software contains proprietary and confidential |
# | information of MIPS and its suppliers.  Use, disclosure or     |
# | reproduction is prohibited without the prior express written   |
# | consent of MIPS.                                               |
# ------------------------------------------------------------------
#
# This makefile is written to be generic, but the following macros should
# generally be re-defined by the caller.
#
#	DESTDIR	The root of the destination directory.
#

include ../../Mkinc.common_defines
include ../../Mkinc.release
include ../../Mkinc.common_targets

DEPTH=../..
DESTDIR=$(DESTROOT)

DEBUG	=
INCLUDE	=
DEF_CFLAGS=-p0 -O
CFLAGS	=$(DEF_CFLAGS) $(DEBUG) $(INCLUDE) -g

LIBRARIES=-lbsd

WHEREIGO=$(DESTDIR)/usr/spp

OBJS	=crc.o

SRCS	=crc.c

HDRS	=

WHOAMI	=crc

all: $(WHOAMI)

install: $(WHOAMI)
	-if [ ! -d $(DESTDIR)/usr/spp ]; \
	then \
		$(IF_SYSV) \
			mkdir -p $(DESTDIR)/usr/spp; \
		$(ENDIF) \
		$(IF_BSD) \
			mkdir $(DESTDIR)/usr; \
			mkdir $(DESTDIR)/usr/spp; \
		$(ENDIF) \
	fi
	$(IF_SYSV) \
		$(MAKE) package_x_install SRC=$(WHOAMI) DEST=$(WHEREIGO); \
	$(ENDIF)
	$(IF_BSD) \
		$(INSTALL) -c -s $(WHOAMI) $(DESTDIR)/usr/spp; \
	$(ENDIF)
	-if [ ! -d $(DESTDIR)/usr/lib ]; \
	then \
		$(IF_SYSV) \
			mkdir -p $(DESTDIR)/usr/lib; \
		$(ENDIF) \
		$(IF_BSD) \
			mkdir $(DESTDIR)/usr; \
			mkdir $(DESTDIR)/usr/lib; \
		$(ENDIF) \
	fi
	-rm -f $(DESTDIR)/usr/lib/crc.script
	cp crc.script $(DESTDIR)/usr/lib

$(WHOAMI): $(OBJS)
	$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) $(LIBRARIES)

clean:
	-rm -f $(OBJS)

clobber: clean
	-rm -f $(WHOAMI)

remake: clobber depend tags
	$(MAKE) $(MAKEARGS) all

tags:	FRC
	-rm -f tags
	ctags $(SRCS) >/dev/null 2>&1

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) $(INCLUDE)

depend:
	-rm -f makedep eddep
	$(IF_SYSV) \
		$(CC) -M -I$(ROOT)/usr/include/bsd -I$(ROOT)/usr/include \
		$(CFLAGS) $(SRCS) | \
		awk -f $(AWKDEPEND) - > makedep ; \
	$(ENDIF)
	$(IF_BSD) \
		$(CC) -M $(CFLAGS) $(SRCS) | \
		awk -f $(AWKDEPEND) - > makedep ; \
	$(ENDIF)
	echo '/^# DO NOT DELETE THIS LINE/+1,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w Makefile' >>eddep
	-rm -f Makefile.bak
	cp Makefile Makefile.bak
	-rm -f Makefile
	ex - Makefile.bak < eddep
	-rm -f eddep makedep


.c.o:
	$(IF_SYSV) \
		$(CC) -I$(ROOT)/usr/include/bsd -I$(ROOT)/usr/include \
		$(CFLAGS) -c $< ; \
	$(ENDIF)
	$(IF_BSD) \
		$(CC) $(CFLAGS) -c $< ; \
	$(ENDIF)

FRC:

# DO NOT DELETE THIS LINE -- make depend uses it


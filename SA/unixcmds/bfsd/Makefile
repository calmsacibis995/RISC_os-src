#ident "$Header: Makefile,v 1.19 90/05/10 17:16:07 menna Exp $"
#
# $Header: Makefile,v 1.19 90/05/10 17:16:07 menna Exp $
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
INCLUDE	=-I$(ROOT)/usr/include -I../../include
DEF_CFLAGS=-p0 -O -Dsgi
CFLAGS	=$(DEF_CFLAGS) $(DEBUG) $(INCLUDE)

LIBRARIES= 

WHEREIGO=$(DESTDIR)/usr/etc

OBJS	=bfsd.o
SRCS	=bfsd.c
HDRS	=
WHOAMI	=bfsd

all: $(WHOAMI)

install: $(WHOAMI)
	-if [ ! -d $(WHEREIGO) ]; \
	then \
		mkdir $(WHEREIGO); \
	fi
	$(IF_SYSV) \
		$(MAKE) package_x_install SRC=$(WHOAMI) DEST=$(WHEREIGO); \
	$(ENDIF)
	$(IF_BSD) \
		$(INSTALL) -c -s $(WHOAMI) $(WHEREIGO); \
	$(ENDIF)

install_umips: all
	strip $(WHOAMI)
	-if [ ! -d $(WHEREIGO) ]; \
	then \
		mkdir $(WHEREIGO); \
	fi
	$(IF_SYSV) \
		$(MAKE) package_x_install SRC=$(WHOAMI) DEST=$(WHEREIGO); \
	$(ENDIF)
	$(IF_BSD) \
		$(INSTALL) -c -s $(WHOAMI) $(WHEREIGO); \
	$(ENDIF)

$(WHOAMI): $(OBJS)
	$(IF_SYSV) \
		$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) -lbsd; \
	$(ENDIF)
	$(IF_BSD) \
		$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) $(LIBRARIES); \
	$(ENDIF)

clean:
	-rm -f $(OBJS)

clobber: clean
	-rm -f $(WHOAMI)

remake: clobber depend tags
	$(MAKE) all

tags:	FRC
	-rm -f tags
	ctags $(SRCS) >/dev/null 2>&1

generic:
	$(GENERIC) -s $(SRCS) -h $(HDRS) -o $(OBJS) -p $(WHOAMI) $(INCLUDE)

depend:
	-rm -f makedep eddep
	$(IF_SYSV) \
		$(CC) -M -DSVR3 -I$(ROOT)/usr/include/bsd \
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

$(OBJS):	$(HDRS)
	$(IF_SYSV) \
		$(CC) -DSVR3 -I$(ROOT)/usr/include/bsd \
		$(CFLAGS) -c bfsd.c; \
	$(ENDIF)
	$(IF_BSD) \
		$(CC) $(CFLAGS) -c bfsd.c; \
	$(ENDIF)

FRC:

# DO NOT DELETE THIS LINE -- make depend uses it

bfsd.o: bfsd.c /usr/include/pwd.h /usr/include/grp.h
bfsd.o: /usr/include/bsd/sys/types.h /usr/include/bsd/sys/../../sys/types.h
bfsd.o: /usr/include/bsd/sys/socket.h /usr/include/bsd/sys/../../sys/socket.h
bfsd.o: /usr/include/sys/stat.h /usr/include/bsd/sys/ioctl.h
bfsd.o: /usr/include/bsd/sys/ttychars.h /usr/include/termio.h
bfsd.o: /usr/include/sys/termio.h /usr/include/bsd/net/soioctl.h
bfsd.o: /usr/include/bsd/sys/types.h /usr/include/bsd/sys/file.h
bfsd.o: /usr/include/bsd/sys/types.h /usr/include/bsd/sys/../../sys/file.h
bfsd.o: /usr/include/sys/fcntl.h /usr/include/sys/fcntl.h
bfsd.o: /usr/include/bsd/net/if.h /usr/include/bsd/net/if_arp.h
bfsd.o: /usr/include/bsd/netinet/in.h /usr/include/stdio.h
bfsd.o: /usr/include/signal.h /usr/include/bsd/sys/signal.h
bfsd.o: /usr/include/bsd/sys/../../sys/signal.h /usr/include/errno.h
bfsd.o: /usr/include/sys/errno.h /usr/include/ctype.h /usr/include/bsd/netdb.h
bfsd.o: /usr/include/bsd/sys/time.h ../../include/saio/bfs.h

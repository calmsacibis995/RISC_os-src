#ident "$Header: Makefile,v 1.11.10.1 90/07/18 16:42:42 huang Exp $"
#
# $Header: Makefile,v 1.11.10.1 90/07/18 16:42:42 huang Exp $
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
DEF_CFLAGS=-p0 -O -Dsgi
CFLAGS	=$(DEF_CFLAGS) $(DEBUG) $(INCLUDE)

LIBRARIES=-lmld

WHEREIGO=$(DESTDIR)/usr/spp

OBJS	=stubs.o convert.o mips.o intel_hex.o s_rec.o 

SRCS	=convert.c mips.c  stubs.c intel_hex.c s_rec.c 

HDRS	=convert.h mips.h  stubs.h intel_hex.h s_rec.h 

WHOAMI	=convert

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

$(WHOAMI): $(OBJS)
	$(CC) $(CFLAGS) -o $(WHOAMI) $(OBJS) $(LIBRARIES)

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

$(OBJS):	$(HDRS)

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

convert.o: convert.c /usr/include/stdio.h /usr/include/bsd/strings.h
convert.o: /usr/include/bsd/../string.h /usr/include/ctype.h
convert.o: /usr/include/a.out.h /usr/include/nlist.h /usr/include/filehdr.h
convert.o: /usr/include/aouthdr.h /usr/include/scnhdr.h /usr/include/reloc.h
convert.o: /usr/include/syms.h /usr/include/symconst.h /usr/include/sym.h
convert.o: /usr/include/cmplrs/stsupport.h /usr/include/sex.h ./convert.h
convert.o: ./stubs.h ./mips.h ./intel_hex.h ./s_rec.h
mips.o: mips.c /usr/include/stdio.h /usr/include/bsd/strings.h
mips.o: /usr/include/bsd/../string.h /usr/include/filehdr.h
mips.o: /usr/include/aouthdr.h /usr/include/scnhdr.h ./convert.h
stubs.o: stubs.c /usr/include/stdio.h
intel_hex.o: intel_hex.c ./convert.h ./intel_hex.h
s_rec.o: s_rec.c ./convert.h ./s_rec.h

#ident "$Header: Makefile,v 1.6 89/03/04 15:49:22 menna Exp $"
#	%Q%	%I%	%M%
#
# Copyright 1985 by MIPS Computer Systems, Inc.
#
# UNIX command makefile
#
VERSION=MIPS
CCOPTS=	-O
ASOPTS=
LDOPTS=
DEPTH=../..

include ../../Mkinc.common_defines
include ../../Mkinc.release

DEFINES= -D$(VERSION) -Dsgi

ASFLAGS= $(INCDIRS) -DLOCORE $(DEFINES) $(ASOPTS) $(CASOPTS)
CFLAGS=	$(INCDIRS) $(DEFINES) $(CCOPTS) $(CCCOPTS)
LDFLAGS= -m $(LDOPTS) $(CLDOPTS)

SHELL=	/bin/sh

VPATH=..
DPATH=..

SFILES=

CFILES=\
	../format.c

FORMATOBJS=\
	format.o

UNIXCMDS=\
	format

all:	$(UNIXCMDS)

#
# Unix level format is not installed for UMIPS-V because it is not
# supported at this time.
#

install: all
		-if [ ! -d $(DESTDIR)/etc ]; \
		then \
			mkdir $(DESTDIR)/etc; \
		fi
		$(IF_BSD) \
			$(INSTALL) -c format $(DESTDIR)/etc; \
		$(ENDIF)

format:		$(FORMATOBJS) stand_vers.o
		-rm -f format format.map format.nm format.$(RELEASE)
		$(IF_SYSV) \
			$(CC) $(LDFLAGS) -o format stand_vers.o \
			    $(FORMATOBJS) -lbsd > format.map; \
		$(ENDIF)
		$(IF_BSD) \
			$(CC) $(LDFLAGS) -o format stand_vers.o \
			    $(FORMATOBJS) > format.map; \
		$(ENDIF)
		$(NM) -n format > format.nm
		$(SIZE) format
		cp format format.$(RELEASE)
		$(IF_SYSV) \
			rm -f format format.$(RELEASE) format.nm; \
		$(ENDIF)

$(FORMATOBJS):	../format.c
		$(IF_SYSV) \
			$(CC) -I$(ROOT)/usr/include/bsd -I$(ROOT)/usr/include \
				-DMAXFRAG=8 $(CFLAGS) -c ../format.c; \
		$(ENDIF)
		$(IF_BSD) \
			$(CC) $(CFLAGS) -c ../format.c; \
		$(ENDIF)

remake:		clean depend
		$(MAKE) all

clobber:	clean
		-rm -f errs
		for i in $(UNIXCMDS); \
		do \
			rm -f $$i*; \
		done

clean:
		-rm -f *.o *.dis Makefile.bak stand_vers.c
		for i in $(UNIXCMDS); \
		do \
			rm -f $$i $$i.nm $$i.map; \
		done

stand_vers.c:	$(FORMATOBJS)
	-rm -f stand_vers.c
	echo "/* Configuration Control File */" > stand_vers.c;
	$(IF_SYSV) \
		echo "static char *SccsId=\"@(#)Mips Computer Systems \c" \
		    >> stand_vers.c; \
		echo "Version $(RELEASE) $(TYPE)" `date` `logname`"\c" \
		    >>stand_vers.c; \
	$(ENDIF)
	$(IF_BSD) \
		echo -n "static char *SccsId=\"@(#)Mips Computer Systems " \
		    >> stand_vers.c; \
		echo -n "Version $(RELEASE) $(TYPE)" `date` `whoami` \
		    >>stand_vers.c; \
	$(ENDIF)
	echo "\";" >> stand_vers.c
	$(IF_SYSV) \
		echo "char *stand_version=\"\c" >> stand_vers.c; \
		echo "Version $(RELEASE) $(TYPE)" `date` `logname`"\c" \
		    >>stand_vers.c; \
	$(ENDIF)
	$(IF_BSD) \
		echo -n "char *stand_version=\"" >> stand_vers.c; \
		echo -n "Version $(RELEASE) $(TYPE)" `date` `whoami` \
		    >>stand_vers.c; \
	$(ENDIF)
	echo "\";" >> stand_vers.c

stand_vers.o:	stand_vers.c
		-rm -f stand_vers.o
		$(CC) $(CFLAGS) -c stand_vers.c

depend:
	-rm -f eddep makedep
	$(IF_SYSV) \
		$(CC) -M -I$(ROOT)/usr/include/bsd -I$(ROOT)/usr/include \
		-DMAXFRAG=8 $(CFLAGS) $(SFILES) $(CFILES) | \
		awk -f $(AWKDEPEND) - > makedep ; \
	$(ENDIF)
	$(IF_BSD) \
		$(CC) -M $(CFLAGS) $(SFILES) $(CFILES) | \
		awk -f $(AWKDEPEND) - > makedep ; \
	$(ENDIF)
	echo '/^# DO NOT DELETE THIS LINE/+1,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w Makefile' >>eddep
	-rm -f Makefile.bak
	cp Makefile Makefile.bak
	rm -f Makefile
	ed - Makefile.bak < eddep
	rm eddep makedep

# DO NOT DELETE THIS LINE (or the line after this one)

format.o: ../format.c /usr/include/bsd/sys/param.h /usr/include/bsd/sys/types.h
format.o: /usr/include/bsd/sys/../../sys/types.h
format.o: /usr/include/bsd/sys/../../sys/param.h /usr/include/sys/fs/s5param.h
format.o: /usr/include/sys/inode.h ../../include/sys/fs.h
format.o: /usr/include/bsd/sys/dir.h /usr/include/sys/dirent.h
format.o: /usr/include/bsd/sys/file.h /usr/include/bsd/sys/types.h
format.o: /usr/include/bsd/sys/../../sys/file.h /usr/include/sys/fcntl.h
format.o: /usr/include/sys/errno.h /usr/include/bsd/netinet/in.h
format.o: ../../include/machine/dvh.h /usr/include/bsd/sys/ioctl.h
format.o: /usr/include/bsd/sys/ttychars.h /usr/include/termio.h
format.o: /usr/include/sys/termio.h /usr/include/bsd/net/soioctl.h
format.o: /usr/include/bsd/sys/types.h ../../include/machine/dkio.h
format.o: /usr/include/stdio.h /usr/include/ctype.h
